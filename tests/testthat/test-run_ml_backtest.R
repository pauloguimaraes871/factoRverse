#END-TO-END TESTS

#BEGIN OLS TESTS (TRAINING + TESTING)
####################
#Define your test
test_that("OLS - run_ml_backtest works with no rebalancing and a 1m target", {

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
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
        dates = as.Date(c("2001-03-15","2001-04-15",
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
                  format = "%Y-%m-%d"),
        Alpha = c(3,-20,-450,5,-2,1,1,7,4,2,
                  20,1,2,9,9,-20,-150,-20,5,-2,
                  2,-1,-50,-25,5,3,-1,2,-1,-20),
        Beta = c(4,7,5,3,13,10,5,2,4,1,
                 -12,-10,6,-3,-2,1,1,4,0,-2,5,2,
                 5,1,2,-9,3,1,2,1),
        Gamma = c(800,11,4,20,0,-523,9,-2,4,
                  -15,3,4,10,-3,2,6,20,12,-9,5,
                  2,3,3,-10,3,1,-500,6,4,405)),

      target_m_df = data.frame(
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
          3,3,0,1)),
      training_sample_size = 4,
      rebalancing_months = 9,
      target_fwd_name = "fwd_premium_1m",
      ml_algorithm = "ols",
      show_plots = FALSE)
  }))


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")
  #Pred list
  prediction_list <- list(c(`Stock A` = 4.86203815251015, `Stock B` = 5.00190010698946, `Stock C` = 4.96435541423554, `Stock D` = 4.94818709592051, `Stock E` = 4.86967646823366),
                          c(`Stock A` = 5.48519825741933, `Stock B` = 4.20386094336353, `Stock C` = 5.43567282083367, `Stock D` = 5.29919524341379, `Stock E` = 4.94189073217023),
                          c(`Stock A` = 8.62515342023342, `Stock B` = 4.37275430239549, `Stock C` = 5.06670954495026, `Stock D` = 5.08661517651311, `Stock E` = 2.45210627787541))
  names(prediction_list) <- c("2001-06-15","2001-07-15", "2001-08-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  error_list <- list(c(`Stock A` = -3.86203815251015, `Stock B` = -2.00190010698946, `Stock C` = 0.0356445857644596, `Stock D` = 2.05181290407949, `Stock E` = -3.86967646823366),
                     c(`Stock A` = -3.48519825741933, `Stock B` = 0.796139056636471, `Stock C` = -4.43567282083367, `Stock D` = -3.29919524341379, `Stock E` = -2.94189073217023),
                     c(`Stock A` = -7.62515342023342, `Stock B` = -5.37275430239549, `Stock C` = -14.0667095449503, `Stock D` = -7.08661517651311, `Stock E` = -2.45210627787541))
  names(error_list) <- c("2001-06-15","2001-07-15", "2001-08-15")
  results$outputs[[2]] <- error_list
  #Y-list
  y_list <- list(c(`Stock A` = 1, `Stock B` = 3, `Stock C` = 5, `Stock D` = 7, `Stock E` = 1), c(`Stock A` = 2, `Stock B` = 5, `Stock C` = 1, `Stock D` = 2, `Stock E` = 2), c(`Stock A` = 1, `Stock B` = -1, `Stock C` = -9, `Stock D` = -2, `Stock E` = 0))
  names(y_list) <- c("2001-06-15","2001-07-15", "2001-08-15")
  results$outputs[[3]] <- y_list
  #Eval metrics
  eval_metrics <- data.frame(#out_of_sample_rsquared_ols
    rss = c(0.551664171429985, -0.368290736807804, -2.92085874432165),
    #out-of-sample_crossproduct
    cp = c(16.8393003368667, 11.5815092007316, -10.3042434279481),
    #out-of-sample_rmse
    rmse = c(2.76074429922263, 3.22474954062161, 8.25971804308093),
    #out-of-sample_mape
    mae = c(2.36421444351545, 2.9916192220947, 7.32066774439354),
    #out-of-sample pseudo huber
    mphe = c(1.70142654, 2.20113245, 6.41253540),
    #out-of-sample quantile
    mpe = c(1.182107, 1.495810, 3.660334),
    #out-of-sample mape
    mape = c(1.739852, 1.891609, Inf),
    #out-of-sample hr
    hr = c(1.0, 1.0, 0.2),
    #mean bias
    mb = c(-1.529231, -2.673164, -7.320668)
  )

  rownames(eval_metrics) <- c("2001-06-15","2001-07-15", "2001-08-15")


  results$outputs[[4]] <- eval_metrics
  #final_model
  results$outputs[[5]] <- ml_backtest_results@final_model

  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL

  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-05
  )

})

#Define your test
test_that("OLS - run_ml_backtest works with rebalancing and a 1m target", {

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
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
          dates = as.Date(c("2001-03-15","2001-04-15",
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
                    "2001-10-15","2001-11-15"), format = "%Y-%m-%d"),
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
      ,
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
          dates = as.Date(c("2001-03-15","2001-04-15",
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
                    "2001-10-15","2001-11-15"), format = "%Y-%m-%d"),
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
        ),
      training_sample_size = 4,
      rebalancing_months = 9,
      show_plots = FALSE,
      ml_algorithm = "ols",
      target_fwd_name = "fwd_premium_1m")
  }))


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")
  #Pred list
  prediction_list <- list(c(`Stock A` = 4.86203815251015, `Stock B` = 5.00190010698946, `Stock C` = 4.96435541423554, `Stock D` = 4.94818709592051, `Stock E` = 4.86967646823366),
                          c(`Stock A` = 5.48519825741933, `Stock B` = 4.20386094336353, `Stock C` = 5.43567282083367, `Stock D` = 5.29919524341379, `Stock E` = 4.94189073217023),
                          c(`Stock A` = 8.62515342023342, `Stock B` = 4.37275430239549, `Stock C` = 5.06670954495026, `Stock D` = 5.08661517651311, `Stock E` = 2.45210627787541),
                          c(`Stock A` = 2.85051501368175, `Stock B` = 2.92383509749049, `Stock C` = 1.69537399669303, `Stock D` = 2.99425108348875, `Stock E` = 3.16999900967429),
                          c(`Stock A` = 3.36877633975403, `Stock B` = 2.88800431705638, `Stock C` = 1.98952539865867, `Stock D` = 2.81715855293, `Stock E` = 4.1880279492188),
                          c(`Stock A` = 2.9979157080522, `Stock B` = 3.03814949016211, `Stock C` = 2.75868492698232, `Stock D` = 2.91867156906171, `Stock E` = 2.86037385247988))
  names(prediction_list) <- c("2001-06-15","2001-07-15", "2001-08-15", "2001-09-15", "2001-10-15", "2001-11-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  error_list <- list(c(`Stock A` = -3.86203815251015, `Stock B` = -2.00190010698946, `Stock C` = 0.0356445857644596, `Stock D` = 2.05181290407949, `Stock E` = -3.86967646823366),
                     c(`Stock A` = -3.48519825741933, `Stock B` = 0.796139056636471, `Stock C` = -4.43567282083367, `Stock D` = -3.29919524341379, `Stock E` = -2.94189073217023),
                     c(`Stock A` = -7.62515342023342, `Stock B` = -5.37275430239549, `Stock C` = -14.0667095449503, `Stock D` = -7.08661517651311, `Stock E` = -1.45210627787541),
                     c(`Stock A` = 7.14948498631825, `Stock B` = 32.0761649025095, `Stock C` = 0.30462600330697, `Stock D` = -12.9942510834887, `Stock E` = 0.830000990325713),
                     c(`Stock A` = -0.368776339754032, `Stock B` = -154.888004317056, `Stock C` = 2.01047460134133, `Stock D` = -47.81715855293, `Stock E` = -9.1880279492188),
                     c(`Stock A` = -1.9979157080522, `Stock B` = -0.0381494901621053, `Stock C` = -22.7586849269823, `Stock D` = -5.91867156906171, `Stock E` = -2.86037385247988))
  names(error_list) <- c("2001-06-15","2001-07-15", "2001-08-15", "2001-09-15", "2001-10-15", "2001-11-15")
  results$outputs[[2]] <- error_list
  #Y-list
  y_list <- list(c(`Stock A` = 1, `Stock B` = 3, `Stock C` = 5, `Stock D` = 7, `Stock E` = 1),
                 c(`Stock A` = 2, `Stock B` = 5, `Stock C` = 1, `Stock D` = 2, `Stock E` = 2),
                 c(`Stock A` = 1, `Stock B` = -1, `Stock C` = -9, `Stock D` = -2, `Stock E` = 1),
                 c(`Stock A` = 10, `Stock B` = 35, `Stock C` = 2, `Stock D` = -10, `Stock E` = 4),
                 c(`Stock A` = 3, `Stock B` = -152, `Stock C` = 4, `Stock D` = -45, `Stock E` = -5),
                 c(`Stock A` = 1, `Stock B` = 3, `Stock C` = -20, `Stock D` = -3, `Stock E` = 0))
  names(y_list) <- c("2001-06-15","2001-07-15", "2001-08-15", "2001-09-15", "2001-10-15", "2001-11-15")
  results$outputs[[3]] <- y_list
  #Eval metrics
  eval_metrics <- data.frame(#out_of_sample_rsquared_ols
    rss = c(0.551664171429985, -0.368290736807804, -2.8319374795481, 0.135205709142454, -0.0471175342588488, -0.348838362491895),
    #out-of-sample_crossproduct
    cp = c(16.8393003368667, 11.5815092007316, -9.81382217237304, 23.3935223492361, -113.724900041323, -10.3634698136586),
    #out-of-sample_rmse
    rmse = c(2.76074429922263, 3.22474954062161, 8.21231390291716, 15.809033811648, 72.6159381886698, 10.6316816532861),
    #out-of-sample_mape
    mae = c(2.36421444351545, 2.9916192220947, 7.12066774439354, 10.6709055931898, 42.8544883520601, 6.71475910934765),
    #out-of-sample_pseudo_huber
    mphe = c(1.70142654, 2.20113245, 6.23552578, 9.93769000, 42.05448109, 6.00965385),
    #oos pinbal
    mpe = c(1.182107, 1.49581, 3.560334, 5.335453, 21.427244, 3.35738),
    #oos mape
    mape = c(1.73985, 1.89161, 3.91126, 0.65813, 0.90895, Inf),
    #oos hr
    hr = c(1, 1, 0.4, 0.8, 0.4, 0.4),
    #oos mb
    mb = c(-1.5292, -2.6732, -7.1207, 5.4732, -42.0503, -6.7148)
  )
  #Rename
  rownames(eval_metrics) <- c("2001-06-15","2001-07-15", "2001-08-15", "2001-09-15", "2001-10-15", "2001-11-15")


  results$outputs[[4]] <- eval_metrics
  #final_model
  results$outputs[[5]] <- ml_backtest_results@final_model

  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model")


  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL

  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-05
  )

})

#Define your test
test_that("OLS - run_ml_backtest works with rebalancing occuring at last month and a 1m target", {

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df =
        data.frame(
          stringsAsFactors = FALSE,
          id = c("Stock A-2001-03-15",
                 "Stock A-2001-04-15","Stock A-2001-05-15","Stock A-2001-06-15",
                 "Stock A-2001-07-15","Stock A-2001-08-15",
                 "Stock A-2001-09-15","Stock B-2001-03-15","Stock B-2001-04-15",
                 "Stock B-2001-05-15","Stock B-2001-06-15",
                 "Stock B-2001-07-15","Stock B-2001-08-15","Stock B-2001-09-15",
                 "Stock C-2001-03-15","Stock C-2001-04-15","Stock C-2001-05-15",
                 "Stock C-2001-06-15","Stock C-2001-07-15",
                 "Stock C-2001-08-15","Stock C-2001-09-15","Stock D-2001-03-15",
                 "Stock D-2001-04-15","Stock D-2001-05-15",
                 "Stock D-2001-06-15","Stock D-2001-07-15","Stock D-2001-08-15",
                 "Stock D-2001-09-15","Stock E-2001-03-15","Stock E-2001-04-15",
                 "Stock E-2001-05-15","Stock E-2001-06-15",
                 "Stock E-2001-07-15","Stock E-2001-08-15","Stock E-2001-09-15"),
          tickers = c("Stock A","Stock A","Stock A",
                      "Stock A","Stock A","Stock A","Stock A","Stock B",
                      "Stock B","Stock B","Stock B","Stock B","Stock B",
                      "Stock B","Stock C","Stock C","Stock C","Stock C",
                      "Stock C","Stock C","Stock C","Stock D","Stock D",
                      "Stock D","Stock D","Stock D","Stock D","Stock D",
                      "Stock E","Stock E","Stock E","Stock E","Stock E","Stock E",
                      "Stock E"),
          dates = as.Date(c("2001-03-15","2001-04-15",
                    "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                    "2001-09-15","2001-03-15","2001-04-15","2001-05-15",
                    "2001-06-15","2001-07-15","2001-08-15","2001-09-15",
                    "2001-03-15","2001-04-15","2001-05-15","2001-06-15",
                    "2001-07-15","2001-08-15","2001-09-15","2001-03-15",
                    "2001-04-15","2001-05-15","2001-06-15","2001-07-15",
                    "2001-08-15","2001-09-15","2001-03-15","2001-04-15",
                    "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                    "2001-09-15"), format = "%Y-%m-%d"),
          Alpha = c(3,-20,-450,5,-2,1,6,1,7,
                    4,2,20,1,1,2,9,9,-20,-150,-20,8,5,-2,2,
                    -1,-50,-25,1,5,3,-1,2,-1,-20,-1),
          Beta = c(4,7,5,3,13,10,4,5,2,4,
                   1,-12,-10,3,6,-3,-2,1,1,4,24,0,-2,5,2,5,
                   1,2,2,-9,3,1,2,1,-1),
          Gamma = c(800,11,4,20,0,-523,2,9,
                    -2,4,-15,3,4,4,10,-3,2,6,20,12,13,-9,5,2,
                    3,3,-10,0,3,1,-500,6,4,405,0)
        )
      ,
      target_m_df =
        data.frame(
          stringsAsFactors = FALSE,
          id = c("Stock A-2001-03-15",
                 "Stock A-2001-04-15","Stock A-2001-05-15","Stock A-2001-06-15",
                 "Stock A-2001-07-15","Stock A-2001-08-15",
                 "Stock A-2001-09-15","Stock B-2001-03-15","Stock B-2001-04-15",
                 "Stock B-2001-05-15","Stock B-2001-06-15",
                 "Stock B-2001-07-15","Stock B-2001-08-15","Stock B-2001-09-15",
                 "Stock C-2001-03-15","Stock C-2001-04-15","Stock C-2001-05-15",
                 "Stock C-2001-06-15","Stock C-2001-07-15",
                 "Stock C-2001-08-15","Stock C-2001-09-15","Stock D-2001-03-15",
                 "Stock D-2001-04-15","Stock D-2001-05-15",
                 "Stock D-2001-06-15","Stock D-2001-07-15","Stock D-2001-08-15",
                 "Stock D-2001-09-15","Stock E-2001-03-15","Stock E-2001-04-15",
                 "Stock E-2001-05-15","Stock E-2001-06-15",
                 "Stock E-2001-07-15","Stock E-2001-08-15","Stock E-2001-09-15"),
          tickers = c("Stock A","Stock A","Stock A",
                      "Stock A","Stock A","Stock A","Stock A","Stock B",
                      "Stock B","Stock B","Stock B","Stock B","Stock B",
                      "Stock B","Stock C","Stock C","Stock C","Stock C",
                      "Stock C","Stock C","Stock C","Stock D","Stock D",
                      "Stock D","Stock D","Stock D","Stock D","Stock D",
                      "Stock E","Stock E","Stock E","Stock E","Stock E","Stock E",
                      "Stock E"),
          dates = as.Date(c("2001-03-15","2001-04-15",
                    "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                    "2001-09-15","2001-03-15","2001-04-15","2001-05-15",
                    "2001-06-15","2001-07-15","2001-08-15","2001-09-15",
                    "2001-03-15","2001-04-15","2001-05-15","2001-06-15",
                    "2001-07-15","2001-08-15","2001-09-15","2001-03-15",
                    "2001-04-15","2001-05-15","2001-06-15","2001-07-15",
                    "2001-08-15","2001-09-15","2001-03-15","2001-04-15",
                    "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                    "2001-09-15"), format = "%Y-%m-%d"),
          fwd_premium_1m = c(0,6,7,1,2,1,10,1,8,2,
                             3,5,-1,35,2,3,7,5,1,-9,2,8,8,8,7,2,-2,
                             -10,5,1,8,1,2,1,4),
          fwd_premium_3m = c(4,4,2,0,6,5,-5,5,3,7,
                             3,8,2,5,0,5,2,8,3,5,3,1,3,8,3,1,1,11,9,
                             9,1,2,3,-9,-4),
          fwd_sharpe_1m = c(7,7,3,1,1,3,1,4,2,8,5,
                            4,1,1,2,6,4,6,5,1,1,4,9,0,10,1,4,12,7,
                            1,3,3,0,1,3)
        ),
      training_sample_size = 4,
      rebalancing_months = 9,
      ml_algorithm = "ols",
      show_plots = FALSE,
      target_fwd_name = "fwd_premium_1m")
  }))


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")
  #Pred list
  prediction_list <- list(c(`Stock A` = 4.86203815251015, `Stock B` = 5.00190010698946, `Stock C` = 4.96435541423554, `Stock D` = 4.94818709592051, `Stock E` = 4.86967646823366),
                          c(`Stock A` = 5.48519825741933, `Stock B` = 4.20386094336353, `Stock C` = 5.43567282083367, `Stock D` = 5.29919524341379, `Stock E` = 4.94189073217023),
                          c(`Stock A` = 8.62515342023342, `Stock B` = 4.37275430239549, `Stock C` = 5.06670954495026, `Stock D` = 5.08661517651311, `Stock E` = 2.45210627787541),
                          c(`Stock A` = 2.85051501368175, `Stock B` = 2.92383509749049, `Stock C` = 1.69537399669303, `Stock D` = 2.99425108348875, `Stock E` = 3.16999900967429))
  names(prediction_list) <- c("2001-06-15","2001-07-15", "2001-08-15", "2001-09-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  error_list <-  list(c(`Stock A` = -3.86203815251015, `Stock B` = -2.00190010698946, `Stock C` = 0.0356445857644596, `Stock D` = 2.05181290407949, `Stock E` = -3.86967646823366),
                      c(`Stock A` = -3.48519825741933, `Stock B` = 0.796139056636471, `Stock C` = -4.43567282083367, `Stock D` = -3.29919524341379, `Stock E` = -2.94189073217023),
                      c(`Stock A` = -7.62515342023342, `Stock B` = -5.37275430239549, `Stock C` = -14.0667095449503, `Stock D` = -7.08661517651311, `Stock E` = -1.45210627787541),
                      c(`Stock A` = 7.14948498631825, `Stock B` = 32.0761649025095, `Stock C` = 0.30462600330697, `Stock D` = -12.9942510834887, `Stock E` = 0.830000990325713))
  names(error_list) <- c("2001-06-15","2001-07-15", "2001-08-15", "2001-09-15")
  results$outputs[[2]] <- error_list
  #Y-list
  y_list <- list(c(`Stock A` = 1, `Stock B` = 3, `Stock C` = 5, `Stock D` = 7, `Stock E` = 1),
                 c(`Stock A` = 2, `Stock B` = 5, `Stock C` = 1, `Stock D` = 2, `Stock E` = 2),
                 c(`Stock A` = 1, `Stock B` = -1, `Stock C` = -9, `Stock D` = -2, `Stock E` = 1),
                 c(`Stock A` = 10, `Stock B` = 35, `Stock C` = 2, `Stock D` = -10, `Stock E` = 4))
  names(y_list) <- c("2001-06-15","2001-07-15", "2001-08-15", "2001-09-15")
  results$outputs[[3]] <- y_list
  #Eval metrics
  eval_metrics <- data.frame(#out_of_sample_rsquared_ols
    rss = c(0.551664171429985, -0.368290736807804, -2.8319374795481, 0.135205709142454),
    #out-of-sample_crossproduct
    cp = c(16.8393003368667, 11.5815092007316, -9.81382217237304, 23.3935223492361),
    #out-of-sample_rmse
    rmse = c(2.76074429922263, 3.22474954062161, 8.21231390291716, 15.809033811648),
    #out-of-sample_mape
    mae = c(2.36421444351545, 2.9916192220947, 7.12066774439354, 10.6709055931898),
    #oos huber_loss
    mphe = c(1.70142654, 2.20113245, 6.23552578, 9.937690000),
    #pinball
    mpe = c(1.182107, 1.49581, 3.560334, 5.335453),
    #mape
    mape = c(1.73985, 1.89161, 3.91126, 0.65813),
    #hr
    hr = c(1, 1, 0.4, 0.8),
    #mb
    mb = c(-1.5292, -2.6732, -7.1207, 5.4732)
  )
  #Rename
  rownames(eval_metrics) <-  c("2001-06-15","2001-07-15", "2001-08-15", "2001-09-15")


  results$outputs[[4]] <- eval_metrics
  #final_model
  results$outputs[[5]] <- ml_backtest_results@final_model

  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL

  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-05
  )

})

#Define your test
test_that("OLS - run_ml_backtest works with rebalancing and a 3m target", {

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
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
          dates = as.Date(c("2001-03-15","2001-04-15",
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
                    "2001-10-15","2001-11-15"), format = "%Y-%m-%d"),
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
      ,
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
          dates = as.Date(c("2001-03-15","2001-04-15",
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
                    "2001-10-15","2001-11-15"), format = "%Y-%m-%d"),
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
        ),
      training_sample_size = 7,
      rebalancing_months = 9,
      ml_algorithm = "ols",
      show_plots = FALSE,
      target_fwd_name = "fwd_premium_3m")
  }))


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")
  #Pred list
  prediction_list <- list(c(`Stock A` = 3.55789756999617, `Stock B` = 3.74027366687273, `Stock C` = -0.229832241432107, `Stock D` = 3.92320884322248, `Stock E` = 4.49042135903706),
                          c(`Stock A` = 5.26391645008663, `Stock B` = 3.54031311371724, `Stock C` = 0.712631497540624, `Stock D` = 3.3564962811891, `Stock E` = 8.12782601207548),
                          c(`Stock A` = 4.14189895984849, `Stock B` = 4.12016967369082, `Stock C` = 4.69907503590588, `Stock D` = 3.74271431008669, `Stock E` = 3.99069214528025))
  names(prediction_list) <- c("2001-09-15", "2001-10-15", "2001-11-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  error_list <- list(c(`Stock A` = -8.55789756999616, `Stock B` = 1.25972633312727, `Stock C` = 3.22983224143211, `Stock D` = 7.07679115677752, `Stock E` = -8.49042135903706),
                     c(`Stock A` = -6.26391645008663, `Stock B` = -2.54031311371724, `Stock C` = 39.2873685024594, `Stock D` = 0.643503718810904, `Stock E` = -4.12782601207548),
                     c(`Stock A` = -0.141898959848494, `Stock B` = -2.12016967369082, `Stock C` = -2.69907503590588, `Stock D` = -1.74271431008669, `Stock E` = -0.990692145280251))
  names(error_list) <- c("2001-09-15", "2001-10-15", "2001-11-15")
  results$outputs[[2]] <- error_list
  #Y-list
  y_list <- list(c(`Stock A` = -5, `Stock B` = 5, `Stock C` = 3, `Stock D` = 11, `Stock E` = -4),
                 c(`Stock A` = -1, `Stock B` = 1, `Stock C` = 40, `Stock D` = 4, `Stock E` = 4),
                 c(`Stock A` = 4, `Stock B` = 2, `Stock C` = 2, `Stock D` = 2, `Stock E` = 3))
  names(y_list) <- c("2001-09-15", "2001-10-15", "2001-11-15")
  results$outputs[[3]] <- y_list
  #Eval metrics
  eval_metrics <- data.frame(#out_of_sample_rsquared_ols
    rss = c(-0.0582885994456739, 0.016744058458068, 0.572465272897418),
    #out-of-sample_crossproduct
    cp = c(5.08319911987711, 14.5437891476628, 10.7327180629203),
    #out-of-sample_rmse
    rmse = c(6.44087828624873, 17.9256252804722, 1.77869530290017),
    #out-of-sample_mape
    mae = c(5.72293373207402, 10.5725855594299, 1.53891002496243),
    #out-of-sample_pseudo
    mphe = c(4.86036267, 9.76195385, 0.92988850),
    #out-of-sample_pinball
    mpe = c(2.861467, 5.286293, 0.769455),
    #out-of-sample mape
    mape = c(1.16122, 2.19585, 0.72934),
    #oos hr
    hr = c(0.4, 0.8, 1),
    #oos mb
    mb = c(-1.0964, 5.3998, -1.5389)
  )
  #Rename
  rownames(eval_metrics) <-  c("2001-09-15", "2001-10-15", "2001-11-15")


  results$outputs[[4]] <- eval_metrics
  #final_model
  results$outputs[[5]] <- ml_backtest_results@final_model

  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-05
  )

})

#Define your test
test_that("OLS - run_ml_backtest works with two rebalancing dates, unbalanced panel and a 3m target", {

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df =
        data.frame(
          stringsAsFactors = FALSE,
          id = c("Stock A-2001-03-15",
                 "Stock A-2001-04-15","Stock A-2001-05-15","Stock A-2001-06-15",
                 "Stock A-2001-07-15","Stock A-2001-08-15",
                 "Stock A-2001-09-15","Stock A-2001-10-15","Stock A-2001-11-15",
                 "Stock A-2001-12-15","Stock A-2002-01-15",
                 "Stock A-2002-02-15","Stock A-2002-03-15","Stock A-2002-04-15",
                 "Stock A-2002-05-15","Stock A-2002-06-15","Stock A-2002-07-15",
                 "Stock A-2002-08-15","Stock A-2002-09-15",
                 "Stock A-2002-10-15","Stock B-2001-03-15","Stock B-2001-04-15",
                 "Stock B-2001-05-15","Stock B-2001-06-15",
                 "Stock B-2001-07-15","Stock B-2001-08-15","Stock B-2001-09-15",
                 "Stock B-2001-10-15","Stock B-2001-11-15","Stock B-2001-12-15",
                 "Stock B-2002-01-15","Stock B-2002-02-15",
                 "Stock B-2002-03-15","Stock B-2002-04-15","Stock B-2002-06-15",
                 "Stock B-2002-07-15","Stock B-2002-08-15",
                 "Stock B-2002-09-15","Stock B-2002-10-15","Stock C-2001-07-15",
                 "Stock C-2001-08-15","Stock C-2001-09-15",
                 "Stock C-2001-10-15","Stock C-2001-11-15","Stock C-2001-12-15",
                 "Stock C-2002-01-15","Stock C-2002-02-15","Stock C-2002-03-15",
                 "Stock C-2002-04-15","Stock C-2002-05-15",
                 "Stock C-2002-06-15","Stock C-2002-09-15","Stock C-2002-10-15",
                 "Stock D-2001-03-15","Stock D-2001-04-15",
                 "Stock D-2001-05-15","Stock D-2001-06-15","Stock D-2001-07-15",
                 "Stock D-2001-08-15","Stock D-2001-09-15","Stock D-2001-10-15",
                 "Stock D-2001-11-15","Stock D-2001-12-15",
                 "Stock D-2002-01-15","Stock D-2002-02-15","Stock D-2002-03-15",
                 "Stock D-2002-04-15","Stock D-2002-07-15",
                 "Stock D-2002-08-15","Stock D-2002-09-15","Stock D-2002-10-15",
                 "Stock E-2001-03-15","Stock E-2001-04-15","Stock E-2001-05-15",
                 "Stock E-2001-06-15","Stock E-2001-07-15",
                 "Stock E-2001-08-15","Stock E-2001-09-15","Stock E-2001-10-15",
                 "Stock E-2001-11-15","Stock E-2001-12-15",
                 "Stock E-2002-01-15","Stock E-2002-02-15","Stock E-2002-04-15",
                 "Stock E-2002-05-15","Stock E-2002-06-15","Stock E-2002-07-15",
                 "Stock E-2002-08-15","Stock E-2002-09-15",
                 "Stock E-2002-10-15"),
          tickers = c("Stock A","Stock A","Stock A",
                      "Stock A","Stock A","Stock A","Stock A","Stock A",
                      "Stock A","Stock A","Stock A","Stock A","Stock A",
                      "Stock A","Stock A","Stock A","Stock A","Stock A",
                      "Stock A","Stock A","Stock B","Stock B","Stock B",
                      "Stock B","Stock B","Stock B","Stock B","Stock B",
                      "Stock B","Stock B","Stock B","Stock B","Stock B","Stock B",
                      "Stock B","Stock B","Stock B","Stock B","Stock B",
                      "Stock C","Stock C","Stock C","Stock C","Stock C",
                      "Stock C","Stock C","Stock C","Stock C","Stock C",
                      "Stock C","Stock C","Stock C","Stock C","Stock D",
                      "Stock D","Stock D","Stock D","Stock D","Stock D","Stock D",
                      "Stock D","Stock D","Stock D","Stock D","Stock D",
                      "Stock D","Stock D","Stock D","Stock D","Stock D",
                      "Stock D","Stock E","Stock E","Stock E","Stock E",
                      "Stock E","Stock E","Stock E","Stock E","Stock E",
                      "Stock E","Stock E","Stock E","Stock E","Stock E",
                      "Stock E","Stock E","Stock E","Stock E","Stock E"),
          dates = as.Date(c("2001-03-15","2001-04-15",
                    "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                    "2001-09-15","2001-10-15","2001-11-15","2001-12-15",
                    "2002-01-15","2002-02-15","2002-03-15","2002-04-15",
                    "2002-05-15","2002-06-15","2002-07-15","2002-08-15",
                    "2002-09-15","2002-10-15","2001-03-15","2001-04-15",
                    "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                    "2001-09-15","2001-10-15","2001-11-15","2001-12-15",
                    "2002-01-15","2002-02-15","2002-03-15","2002-04-15",
                    "2002-06-15","2002-07-15","2002-08-15","2002-09-15","2002-10-15",
                    "2001-07-15","2001-08-15","2001-09-15","2001-10-15",
                    "2001-11-15","2001-12-15","2002-01-15","2002-02-15",
                    "2002-03-15","2002-04-15","2002-05-15","2002-06-15",
                    "2002-09-15","2002-10-15","2001-03-15","2001-04-15",
                    "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                    "2001-09-15","2001-10-15","2001-11-15","2001-12-15",
                    "2002-01-15","2002-02-15","2002-03-15","2002-04-15",
                    "2002-07-15","2002-08-15","2002-09-15","2002-10-15",
                    "2001-03-15","2001-04-15","2001-05-15","2001-06-15",
                    "2001-07-15","2001-08-15","2001-09-15","2001-10-15",
                    "2001-11-15","2001-12-15","2002-01-15","2002-02-15",
                    "2002-04-15","2002-05-15","2002-06-15","2002-07-15",
                    "2002-08-15","2002-09-15","2002-10-15"), format = "%Y-%m-%d"),
          Alpha = c(3,-20,-450,5,-2,1,6,1,
                    -9,-4,-323,28,-353,-286,-326,-267,-53,-345,-69,
                    4,1,7,4,2,20,1,1,-2,-2,4.27400664537767,
                    9.90111375510975,1.21804327509852,8.32715720168221,
                    3.70432267828307,7.2317432168442,3.33191727050546,
                    2.41477701144831,8.0849555836344,0.4910346478647,-150,-20,8,17,
                    1,41,287,13,258,299,148,269,235,109,5,-2,2,
                    -1,-50,-25,1,4,2,5,-2,2,-1,-50,4,2,4,1,5,3,
                    -1,2,-1,-20,-1,4,4,-8.28542807338087,
                    -8.41223063600313,-8.28203098022789,-3.41237860621712,
                    -3.42824802875889,-3.47800362458764,-3.58449804733582,
                    -3.63600926135083,-3.5757397024896,-3.57097273285465),
          Beta = c(4,7,5,3,13,10,4,-5,1,
                   -400,-310,-191,-72,-155,-261,-245,-186,-280,-89,
                   3,5,2,4,1,-12,-10,3,4,1,8.63501867386761,
                   9.54615007212824,7.07054870478622,5.45538081701934,
                   7.44293739786216,4.75960433883533,1.86372973373474,
                   2.5534474505921,5.12860795961973,6.07623856836013,1,4,24,19,
                   -1,88,69,66,296,-13,246,146,109,17,0,-2,5,2,
                   5,1,2,5,3,0,-2,5,2,5,5,3,5,-5,2,-9,3,1,
                   2,1,-1,-20,2,-9.12835429041764,40,4,
                   -9.05020448837052,-1,5,6,7,52,0),
          Gamma = c(800,11,4,20,0,-523,2,3,
                    27,-42,-110,-250,-215,-460,-163,-470,-106,-430,
                    11,1,9,-2,4,-15,3,4,4,3,7,1.6487109735006,
                    7.10583100801392,2.93834894706739,9.69526295862682,
                    6.87855210290332,9.06761681428307,7.13200228721206,
                    9.31419677379033,5.30983154273534,8.91635543435034,20,12,13,
                    -4,105,86,202,122,5,226,212,190,208,104,-9,5,
                    2,3,3,-10,0,-1,4,-9,5,2,3,3,-1,4,3,0,3,
                    1,-500,6,4,405,0,1,31,-1.58703386660033,20,1,
                    45,4,-412,40,56,6,41)
        )
      ,
      target_m_df =
        data.frame(
          stringsAsFactors = FALSE,
          id = c("Stock A-2001-03-15",
                 "Stock A-2001-04-15","Stock A-2001-05-15","Stock A-2001-06-15",
                 "Stock A-2001-07-15","Stock A-2001-08-15",
                 "Stock A-2001-09-15","Stock A-2001-10-15","Stock A-2001-11-15",
                 "Stock A-2001-12-15","Stock A-2002-01-15",
                 "Stock A-2002-02-15","Stock A-2002-03-15","Stock A-2002-04-15",
                 "Stock A-2002-05-15","Stock A-2002-06-15","Stock A-2002-07-15",
                 "Stock A-2002-08-15","Stock A-2002-09-15",
                 "Stock A-2002-10-15","Stock B-2001-03-15","Stock B-2001-04-15",
                 "Stock B-2001-05-15","Stock B-2001-06-15",
                 "Stock B-2001-07-15","Stock B-2001-08-15","Stock B-2001-09-15",
                 "Stock B-2001-10-15","Stock B-2001-11-15","Stock B-2001-12-15",
                 "Stock B-2002-01-15","Stock B-2002-02-15",
                 "Stock B-2002-03-15","Stock B-2002-04-15","Stock B-2002-06-15",
                 "Stock B-2002-07-15","Stock B-2002-08-15",
                 "Stock B-2002-09-15","Stock B-2002-10-15","Stock C-2001-07-15",
                 "Stock C-2001-08-15","Stock C-2001-09-15",
                 "Stock C-2001-10-15","Stock C-2001-11-15","Stock C-2001-12-15",
                 "Stock C-2002-01-15","Stock C-2002-02-15","Stock C-2002-03-15",
                 "Stock C-2002-04-15","Stock C-2002-05-15",
                 "Stock C-2002-06-15","Stock C-2002-09-15","Stock C-2002-10-15",
                 "Stock D-2001-03-15","Stock D-2001-04-15",
                 "Stock D-2001-05-15","Stock D-2001-06-15","Stock D-2001-07-15",
                 "Stock D-2001-08-15","Stock D-2001-09-15","Stock D-2001-10-15",
                 "Stock D-2001-11-15","Stock D-2001-12-15",
                 "Stock D-2002-01-15","Stock D-2002-02-15","Stock D-2002-03-15",
                 "Stock D-2002-04-15","Stock D-2002-07-15",
                 "Stock D-2002-08-15","Stock D-2002-09-15","Stock D-2002-10-15",
                 "Stock E-2001-03-15","Stock E-2001-04-15","Stock E-2001-05-15",
                 "Stock E-2001-06-15","Stock E-2001-07-15",
                 "Stock E-2001-08-15","Stock E-2001-09-15","Stock E-2001-10-15",
                 "Stock E-2001-11-15","Stock E-2001-12-15",
                 "Stock E-2002-01-15","Stock E-2002-02-15","Stock E-2002-04-15",
                 "Stock E-2002-05-15","Stock E-2002-06-15","Stock E-2002-07-15",
                 "Stock E-2002-08-15","Stock E-2002-09-15",
                 "Stock E-2002-10-15"),
          tickers = c("Stock A","Stock A","Stock A",
                      "Stock A","Stock A","Stock A","Stock A","Stock A",
                      "Stock A","Stock A","Stock A","Stock A","Stock A",
                      "Stock A","Stock A","Stock A","Stock A","Stock A",
                      "Stock A","Stock A","Stock B","Stock B","Stock B",
                      "Stock B","Stock B","Stock B","Stock B","Stock B",
                      "Stock B","Stock B","Stock B","Stock B","Stock B","Stock B",
                      "Stock B","Stock B","Stock B","Stock B","Stock B",
                      "Stock C","Stock C","Stock C","Stock C","Stock C",
                      "Stock C","Stock C","Stock C","Stock C","Stock C",
                      "Stock C","Stock C","Stock C","Stock C","Stock D",
                      "Stock D","Stock D","Stock D","Stock D","Stock D","Stock D",
                      "Stock D","Stock D","Stock D","Stock D","Stock D",
                      "Stock D","Stock D","Stock D","Stock D","Stock D",
                      "Stock D","Stock E","Stock E","Stock E","Stock E",
                      "Stock E","Stock E","Stock E","Stock E","Stock E",
                      "Stock E","Stock E","Stock E","Stock E","Stock E",
                      "Stock E","Stock E","Stock E","Stock E","Stock E"),
          dates = as.Date(c("2001-03-15","2001-04-15",
                    "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                    "2001-09-15","2001-10-15","2001-11-15","2001-12-15",
                    "2002-01-15","2002-02-15","2002-03-15","2002-04-15",
                    "2002-05-15","2002-06-15","2002-07-15","2002-08-15",
                    "2002-09-15","2002-10-15","2001-03-15","2001-04-15",
                    "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                    "2001-09-15","2001-10-15","2001-11-15","2001-12-15",
                    "2002-01-15","2002-02-15","2002-03-15","2002-04-15",
                    "2002-06-15","2002-07-15","2002-08-15","2002-09-15","2002-10-15",
                    "2001-07-15","2001-08-15","2001-09-15","2001-10-15",
                    "2001-11-15","2001-12-15","2002-01-15","2002-02-15",
                    "2002-03-15","2002-04-15","2002-05-15","2002-06-15",
                    "2002-09-15","2002-10-15","2001-03-15","2001-04-15",
                    "2001-05-15","2001-06-15","2001-07-15","2001-08-15",
                    "2001-09-15","2001-10-15","2001-11-15","2001-12-15",
                    "2002-01-15","2002-02-15","2002-03-15","2002-04-15",
                    "2002-07-15","2002-08-15","2002-09-15","2002-10-15",
                    "2001-03-15","2001-04-15","2001-05-15","2001-06-15",
                    "2001-07-15","2001-08-15","2001-09-15","2001-10-15",
                    "2001-11-15","2001-12-15","2002-01-15","2002-02-15",
                    "2002-04-15","2002-05-15","2002-06-15","2002-07-15",
                    "2002-08-15","2002-09-15","2002-10-15"), format = "%Y-%m-%d"),
          fwd_premium_1m = c(0,6,7,1,2,1,10,3,1,1,
                             30,10,3,1,1,8,2,3,5,2,1,8,2,3,5,-1,35,
                             -152,3,3,0,0,-9,-18,-6,1,1,6,-7,1,-9,2,4,
                             -20,4,5,-8,0,4,5,1,30,2,8,8,8,7,2,-2,-10,
                             -45,-3,5,1,8,1,2,-5,0,-10,-45,5,1,8,1,2,1,
                             4,-5,0,5,1,8,2,1,-9,401,1,1,4),
          fwd_premium_3m = c(4,4,2,0,6,5,-5,-1,4,5,
                             8,-5,-1,4,5,3,7,3,8,7,5,3,7,3,8,2,5,1,
                             2,2,-17,-3,6,-5,-16,1,-2,-17,-8,3,5,3,40,2,
                             -9,8,4,50,150,4,50,30,100,1,3,8,3,1,1,11,
                             4,2,9,9,1,2,3,4,3,11,4,9,9,1,2,3,-9,-4,
                             4,3,9,9,1,3,3,5,51152582,40,2,-9),
          fwd_sharpe_1m = c(7,7,3,1,1,3,1,0,10,3,
                            9,1,0,10,3,2,8,5,4,8,4,2,8,5,4,1,1,4,-5,
                            -8,-1,-5,-15,4,-9,-9,9,-19,-8,5,1,1,5,3,
                            0,1,2,11,1,-909,11,5,5,4,9,0,10,1,4,12,1,
                            92,7,1,3,3,0,1,9,12,1,7,1,3,3,0,1,3,1,
                            9,7,1,3,0,5,1,1,5,3,40)
        ),
      training_sample_size = 7,
      rebalancing_months = 9,
      ml_algorithm = "ols",
      show_plots = FALSE,
      quantile_tau = 0.25,
      target_fwd_name = "fwd_premium_3m")
  }))

  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")
  #Pred list
  prediction_list <- list(c(`Stock A` = 3.78862791736474, `Stock B` = 3.92623410843985, `Stock C` = 0.780971123439226, `Stock D` = 4.07060643723063, `Stock E` = 4.51873468717562),
                          c(`Stock A` = 5.13842620526882, `Stock B` = 3.76192997405441, `Stock C` = 1.54048103585349, `Stock D` = 3.62420053396751, `Stock E` = 7.42175951448791),
                          c(`Stock A` = 4.23651280876919, `Stock B` = 4.22457649152493, `Stock C` = 4.71965811975846, `Stock D` = 3.92980205081535, `Stock E` = 4.13852373085986),
                          c(`Stock A` = 64.9806457554812, `Stock B` = 3.07843541306775, `Stock C` = -8.67885983271476, `Stock D` = 4.37177726862054, `Stock E` = 5.72332773760223),
                          c(`Stock A` = 50.0590475896515, `Stock B` = 2.97031576167022, `Stock C` = -4.70371738813978, `Stock D` = 4.67614943483363, `Stock E` = -1.69274298491313),
                          c(`Stock A` = 32.9942012900676, `Stock B` = 3.30732775106189, `Stock C` = -5.37371711982056, `Stock D` = 3.62260143629743, `Stock E` = 3.7358248581286),
                          c(`Stock A` = 13.6406004180073, `Stock B` = 3.59027252823242, `Stock C` = -39.6190895940044, `Stock D` = 4.06900733956055),
                          c(`Stock A` = 26.0231226721466, `Stock B` = 3.26695893597037, `Stock C` = 7.82727856537011, `Stock D` = 3.43891402846529, `Stock E` = 5.81483572570896),
                          c(`Stock A` = 42.5145463240613, `Stock C` = -32.0417893684599, `Stock E` = 4.51745322091014),
                          c(`Stock A` = 39.7303816612239, `Stock B` = 3.69079313199683, `Stock C` = -16.4752002897524, `Stock E` = 2.83897961786658),
                          c(`Stock A` = 32.2121901797095, `Stock B` = 4.11276903133652, `Stock D` = 3.62420053396751, `Stock E` = 3.52105419386133),
                          c(`Stock A` = 44.8373208884671, `Stock B` = 4.00885626330588, `Stock D` = 3.92980205081535, `Stock E` = 3.3986452243509),
                          c(`Stock A` = 4.36746637438663, `Stock B` = 7.6691349095432, `Stock C` = 23.5459028103692, `Stock D` = 7.32748804820718, `Stock E` = 5.12955231191563),
                          c(`Stock A` = 7.3908226732168, `Stock B` = 7.00793112635805, `Stock C` = 15.8902766514117, `Stock D` = 7.40499401483414, `Stock E` = 6.92563153963565))
  names(prediction_list) <- c("2001-09-15", "2001-10-15", "2001-11-15", "2001-12-15",
                              "2002-01-15", "2002-02-15", "2002-03-15", "2002-04-15",
                              "2002-05-15", "2002-06-15", "2002-07-15", "2002-08-15",
                              "2002-09-15", "2002-10-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  error_list <- list(c(`Stock A` = -8.78862791736474, `Stock B` = 1.07376589156015, `Stock C` = 2.21902887656077, `Stock D` = 6.92939356276937, `Stock E` = -8.51873468717562),
                     c(`Stock A` = -6.13842620526882, `Stock B` = -2.76192997405441, `Stock C` = 38.4595189641465, `Stock D` = 0.375799466032491, `Stock E` = -3.42175951448791),
                     c(`Stock A` = -0.236512808769191, `Stock B` = -2.22457649152493, `Stock C` = -2.71965811975846, `Stock D` = -1.92980205081535, `Stock E` = -1.13852373085986),
                     c(`Stock A` = -59.9806457554812, `Stock B` = -1.07843541306775, `Stock C` = -0.321140167285245, `Stock D` = 4.62822273137946, `Stock E` = 3.27667226239777),
                     c(`Stock A` = -42.0590475896515, `Stock B` = -19.9703157616702, `Stock C` = 12.7037173881398, `Stock D` = 4.32385056516637, `Stock E` = 10.6927429849131),
                     c(`Stock A` = -37.9942012900676, `Stock B` = -6.30732775106189, `Stock C` = 9.37371711982056, `Stock D` = -2.62260143629743, `Stock E` = -2.7358248581286),
                     c(`Stock A` = -14.6406004180073, `Stock B` = 2.40972747176758, `Stock C` = 89.6190895940044, `Stock D` = -2.06900733956055),
                     c(`Stock A` = -22.0231226721466, `Stock B` = -8.26695893597037, `Stock C` = 142.17272143463, `Stock D` = -0.438914028465287, `Stock E` = -2.81483572570896),
                     c(`Stock A` = -37.5145463240613, `Stock C` = 36.0417893684599, `Stock E` = -1.51745322091014),
                     c(`Stock A` = -36.7303816612239, `Stock B` = -19.6907931319968, `Stock C` = 66.4752002897524, `Stock E` = 2.16102038213342),
                     c(`Stock A` = -25.2121901797095, `Stock B` = -3.11276903133652, `Stock D` = 0.375799466032491, `Stock E` = 51152578.4789458),
                     c(`Stock A` = -41.8373208884671, `Stock B` = -6.00885626330588, `Stock D` = -0.929802050815351, `Stock E` = 36.6013547756491),
                     c(`Stock A` = 3.63253362561337, `Stock B` = -24.6691349095432, `Stock C` = 6.45409718963078, `Stock D` = 3.67251195179282, `Stock E` = -3.12955231191563),
                     c(`Stock A` = -0.390822673216798, `Stock B` = -15.0079311263581, `Stock C` = 84.1097233485882, `Stock D` = -3.40499401483414, `Stock E` = -15.9256315396356))
  names(error_list) <- c("2001-09-15", "2001-10-15", "2001-11-15", "2001-12-15",
                         "2002-01-15", "2002-02-15", "2002-03-15", "2002-04-15",
                         "2002-05-15", "2002-06-15", "2002-07-15", "2002-08-15",
                         "2002-09-15", "2002-10-15")
  results$outputs[[2]] <- error_list
  #Y-list
  y_list <- list(c(`Stock A` = -5, `Stock B` = 5, `Stock C` = 3, `Stock D` = 11, `Stock E` = -4),
                 c(`Stock A` = -1, `Stock B` = 1, `Stock C` = 40, `Stock D` = 4, `Stock E` = 4),
                 c(`Stock A` = 4, `Stock B` = 2, `Stock C` = 2, `Stock D` = 2, `Stock E` = 3),
                 c(`Stock A` = 5, `Stock B` = 2, `Stock C` = -9, `Stock D` = 9, `Stock E` = 9),
                 c(`Stock A` = 8, `Stock B` = -17, `Stock C` = 8, `Stock D` = 9, `Stock E` = 9),
                 c(`Stock A` = -5, `Stock B` = -3, `Stock C` = 4, `Stock D` = 1, `Stock E` = 1),
                 c(`Stock A` = -1, `Stock B` = 6, `Stock C` = 50, `Stock D` = 2),
                 c(`Stock A` = 4, `Stock B` = -5, `Stock C` = 150, `Stock D` = 3, `Stock E` = 3),
                 c(`Stock A` = 5, `Stock C` = 4, `Stock E` = 3), c(`Stock A` = 3, `Stock B` = -16, `Stock C` = 50, `Stock E` = 5),
                 c(`Stock A` = 7, `Stock B` = 1, `Stock D` = 4, `Stock E` = 51152582), c(`Stock A` = 3, `Stock B` = -2, `Stock D` = 3, `Stock E` = 40),
                 c(`Stock A` = 8, `Stock B` = -17, `Stock C` = 30, `Stock D` = 11, `Stock E` = 2),
                 c(`Stock A` = 7, `Stock B` = -8, `Stock C` = 100, `Stock D` = 4, `Stock E` = -9))
  names(y_list) <-  c("2001-09-15", "2001-10-15", "2001-11-15", "2001-12-15",
                      "2002-01-15", "2002-02-15", "2002-03-15", "2002-04-15",
                      "2002-05-15", "2002-06-15", "2002-07-15", "2002-08-15",
                      "2002-09-15", "2002-10-15")
  results$outputs[[3]] <- y_list
  #Eval metrics
  eval_metrics <- data.frame(#out_of_sample_rsquared_ols
    rss = c(-0.0403182593520917, 0.0597963305836505, 0.529146178248986, -12.3496361983755, -3.25249027201797, -29.4917512234603,
                 -2.24912080996351, 0.0791010590851802, -53.1730886211644, -1.20805395476737, 1.37668452748763e-07, -0.927861575822606,
                 0.501670297555347, 0.259054258079456),
    #out-of-sample_crossproduct
    cp = c(5.94653527730555, 20.8853170793494, 11.0219391503708, 100.005156630796, 67.8395863425969, -37.805886377676,
                     -491.228857567428, 257.922146015355, 32.6526446030658, -187.356665381641, 45027814.3682095, 68.5573663163178,
                     140.360598999609, 310.397853409089),
    #out-of-sample_rmse
    rmse = c(6.38595926753389, 17.5287922905505, 1.86663287256962, 26.9484732256139, 22.1909525144749, 17.8077009387508, 45.431310728718,
             64.4586056443939, 30.0480417057874, 39.2443325010152, 25576289.2394761, 27.9597544516411, 11.7192007403981, 38.8974447104402),
    #out-of-sample_mape
    mae = c(5.50591018708613, 10.231486824798, 1.64981464034556, 13.8570232659223, 17.9499348579082, 11.8067344910752, 27.184606205835, 35.1433105593842,
            25.0245963044771, 31.2643488662766, 12788151.7949261, 21.3443334945594, 8.31156599769916, 23.7678205405266),
    #out-of-sample_pseudo_loss
    mphe = c(4.66499787, 9.452485, 1.0106225, 13.13417925, 16.99733404, 10.9080032, 26.30159, 34.32570922, 24.13361833, 30.33101519,
                     12788151.0121739, 20.48031506, 7.41596589, 22.94726662),
    #out-of-sample_quantile
    mpe = c(3.107214, 3.790083, 1.237361, 9.602278, 10.69042, 7.917679,
                8.884853, 12.140211, 12.761482, 14.868734, 3197041.489351, 11.433081,
                4.85776, 9.414893),
    #oos mape
    mape = c(1.0944, 2.1622, 0.7751, 2.6899, 1.9377, 3.4806, 4.4673, 1.8383,
             5.6731, 3.809, 1.9521, 4.5438, 0.8038, 1.0787),
    #hr
    hr = c(0.6, 0.8, 1, 1, 0.4, 0.4, 0.5, 0.8, 0.66667, 0.5, 1, 0.75, 0.8,
           0.6),
    #mb
    mb = c(-1.417, 5.3026, -1.6498, -10.6951, -6.8618, -8.0572, 18.8298,
           21.7258, -0.9967, 3.0538, 12788137.6324, -3.0437, -2.8079, 9.8761
    )


  )
  #Rename
  rownames(eval_metrics) <-  c("2001-09-15", "2001-10-15", "2001-11-15", "2001-12-15",
                               "2002-01-15", "2002-02-15", "2002-03-15", "2002-04-15",
                               "2002-05-15", "2002-06-15", "2002-07-15", "2002-08-15",
                               "2002-09-15", "2002-10-15")


  results$outputs[[4]] <- eval_metrics
  #final_model
  results$outputs[[5]] <- ml_backtest_results@final_model


  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-03
  )

})

#Define your test
test_that("OLS - run_ml_backtest works with toy_preprocessed_features_and_targets", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      rebalancing_months = 7,
      training_sample_size = 5,
      ml_algorithm = "ols",
      target_fwd_name = "fwd_premium_3m",
      show_plots = FALSE)
  }))

  #1st rebalancing
  #Features Objects
  features_first_rebal <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15")),]
  #Targets
  targets_first_rebal <- toy_preprocessed_targets[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15")),]
  #Full data
  full_data_first_rebal <- cbind(
    fwd_premium_3m = targets_first_rebal$fwd_premium_3m,
    features_first_rebal)
  #Fitting
  first_model <- stats::lm(formula = fwd_premium_3m~., data = full_data_first_rebal[,-c(2:4)])
  #Predict
  dates_first_prediction <- c("2022-11-15", "2022-12-15", "2023-01-15",
                              "2023-02-15", "2023-03-15", "2023-04-15",
                              "2023-05-15", "2023-06-15")
  #Resulting vectors
  prediction_list_first_prediction <- list()
  y_list_first_prediction <- list()
  error_list_first_prediction <- list()
  y_list_first_prediction <- list()
  r_oos_first_prediction <- vector(length = length(dates_first_prediction))
  cp_oos_first_prediction <- vector(length = length(dates_first_prediction))
  rmse_oos_first_prediction <- vector(length = length(dates_first_prediction))
  mae_oos_first_prediction <- vector(length = length(dates_first_prediction))
  pseudo_huber_oos_first_prediction <- vector(length = length(dates_first_prediction))
  pinball_oos_first_prediction <- vector(length = length(dates_first_prediction))
  mape_oos_first_prediction <- vector(length = length(dates_first_prediction))
  hr_oos_first_prediction <- vector(length = length(dates_first_prediction))
  mb_oos_first_prediction <- vector(length = length(dates_first_prediction))


  #First Predictions
  for(i in 1:length(dates_first_prediction)){

    #Subset for each date
    features_first_prediction <-  toy_preprocessed_features[which(toy_preprocessed_features$dates %in% dates_first_prediction[i]),]
    target_first_prediction <-  toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% dates_first_prediction[i])]

    #Fill obj
    prediction_list_first_prediction[[i]] <- stats::predict(first_model, features_first_prediction[,-c(1:3)])
    error_list_first_prediction[[i]] <- target_first_prediction - prediction_list_first_prediction[[i]]
    y_list_first_prediction[[i]] <- target_first_prediction

    #Rename
    names(prediction_list_first_prediction[[i]]) <- features_first_prediction$tickers
    names(error_list_first_prediction[[i]]) <- features_first_prediction$tickers
    names(y_list_first_prediction[[i]]) <- features_first_prediction$tickers

    r_oos_first_prediction[i] <- 1 - (sum(error_list_first_prediction[[i]]^(2))/sum(y_list_first_prediction[[i]]^2))
    cp_oos_first_prediction[i] <- sum((y_list_first_prediction[[i]])*(prediction_list_first_prediction[[i]]))/length(y_list_first_prediction[[i]])
    rmse_oos_first_prediction[i] <- sqrt(sum(error_list_first_prediction[[i]]^(2))/length(y_list_first_prediction[[i]]))
    mae_oos_first_prediction[i] <- sum(abs(error_list_first_prediction[[i]]))/length(y_list_first_prediction[[i]])
    pseudo_huber_oos_first_prediction[i] <- mean(1^2*(sqrt(1+(error_list_first_prediction[[i]])^2)-1))
    pinball_oos_first_prediction[i] <- mean(ifelse(error_list_first_prediction[[i]] >= 0, 0.5*error_list_first_prediction[[i]], (1-0.5)*(-1)*error_list_first_prediction[[i]]))
    mape_oos_first_prediction[i] <- mean(abs(error_list_first_prediction[[i]]/y_list_first_prediction[[i]]))
    hr_oos_first_prediction[i] <- length(which(sign(y_list_first_prediction[[i]]) == sign(prediction_list_first_prediction[[i]])))/length(y_list_first_prediction[[i]])
    mb_oos_first_prediction[i] <- mean(error_list_first_prediction[[i]])

  }
  #Set dates
  names(prediction_list_first_prediction) <- dates_first_prediction
  names(error_list_first_prediction) <- dates_first_prediction
  names(y_list_first_prediction) <- dates_first_prediction
  names(r_oos_first_prediction) <- dates_first_prediction
  names(cp_oos_first_prediction) <- dates_first_prediction
  names(rmse_oos_first_prediction) <- dates_first_prediction
  names(mae_oos_first_prediction) <- dates_first_prediction
  names(pseudo_huber_oos_first_prediction) <- dates_first_prediction
  names(pinball_oos_first_prediction) <- dates_first_prediction
  names(mape_oos_first_prediction) <- dates_first_prediction
  names(hr_oos_first_prediction) <- dates_first_prediction
  names(mb_oos_first_prediction) <- dates_first_prediction


  #2nd rebalancing
  #Features Objects
  features_second_rebal <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                  "2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                  "2023-02-15", "2023-03-15", "2023-04-15")),]
  #Targets
  targets_second_rebal <- toy_preprocessed_targets[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                "2022-10-15",
                                                                                                "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                "2023-02-15", "2023-03-15", "2023-04-15")),]
  #Full data
  full_data_second_rebal <- cbind(
    fwd_premium_3m = targets_second_rebal$fwd_premium_3m,
    features_second_rebal)
  #Fitting
  second_model <- stats::lm(formula = fwd_premium_3m~., data = full_data_second_rebal[,-c(2:4)])
  #Predict
  dates_second_prediction <- c("2023-07-15")
  #Resulting obj
  prediction_list_second_prediction <- list()
  y_list_second_prediction <- list()
  error_list_second_prediction <- list()
  r_oos_second_prediction <- vector(length = length(dates_second_prediction))
  cp_oos_second_prediction <- vector(length = length(dates_second_prediction))
  rmse_oos_second_prediction <- vector(length = length(dates_second_prediction))
  mae_oos_second_prediction <- vector(length = length(dates_second_prediction))
  pseudo_huber_oos_second_prediction <- vector(length = length(dates_second_prediction))
  pinball_oos_second_prediction <- vector(length = length(dates_second_prediction))
  mape_oos_second_prediction <- vector(length = length(dates_second_prediction))
  hr_oos_second_prediction <- vector(length = length(dates_second_prediction))
  mb_oos_second_prediction <- vector(length = length(dates_second_prediction))




  for(i in 1:length(dates_second_prediction)){

    #Subset for each date
    features_second_prediction <-  toy_preprocessed_features[which(toy_preprocessed_features$dates %in% dates_second_prediction[i]),]
    target_second_prediction <-  toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% dates_second_prediction[i])]

    #Fill obj
    prediction_list_second_prediction[[i]] <- stats::predict(second_model, features_second_prediction[,-c(1:3)])
    error_list_second_prediction[[i]] <- target_second_prediction - prediction_list_second_prediction[[i]]
    y_list_second_prediction[[i]] <- target_second_prediction

    #Rename
    names(prediction_list_second_prediction[[i]]) <- features_second_prediction$tickers
    names(error_list_second_prediction[[i]]) <- features_second_prediction$tickers
    names(y_list_second_prediction[[i]]) <- features_second_prediction$tickers

    r_oos_second_prediction[i] <- 1 - (sum(error_list_second_prediction[[i]]^(2))/sum(y_list_second_prediction[[i]]^2))
    cp_oos_second_prediction[i] <- sum((y_list_second_prediction[[i]])*(prediction_list_second_prediction[[i]]))/length(y_list_second_prediction[[i]])
    rmse_oos_second_prediction[i] <- sqrt(sum(error_list_second_prediction[[i]]^(2))/length(y_list_second_prediction[[i]]))
    mae_oos_second_prediction[i] <- sum(abs(error_list_second_prediction[[i]]))/length(y_list_second_prediction[[i]])
    pseudo_huber_oos_second_prediction[i] <- mean(1^2*(sqrt(1+(error_list_second_prediction[[i]])^2)-1))
    pinball_oos_second_prediction[i] <- mean(ifelse(error_list_second_prediction[[i]] >= 0, 0.5*error_list_second_prediction[[i]], (1-0.5)*(-1)*error_list_second_prediction[[i]]))
    mape_oos_second_prediction[i] <- mean(abs(error_list_second_prediction[[i]]/y_list_second_prediction[[i]]))
    hr_oos_second_prediction[i] <- length(which(sign(y_list_second_prediction[[i]]) == sign(prediction_list_second_prediction[[i]])))/length(y_list_second_prediction[[i]])
    mb_oos_second_prediction[i] <- mean(error_list_second_prediction[[i]])


  }
  #Set dates
  names(prediction_list_second_prediction) <- dates_second_prediction
  names(error_list_second_prediction) <- dates_second_prediction
  names(y_list_second_prediction) <- dates_second_prediction
  names(r_oos_second_prediction) <- dates_second_prediction
  names(cp_oos_second_prediction) <- dates_second_prediction
  names(rmse_oos_second_prediction) <- dates_second_prediction
  names(mae_oos_second_prediction) <- dates_second_prediction
  names(pseudo_huber_oos_second_prediction) <- dates_second_prediction
  names(pinball_oos_second_prediction) <- dates_second_prediction
  names(mape_oos_second_prediction) <- dates_second_prediction
  names(hr_oos_second_prediction) <- dates_second_prediction
  names(mb_oos_second_prediction) <- dates_second_prediction


  #Create final objects
  results <- list()
  outputs <- list()

  results[[1]] <- outputs
  #Prediction list
  prediction_list <- c(prediction_list_first_prediction, prediction_list_second_prediction)
  names(prediction_list) <- c(names(prediction_list_first_prediction), names(prediction_list_second_prediction))
  results$outputs[[1]] <- prediction_list

  #Error list
  error_list <- c(error_list_first_prediction, error_list_second_prediction)
  names(error_list) <- c(names(error_list_first_prediction), names(error_list_second_prediction))
  results$outputs[[2]] <- error_list

  #Y list
  y_list <- c(y_list_first_prediction, y_list_second_prediction)
  names(y_list) <- c(names(y_list_first_prediction), names(y_list_second_prediction))
  results$outputs[[3]] <- y_list

  #Eval metrics
  eval_metrics <- data.frame(rss = c(r_oos_first_prediction, r_oos_second_prediction),
                             cp = c(cp_oos_first_prediction, cp_oos_second_prediction),
                             rmse = c(rmse_oos_first_prediction, rmse_oos_second_prediction),
                             mae = c(mae_oos_first_prediction, mae_oos_second_prediction),
                             mphe = c(pseudo_huber_oos_first_prediction, pseudo_huber_oos_second_prediction),
                             mpe = c(pinball_oos_first_prediction, pinball_oos_second_prediction),
                             mape = c(mape_oos_first_prediction, mape_oos_second_prediction),
                             hr = c(hr_oos_first_prediction, hr_oos_second_prediction),
                             mb = c(mb_oos_first_prediction, mb_oos_second_prediction)
  )
  rownames(eval_metrics) <- c(names(y_list_first_prediction), names(y_list_second_prediction))


  results$outputs[[4]] <- eval_metrics

  if(all(coefficients(second_model) == coefficients(ml_backtest_results@final_model@model))){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  names(results$outputs) <- c('oos_prediction_list', 'oos_error_list', 'oos_y_list', 'oos_testing_eval_metrics', 'final_model')
  #Compare
  expect_equal(ml_backtest_results,
               results$outputs,
               tolerance = 1e-05)
})

###################
#END OLS TESTS

#BEGIN ML TESTS (TRAINING + VALIDATION + TESTING)
####################
###Grid Search
#Define your test Excel sheet test glmnet 1
test_that("GLMNET - run_ml_backtest works with no rebalancing, 1m target, grid_search as tuning method and rss as chosen eval metric",{

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
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
             dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400),
                               class = c("POSIXct", "POSIXt"), tzone = "UTC"), format = "%Y-%m-%d"),
             Alpha = c(3, -20, -450, 5, -2, 1,
                       6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                       8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                       -1, 4, 4),
             Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                      -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                      2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
             Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                       -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                       3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame"),
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
                       dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"), format = "%Y-%m-%d"),
                       fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                          2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                       fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                          3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                       fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                         10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame"),
      training_sample_size = 4,
      validation_sample_size = 3,
      rebalancing_months = 12,
      target_fwd_name = c("fwd_premium_1m"),
      ml_algorithm = "glmnet",
      chosen_eval_metric  = "rss",
      hyper_grid_domain = list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0, 0.9, length=10)), #Grid for lambda search
      tuning_method = c("grid_search"),
      verbose = FALSE,
      parallel = FALSE,
      show_plots = FALSE,
      quantile_tau = 0.75
    )}))

  #Define initial objects
  hyper_expanded_grid <- expand.grid(list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0, 0.9, length=10)))

  validation_eval_hyper_choice <- data.frame(rss = c(NA),  #Validation loss df
                                             cp = c(NA),
                                             rmse = c(NA),
                                             mae = c(NA),
                                             mphe = c(NA),
                                             mpe = c(NA),
                                             mape = c(NA),
                                             hr = c(NA),
                                             mb = c(NA),
                                             row.names = c("2001-09-15"))
  rebalance_dates <- c("2001-09-15")
  n_rebalance_dates <- 1

  chosen_eval_metric_val <- list()


  #Get objects to train and validate model
  features_training <- structure(
    list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                "Stock E-2001-05-15"),
         tickers = c("Stock A", "Stock B", "Stock C",
                     "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                     "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E"),
         dates = structure(c(984614400, 984614400, 984614400, 984614400,
                             984614400, 987292800, 987292800, 987292800, 987292800, 987292800,
                             989884800, 989884800, 989884800, 989884800, 989884800),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1),
         Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3),
         Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500)), row.names = c(NA, -15L), class = "data.frame")

  target_training <- structure(list(
    fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8, 1, 7, 2, 7, 8, 8),
    fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1),
    fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3)),
    row.names = c(NA, -15L), class = "data.frame")

  features_validation <- structure(
    list(id = c("Stock A-2001-06-15", "Stock B-2001-06-15",
                "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15",
                "Stock A-2001-07-15", "Stock B-2001-07-15", "Stock C-2001-07-15",
                "Stock D-2001-07-15", "Stock E-2001-07-15", "Stock A-2001-08-15",
                "Stock B-2001-08-15", "Stock C-2001-08-15", "Stock D-2001-08-15",
                "Stock E-2001-08-15"),
         tickers = c("Stock A", "Stock B", "Stock C",
                     "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                     "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E"),
         dates = structure(c(992563200, 992563200, 992563200, 992563200,
                             992563200, 995155200, 995155200, 995155200, 995155200, 995155200,
                             997833600, 997833600, 997833600, 997833600, 997833600), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(5, 2, -20, -1, 2, -2, 20, -150, -50, -1, 1, 1, -20, -25, -20),
         Beta = c(3, 1, 1, 2, 1, 13, -12, 1, 5, 2, 10, -10, 4, 1, 1),
         Gamma = c(20, -15, 6, 3, 6, 0, 3, 20, 3, 4, -523, 4, 12, -10, 405)),
    row.names = c(NA, -15L), class = "data.frame")

  target_validation <- structure(list(
    fwd_premium_1m = c(1, 3, 5, 7, 1, 2, 5, 1, 2, 2, 1, -1, -9, -2, 1),
    fwd_premium_3m = c(0, 3, 8, 3, 2, 6, 8, 3, 1, 3, 5, 2, 5, 1, -9),
    fwd_sharpe_1m = c(1, 5, 6, 10, 3, 1, 4, 5, 1, 0, 3, 1, 1, 4, 1)), row.names = c(NA, -15L), class = "data.frame")


  #Start first rebalancing
  chosen_eval_metric_val[[1]] <- data.frame(alpha = hyper_expanded_grid$alpha,
                                            lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio,
                                            best_lam = rep(NA,30), chosen_eval_metric = rep(NA, 30))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_1m),
                                         ncol = nrow(hyper_expanded_grid)))
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]])
  best_lam <- vector(length =  nrow(hyper_expanded_grid))

  for(s in 1:length(hyper_expanded_grid$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_1m,
      alpha = hyper_expanded_grid$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam[s] <- glm.mod1$lambda[
      which.max(1 - (colSums((target_validation$fwd_premium_1m -
                predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)])))^2)/sum(target_validation$fwd_premium_1m^2)))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam[s])

    #RSQUARED CHOSEN
    chosen_eval_metric_val[[1]]$chosen_eval_metric[which(chosen_eval_metric_val[[1]]$alpha == unique(hyper_expanded_grid$alpha)[s])] <-
      (1 - (sum((target_validation$fwd_premium_1m -
                     shrinkage.pred_df[,s])^2)/sum(target_validation$fwd_premium_1m^2)))



  }

  chosen_eval_metric_val[[1]]$best_lam <- best_lam


  #RSQUARED IS MAX: PAY ATTENTION
  hyper_choice <- which.max(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice])^2)/sum(target_validation$fwd_premium_1m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice])^2))

  validation_eval_hyper_choice$cp[1] <- mean(target_validation$fwd_premium_1m*shrinkage.pred_df[,hyper_choice])

  validation_eval_hyper_choice$mae[1] <- mean(abs(target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice]))

  validation_eval_hyper_choice$mphe[1] <- mean(1^2*(sqrt(1+((target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice])/1)^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice]) >= 0,
                                                         0.75*(target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice]),
                                                         (1-0.75)*(-1)*(target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice])/target_validation$fwd_premium_1m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(target_validation$fwd_premium_1m) == sign(shrinkage.pred_df[,hyper_choice])))/
    length(target_validation$fwd_premium_1m)

  validation_eval_hyper_choice$mb[1] <- mean(target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice])




  #Refit
  features_training_and_validation <- rbind(features_training, features_validation)
  target_training_and_validation <- rbind(target_training, target_validation)

  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_1m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice],
                                  lambda = best_lam[hyper_choice])
  coef(glm.mod.refit)

  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_1m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice],
                                  lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[hyper_choice])


  coef(glm.mod.refit)

  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")
  #Pred list
  prediction_list <- list(`2001-09-15` = c(`Stock A` = 3.1, `Stock B` = 3.1, `Stock C` = 3.1, `Stock D` = 3.1, `Stock E` = 3.1),
                          `2001-10-15` = c(`Stock A` = 3.1, `Stock B` = 3.1, `Stock C` = 3.1, `Stock D` = 3.1, `Stock E` = 3.1),
                          `2001-11-15` = c(`Stock A` = 3.1, `Stock B` = 3.1, `Stock C` = 3.1, `Stock D` = 3.1, `Stock E` = 3.1))
  results$outputs[[1]] <- prediction_list
  #Error list
  error_list <- list(`2001-09-15` = c(`Stock A` = 6.9, `Stock B` = 31.9, `Stock C` = -1.1, `Stock D` = -13.1, `Stock E` = 0.9),
                     `2001-10-15` = c(`Stock A` = -0.1,`Stock B` = -155.1, `Stock C` = 0.9, `Stock D` = -48.1, `Stock E` = -8.1),
                     `2001-11-15` = c(`Stock A` = -2.1, `Stock B` = -0.1, `Stock C` = -23.1, `Stock D` = -6.1, `Stock E` = -3.1))
  results$outputs[[2]] <- error_list
  #Y-list
  y_list <- list(`2001-09-15` = c(`Stock A` = 10, `Stock B` = 35, `Stock C` = 2, `Stock D` = -10, `Stock E` = 4),
                 `2001-10-15` = c(`Stock A` = 3, `Stock B` = -152, `Stock C` = 4, `Stock D` = -45, `Stock E` = -5),
                 `2001-11-15` = c(`Stock A` = 1, `Stock B` = 3, `Stock C` = -20, `Stock D` = -3, `Stock E` = 0))
  results$outputs[[3]] <- y_list
  #Eval metrics
  oos_testing_eval_metrics <-structure(list(rss =c(0.142664359861592, -0.0499245402915127, -0.39582338902148),
                                            cp = c(25.42, -120.9, -11.78),
                                            rmse = c(15.7407115468139, 72.7132037528261, 10.8152669869957),
                                            mae = c(10.78, 42.46, 6.9),
                                            mphe = c(9.97156782, 41.74509250, 6.17825728),
                                            mpe = c(6.665, 10.705, 1.725),
                                            mape = c(0.737286, 0.793523, Inf),
                                            hr = c(0.8, 0.4, 0.4),
                                            mb = c(5.1, -42.1, -6.9)
                                            ), class = "data.frame", row.names = c("2001-09-15", "2001-10-15", "2001-11-15"))
  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(glm.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }


  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- c("2001-09-15")
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = c("2001-09-15"),
                                     alpha = hyper_expanded_grid$alpha[hyper_choice],
                                     lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[hyper_choice],
                                     best_lam = best_lam[hyper_choice])


  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-05
  )

})

#Define your test Excel sheet test glmnet 2
test_that("GLMNET - run_ml_backtest works with rebalancing at final, 1m target, grid_search as tuning method and hr as chosen eval metric",{

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
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
             dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400),
                               class = c("POSIXct", "POSIXt"), tzone = "UTC"), format = "%Y-%m-%d"),
             Alpha = c(3, -20, -450, 5, -2, 1,
                       6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                       8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                       -1, 4, 4),
             Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                      -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                      2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
             Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                       -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                       3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame"),
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
                       dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                       format = "%Y-%m-%d"),
                       fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                          2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                       fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                          3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                       fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                         10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame"),
      training_sample_size = 4,
      validation_sample_size = 3,
      rebalancing_months = 11,
      target_fwd_name = c("fwd_premium_1m"),
      chosen_eval_metric  = "hr",
      hyper_grid_domain = list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0.1, 0.9, length=10)), #Grid for lambda search
      tuning_method = c("grid_search"),
      verbose = FALSE,
      show_plots = FALSE,
      parallel = FALSE,
      ml_algorithm = c("glmnet"),
      huber_delta = 1.35
    )}))

  #Define initial objects
  hyper_expanded_grid <- expand.grid(list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0.1, 0.9, length=10)))

  validation_eval_hyper_choice <- data.frame(rss =c(NA, NA),  #Validation loss df
                                             cp = c(NA, NA),
                                             rmse = c(NA, NA),
                                             mae = c(NA, NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             row.names = c("2001-09-15", "2001-11-15"))


  rebalance_dates <- c("2001-09-15", "2001-11-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()



  #Get objects to train and validate model
  features_training <- structure(
    list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                "Stock E-2001-05-15"),
         tickers = c("Stock A", "Stock B", "Stock C",
                     "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                     "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E"),
         dates = structure(c(984614400, 984614400, 984614400, 984614400,
                             984614400, 987292800, 987292800, 987292800, 987292800, 987292800,
                             989884800, 989884800, 989884800, 989884800, 989884800),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1),
         Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3),
         Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500)), row.names = c(NA, -15L), class = "data.frame")

  target_training <- structure(list(
    fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8, 1, 7, 2, 7, 8, 8),
    fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1),
    fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3)),
    row.names = c(NA, -15L), class = "data.frame")

  features_validation <- structure(
    list(id = c("Stock A-2001-06-15", "Stock B-2001-06-15",
                "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15",
                "Stock A-2001-07-15", "Stock B-2001-07-15", "Stock C-2001-07-15",
                "Stock D-2001-07-15", "Stock E-2001-07-15", "Stock A-2001-08-15",
                "Stock B-2001-08-15", "Stock C-2001-08-15", "Stock D-2001-08-15",
                "Stock E-2001-08-15"),
         tickers = c("Stock A", "Stock B", "Stock C",
                     "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                     "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E"),
         dates = structure(c(992563200, 992563200, 992563200, 992563200,
                             992563200, 995155200, 995155200, 995155200, 995155200, 995155200,
                             997833600, 997833600, 997833600, 997833600, 997833600), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
         Alpha = c(5, 2, -20, -1, 2, -2, 20, -150, -50, -1, 1, 1, -20, -25, -20),
         Beta = c(3, 1, 1, 2, 1, 13, -12, 1, 5, 2, 10, -10, 4, 1, 1),
         Gamma = c(20, -15, 6, 3, 6, 0, 3, 20, 3, 4, -523, 4, 12, -10, 405)),
    row.names = c(NA, -15L), class = "data.frame")

  target_validation <- structure(list(
    fwd_premium_1m = c(1, 3, 5, 7, 1, 2, 5, 1, 2, 2, 1, -1, -9, -2, 1),
    fwd_premium_3m = c(0, 3, 8, 3, 2, 6, 8, 3, 1, 3, 5, 2, 5, 1, -9),
    fwd_sharpe_1m = c(1, 5, 6, 10, 3, 1, 4, 5, 1, 0, 3, 1, 1, 4, 1)), row.names = c(NA, -15L), class = "data.frame")


  #Start first rebalancing
  chosen_eval_metric_val[[1]] <- data.frame(alpha = hyper_expanded_grid$alpha,
                                            lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio,
                                            best_lam = rep(NA,30), chosen_eval_metric = rep(NA, 30))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_1m),
                                         ncol = nrow(hyper_expanded_grid)))
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]])
  best_lam1 <- vector(length =  nrow(hyper_expanded_grid))

  for(s in 1:length(hyper_expanded_grid$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_1m,
      alpha = hyper_expanded_grid$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam1[s] <- glm.mod1$lambda[
      which.max(
        (colMeans((target_validation$fwd_premium_1m *
                     predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]))) > 0
        ))
      )
    ]

    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam1[s])

    #HR CHOSEN
    chosen_eval_metric_val[[1]]$chosen_eval_metric[which(chosen_eval_metric_val[[1]]$alpha == unique(hyper_expanded_grid$alpha)[s])] <-
     mean((target_validation$fwd_premium_1m * shrinkage.pred_df[,s]) > 0)


  }


  chosen_eval_metric_val[[1]]$best_lam <- best_lam1

  #HR IS MAX: PAY ATTENTION
  hyper_choice1 <- which.max(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice1])^2)/sum(target_validation$fwd_premium_1m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(target_validation$fwd_premium_1m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1.35)^2*(sqrt(1+((target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice1])/(1.35))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                         0.5*(target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice1]),
                                                         (1-0.5)*(-1)*(target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice1])/target_validation$fwd_premium_1m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(target_validation$fwd_premium_1m) == sign(shrinkage.pred_df[,hyper_choice1])))/
    length(target_validation$fwd_premium_1m)

  validation_eval_hyper_choice$mb[1] <- mean(target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice1])

  #Refit
  features_training_and_validation <- rbind(features_training, features_validation)
  target_training_and_validation <- rbind(target_training, target_validation)

  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_1m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice1],
                                  lambda = best_lam1[hyper_choice1])

  #2nd Rebalancing

  #Get objects to train and validate model
  features_training <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                             "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                             "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                             "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                             "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                             "Stock E-2001-05-15", "Stock A-2001-06-15", "Stock B-2001-06-15",
                                             "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15",
                                             "Stock A-2001-07-15", "Stock B-2001-07-15", "Stock C-2001-07-15",
                                             "Stock D-2001-07-15", "Stock E-2001-07-15"),
                                      tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                                                  "Stock C", "Stock D", "Stock E", "Stock A", "Stock B", "Stock C",
                                                  "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                  "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E"),
                                      dates = structure(c(984614400, 984614400, 984614400, 984614400,
                                                          984614400, 987292800, 987292800, 987292800, 987292800, 987292800,
                                                          989884800, 989884800, 989884800, 989884800, 989884800, 992563200,
                                                          992563200, 992563200, 992563200, 992563200, 995155200, 995155200,
                                                          995155200, 995155200, 995155200), class = c("POSIXct", "POSIXt"
                                                          ), tzone = "UTC"), Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3,
                                                                                       -450, 4, 9, 2, -1, 5, 2, -20, -1, 2, -2, 20, -150, -50, -1),
                                      Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3,
                                               3, 1, 1, 2, 1, 13, -12, 1, 5, 2),
                                      Gamma = c(800, 9, 10, -9,
                                                3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500, 20, -15, 6, 3, 6,
                                                0, 3, 20, 3, 4)), row.names = c(NA, -25L), class = "data.frame")

  target_training <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                       1, 7, 2, 7, 8, 8, 1, 3, 5, 7, 1, 2, 5, 1, 2, 2),
                                    fwd_premium_3m = c(4,  5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1, 0, 3, 8, 3, 2, 6, 8,  3, 1, 3),
                                    fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3, 1, 5, 6, 10, 3, 1, 4, 5, 1, 0)),
                               row.names = c(NA,-25L), class = "data.frame")

  features_validation <- structure(list(id = c("Stock A-2001-08-15", "Stock B-2001-08-15",
                                               "Stock C-2001-08-15", "Stock D-2001-08-15", "Stock E-2001-08-15",
                                               "Stock A-2001-09-15", "Stock B-2001-09-15", "Stock C-2001-09-15",
                                               "Stock D-2001-09-15", "Stock E-2001-09-15", "Stock A-2001-10-15",
                                               "Stock B-2001-10-15", "Stock C-2001-10-15", "Stock D-2001-10-15",
                                               "Stock E-2001-10-15"),
                                        tickers = c("Stock A", "Stock B", "Stock C",
                                                    "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                    "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E"),
                                        dates = structure(c(997833600, 997833600, 997833600, 997833600,
                                                            997833600, 1000512000, 1000512000, 1000512000, 1000512000, 1000512000,
                                                            1003104000, 1003104000, 1003104000, 1003104000, 1003104000),
                                                          class = c("POSIXct",  "POSIXt"), tzone = "UTC"),
                                        Alpha = c(1, 1, -20, -25, -20, 6, 1,8, 1, -1, 1, -2, 17, 4, 4),
                                        Beta = c(10, -10, 4, 1, 1, 4, 3,  24, 2, -1, -5, 4, 19, 5, -20),
                                        Gamma = c(-523, 4, 12, -10, 405, 2, 4, 13, 0, 0, 3, 3, -4, -1, 1)), row.names = c(NA, -15L), class = "data.frame")

  target_validation <- structure(list(fwd_premium_1m = c(1, -1, -9, -2, 1, 10, 35, 2, -10, 4,
                                                         3, -152, 4, -45, -5),
                                      fwd_premium_3m = c(5, 2, 5, 1, -9, -5, 5, 3, 11, -4, -1,
                                                         1, 40, 4, 4),
                                      fwd_sharpe_1m = c(3, 1, 1, 4, 1, 1, 1, 1, 12, 3, 0, 4,
                                                        5, 1, 1)), row.names = c(NA, -15L), class = "data.frame")


  chosen_eval_metric_val[[2]] <- data.frame(alpha = hyper_expanded_grid$alpha,
                                            lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio,
                                            best_lam = rep(NA,30), chosen_eval_metric = rep(NA, 30))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_1m),
                                         ncol = nrow(hyper_expanded_grid)))
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]])
  best_lam2 <- vector(length =  nrow(hyper_expanded_grid))

  for(s in 1:length(hyper_expanded_grid$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_1m,
      alpha = hyper_expanded_grid$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam2[s] <- glm.mod1$lambda[
      which.max(
        (colMeans((target_validation$fwd_premium_1m *
                   predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]))) > 0
                  ))
        )
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam2[s])

    #HR CHOSEN
    chosen_eval_metric_val[[2]]$chosen_eval_metric[which(chosen_eval_metric_val[[2]]$alpha == unique(hyper_expanded_grid$alpha)[s])] <-
      mean((target_validation$fwd_premium_1m * shrinkage.pred_df[,s]) > 0)


  }

  chosen_eval_metric_val[[2]]$best_lam <- best_lam2


  #HR IS MAX: PAY ATTENTION
  hyper_choice2 <- which.max(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice2])^2)/sum(target_validation$fwd_premium_1m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(target_validation$fwd_premium_1m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice2]))

  validation_eval_hyper_choice$mphe[2] <- mean((1.35)^2*(sqrt(1+((target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice2])/(1.35))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                         0.5*(target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice2]),
                                                         (1-0.5)*(-1)*(target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs(
    (target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice2])/target_validation$fwd_premium_1m))

  validation_eval_hyper_choice$hr[2] <- length(which(sign(target_validation$fwd_premium_1m) == sign(shrinkage.pred_df[,hyper_choice2])))/
    length(target_validation$fwd_premium_1m)

  validation_eval_hyper_choice$mb[2] <- mean(target_validation$fwd_premium_1m - shrinkage.pred_df[,hyper_choice2])


  #Refit
  features_training_and_validation <- rbind(features_training, features_validation)
  target_training_and_validation <- rbind(target_training, target_validation)

  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_1m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice2],
                                  lambda = best_lam2[hyper_choice2])

  coef(glm.mod.refit)



  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_1m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice2],
                                  lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[hyper_choice2])

  coef(glm.mod.refit)


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")
  #Pred list
  prediction_list <- list(
    `2001-09-15` = c(`Stock A` = 3.09999999999021, `Stock B` = 3.09999999999021,
                     `Stock C` = 3.09999999999021, `Stock D` = 3.09999999999021, `Stock E` = 3.09999999999021
    ), `2001-10-15` = c(`Stock A` = 3.09999999999021, `Stock B` = 3.09999999999021,
                        `Stock C` = 3.09999999999021, `Stock D` = 3.09999999999021, `Stock E` = 3.09999999999021
    ), `2001-11-15` = c(`Stock A` = -1.5250000, `Stock B` = -1.5250000,
                        `Stock C` = -1.5250000, `Stock D` = -1.5250000, `Stock E` = -1.5250000
    ))
  results$outputs[[1]] <- prediction_list
  #Error list
  error_list <- list(`2001-09-15` = c(`Stock A` = 6.90000000000979, `Stock B` = 31.9000000000098,
                                      `Stock C` = -1.09999999999021, `Stock D` = -13.0999999999902,
                                      `Stock E` = 0.90000000000979),
                     `2001-10-15` = c(`Stock A` = -0.0999999999902101, `Stock B` = -155.09999999999,
                                      `Stock C` = 0.90000000000979, `Stock D` = -48.0999999999902, `Stock E` = -8.09999999999021),
                     `2001-11-15` = c(`Stock A` = 2.525, `Stock B` = 4.525, `Stock C` = -18.475, `Stock D` = -1.475, `Stock E` = 1.525))
  results$outputs[[2]] <- error_list
  #Y-list
  y_list <- list(`2001-09-15` = c(`Stock A` = 10, `Stock B` = 35, `Stock C` = 2, `Stock D` = -10, `Stock E` = 4),
                 `2001-10-15` = c(`Stock A` = 3, `Stock B` = -152, `Stock C` = 4, `Stock D` = -45, `Stock E` = -5),
                 `2001-11-15` = c(`Stock A` = 1, `Stock B` = 3, `Stock C` = -20, `Stock D` = -3, `Stock E` = 0))
  results$outputs[[3]] <- y_list
  #Eval metrics
  oos_testing_eval_metrics <-structure(list(rss =c(0.14266435986, -0.04992454029, 0.110553401),
                                            cp = c(25.4199999999197, -120.899999999618,
                                                             5.795),
                                            rmse = c(15.7407115, 72.7132038, 8.6334017),
                                            mae = c(10.78, 42.46, 5.705),
                                            mphe = c(13.16052, 56.06894, 6.31687),
                                            mpe = c(5.39, 21.23, 2.8525),
                                            mape = c(0.737286, 0.793523, Inf),
                                            hr = c(0.8, 0.4, 0.4),
                                            mb = c(5.1, -42.1, -2.275)
                                            ),

                                       class = "data.frame", row.names = c("2001-09-15", "2001-10-15", "2001-11-15"))
  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(glm.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }


  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- c("2001-09-15", "2001-11-15")
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = c("2001-09-15", "2001-11-15"),
                                     alpha = c(hyper_expanded_grid$alpha[hyper_choice1], hyper_expanded_grid$alpha[hyper_choice2]),
                                     lambda.min.ratio = c(hyper_expanded_grid$lambda.min.ratio[hyper_choice1], hyper_expanded_grid$lambda.min.ratio[hyper_choice2]),
                                     best_lam = c(best_lam1[hyper_choice1], best_lam2[hyper_choice2])
                                     )

  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-2
  )

})

#Define your test Excel sheet test glmnet 3
test_that("GLMNET - run_ml_backtest works with rebalancing at final, 3m target, grid_search as tuning method and rss as chosen eval metric",{

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
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
             dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400),
                               class = c("POSIXct", "POSIXt"), tzone = "UTC"), format = "%Y-%m-%d"),
             Alpha = c(3, -20, -450, 5, -2, 1,
                       6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                       8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                       -1, 4, 4),
             Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                      -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                      2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
             Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                       -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                       3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame"),
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
                       dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                       format = "%Y-%m-%d"),
                       fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                          2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                       fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                          3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                       fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                         10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame"),
      training_sample_size = 4,
      validation_sample_size = 3,
      rebalancing_months = 11,
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "rss",
      hyper_grid_domain = list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0, 0.9, length=10)), #Grid for lambda search
      tuning_method = c("grid_search"),
      parallel = FALSE,
      verbose = FALSE,
      ml_algorithm = "glmnet",
      show_plots = FALSE,
      huber_delta = 0.5
    )}))


  #Define initial objects
  hyper_expanded_grid <- expand.grid(list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0, 0.9, length=10)))
  validation_eval_hyper_choice <- data.frame(rss =c(NA, NA),  #Validation loss df
                                             cp = c(NA, NA),
                                             rmse = c(NA, NA),
                                             mae = c(NA, NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             mape = c(NA,NA),
                                             hr = c(NA,NA),
                                             mb = c(NA,NA),
                                             row.names = c("2001-09-15", "2001-11-15"))
  rebalance_dates <- c("2001-09-15", "2001-11-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #Start first rebalancing

  #Get objects to train and validate model
  features_training <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                             "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(984614400, 984614400, 984614400, 984614400,
                         984614400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(3,  1, 2, 5, 5), Beta = c(4, 5, 6, 0, 2), Gamma = c(800, 9, 10, -9, 3)), row.names = c(NA, -5L), class = "data.frame")

  target_training <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5),
                                    fwd_premium_3m = c(4, 5, 0, 1, 9),
                                    fwd_sharpe_1m = c(7, 4, 2, 4, 7)), row.names = c(NA, -5L), class = "data.frame")

  features_validation <- structure(list(id = c("Stock A-2001-06-15", "Stock B-2001-06-15",
                                               "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(992563200, 992563200, 992563200, 992563200,
                         992563200), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(5,  2, -20, -1, 2), Beta = c(3, 1, 1, 2, 1), Gamma = c(20, -15, 6,  3, 6)), row.names = c(NA, -5L), class = "data.frame")

  target_validation <- structure(list(fwd_premium_1m = c(1, 3, 5, 7, 1),
                                      fwd_premium_3m = c(0, 3, 8, 3, 2),
                                      fwd_sharpe_1m = c(1, 5, 6, 10, 3)), row.names = c(NA,  -5L), class = "data.frame")



  #Start first rebalancing
  chosen_eval_metric_val[[1]] <- data.frame(alpha = hyper_expanded_grid$alpha,
                                            lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio,
                                            best_lam = rep(NA,30), chosen_eval_metric = rep(NA, 30))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid)))
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]])
  best_lam1 <- vector(length =  nrow(hyper_expanded_grid))

  for(s in 1:length(hyper_expanded_grid$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_3m,
      alpha = hyper_expanded_grid$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam1[s] <- glm.mod1$lambda[
      which.max(1 - (colSums((target_validation$fwd_premium_3m -
                                predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)])))^2)/sum(target_validation$fwd_premium_3m^2)))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam1[s])

    #RSQUARED CHOSEN
    chosen_eval_metric_val[[1]]$chosen_eval_metric[which(chosen_eval_metric_val[[1]]$alpha == unique(hyper_expanded_grid$alpha)[s])] <-
      (1 - (sum((target_validation$fwd_premium_3m -
                   shrinkage.pred_df[,s])^2)/sum(target_validation$fwd_premium_3m^2)))



  }
  chosen_eval_metric_val[[1]]$best_lam <- best_lam1


  #RSQUARED IS MAX: PAY ATTENTION
  hyper_choice1 <- which.max(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(target_validation$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(target_validation$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((0.5)^2*(sqrt(1+((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                          (0.5))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                         0.5*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                         (1-0.5)*(-1)*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/target_validation$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(target_validation$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice1])))/
    length(target_validation$fwd_premium_3m)

  validation_eval_hyper_choice$mb[1] <- mean(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])




  #Refit
  features_training_and_validation <-structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                                           "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                                           "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                                           "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                                           "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                                           "Stock E-2001-05-15", "Stock A-2001-06-15", "Stock B-2001-06-15",
                                                           "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E",
                 "Stock A", "Stock B", "Stock C", "Stock D", "Stock E", "Stock A",
                 "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                 "Stock C", "Stock D", "Stock E"), dates = structure(c(984614400,
                                                                       984614400, 984614400, 984614400, 984614400, 987292800, 987292800,
                                                                       987292800, 987292800, 987292800, 989884800, 989884800, 989884800,
                                                                       989884800, 989884800, 992563200, 992563200, 992563200, 992563200,
                                                                       992563200), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1, 5, 2, -20, -1, 2), Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3,  3, 1, 1, 2, 1),
  Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500, 20, -15, 6, 3, 6)), row.names = c(NA, -20L), class = "data.frame")

  target_training_and_validation <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                                      1, 7, 2, 7, 8, 8, 1, 3, 5, 7, 1),
                                                   fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1, 0, 3, 8, 3, 2),
                                                   fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3, 1, 5, 6, 10, 3)),
                                              row.names = c(NA, -20L), class = "data.frame")


  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice1],
                                  lambda = best_lam1[hyper_choice1])
  coef(glm.mod.refit)

  #2nd Rebalancing

  #Get objects to train and validate model
  features_training <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                             "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                             "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                             "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                             "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                             "Stock E-2001-05-15"),
                                      tickers = c("Stock A", "Stock B", "Stock C",
                                                  "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                  "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
                                      ),
                                      dates = structure(c(984614400, 984614400, 984614400, 984614400,
                                                          984614400, 987292800, 987292800, 987292800, 987292800, 987292800,
                                                          989884800, 989884800, 989884800, 989884800, 989884800), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                      Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1), Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3),
                                      Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500)), row.names = c(NA, -15L), class = "data.frame")


  target_training <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                       1, 7, 2, 7, 8, 8),
                                    fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1),
                                    fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3)), row.names = c(NA, -15L), class = "data.frame")

  features_validation <- structure(list(id = c("Stock A-2001-08-15", "Stock B-2001-08-15",
                                               "Stock C-2001-08-15", "Stock D-2001-08-15", "Stock E-2001-08-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(997833600, 997833600, 997833600, 997833600,
                         997833600), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(1, 1, -20, -25, -20), Beta = c(10, -10, 4, 1, 1), Gamma = c(-523, 4, 12, -10, 405)), row.names = c(NA, -5L), class = "data.frame")

  target_validation <-structure(list(fwd_premium_1m = c(1, -1, -9, -2, 1),
                                     fwd_premium_3m = c(5, 2, 5, 1, -9),
                                     fwd_sharpe_1m = c(3, 1, 1, 4, 1)), row.names = c(NA,  -5L), class = "data.frame")



  chosen_eval_metric_val[[2]] <- data.frame(alpha = hyper_expanded_grid$alpha,
                                            lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio,
                                            best_lam = rep(NA,30), chosen_eval_metric = rep(NA, 30))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid)))
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]])
  best_lam2 <- vector(length =  nrow(hyper_expanded_grid))

  for(s in 1:length(hyper_expanded_grid$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_3m,
      alpha = hyper_expanded_grid$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam2[s] <- glm.mod1$lambda[
      which.max(1 - (colSums((target_validation$fwd_premium_3m -
                                predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)])))^2)/sum(target_validation$fwd_premium_3m^2)))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam2[s])

    #RSQUARED CHOSEN
    chosen_eval_metric_val[[2]]$chosen_eval_metric[which(chosen_eval_metric_val[[2]]$alpha == unique(hyper_expanded_grid$alpha)[s])] <-
      (1 - (sum((target_validation$fwd_premium_3m -
                   shrinkage.pred_df[,s])^2)/sum(target_validation$fwd_premium_3m^2)))



  }

  chosen_eval_metric_val[[2]]$best_lam <- best_lam2


  #RSQUARED IS MAX: PAY ATTENTION
  hyper_choice2 <- which.max(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(target_validation$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(target_validation$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))

  validation_eval_hyper_choice$mphe[2] <- mean((0.5)^2*(sqrt(1+((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                          (0.5))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                         0.5*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                         (1-0.5)*(-1)*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs(
    (target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/target_validation$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- length(which(sign(target_validation$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice2])))/
    length(target_validation$fwd_premium_3m)

  validation_eval_hyper_choice$mb[2] <- mean(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])




  #Refit
  features_training_and_validation <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                                            "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                                            "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                                            "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                                            "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                                            "Stock E-2001-05-15", "Stock A-2001-06-15", "Stock B-2001-06-15",
                                                            "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15",
                                                            "Stock A-2001-07-15", "Stock B-2001-07-15", "Stock C-2001-07-15",
                                                            "Stock D-2001-07-15", "Stock E-2001-07-15", "Stock A-2001-08-15",
                                                            "Stock B-2001-08-15", "Stock C-2001-08-15", "Stock D-2001-08-15",
                                                            "Stock E-2001-08-15"),
                                                     tickers = c("Stock A", "Stock B", "Stock C","Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                                 "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E",
                                                                 "Stock A", "Stock B", "Stock C", "Stock D", "Stock E", "Stock A",
                                                                 "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                                                                 "Stock C", "Stock D", "Stock E"),
                                                     dates = structure(c(984614400, 984614400, 984614400, 984614400, 984614400, 987292800, 987292800,
                                                                         987292800, 987292800, 987292800, 989884800, 989884800, 989884800,
                                                                         989884800, 989884800, 992563200, 992563200, 992563200, 992563200,
                                                                         992563200, 995155200, 995155200, 995155200, 995155200, 995155200,
                                                                         997833600, 997833600, 997833600, 997833600, 997833600), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                                     Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1, 5, 2, -20, -1, 2, -2, 20, -150, -50, -1, 1, 1, -20, -25, -20),
                                                     Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3, 3, 1, 1, 2, 1, 13, -12, 1, 5, 2, 10, -10, 4, 1, 1),
                                                     Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500, 20, -15, 6, 3, 6, 0, 3, 20, 3, 4, -523, 4, 12, -10, 405)),
                                                row.names = c(NA, -30L), class = "data.frame")

  target_training_and_validation <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                                      1, 7, 2, 7, 8, 8, 1, 3, 5, 7, 1, 2, 5, 1, 2, 2, 1, -1, -9, -2,
                                                                      1),
                                                   fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1, 0, 3, 8, 3, 2, 6, 8, 3, 1, 3, 5, 2, 5, 1, -9),
                                                   fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3, 1, 5, 6, 10, 3, 1,  4, 5, 1, 0, 3, 1, 1, 4, 1)),
                                              row.names = c(NA, -30L), class = "data.frame")

  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice2],
                                  lambda = best_lam2[hyper_choice2])


  coef(glm.mod.refit)

  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice2],
                                  lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[hyper_choice2])


  coef(glm.mod.refit)

  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")
  #Pred list
  prediction_list <- list(`2001-09-15` = c(`Stock A` = 3.95, `Stock B` = 3.95, `Stock C` = 3.95,
                                           `Stock D` = 3.95, `Stock E` = 3.95), `2001-10-15` = c(`Stock A` = 3.95,
                                                                                                 `Stock B` = 3.95, `Stock C` = 3.95, `Stock D` = 3.95, `Stock E` = 3.95
                                           ), `2001-11-15` = c(`Stock A` = 3.4666667, `Stock B` = 3.4666667,
                                                               `Stock C` = 3.4666667, `Stock D` = 3.4666667, `Stock E` = 3.4666667
                                           ))
  results$outputs[[1]] <- prediction_list
  #Error list
  error_list <- list(`2001-09-15` = c(`Stock A` = -8.95, `Stock B` = 1.05, `Stock C` = -0.95,
                                      `Stock D` = 7.05, `Stock E` = -7.95),
                     `2001-10-15` = c(`Stock A` = -4.95, `Stock B` = -2.95, `Stock C` = 36.05, `Stock D` = 0.0499999999999998,
                                      `Stock E` = 0.0499999999999998),
                     `2001-11-15` = c(`Stock A` = 0.53333333, `Stock B` = -1.46666667, `Stock C` = -1.46666667, `Stock D` = -1.46666667, `Stock E` = -0.46666667))
  results$outputs[[2]] <- error_list
  #Y-list
  y_list <- list(`2001-09-15` = c(`Stock A` = -5, `Stock B` = 5, `Stock C` = 3,
                                  `Stock D` = 11, `Stock E` = -4),
                 `2001-10-15` = c(`Stock A` = -1, `Stock B` = 1, `Stock C` = 40,
                                  `Stock D` = 4, `Stock E` = 4),
                 `2001-11-15` = c(`Stock A` = 4, `Stock B` = 2, `Stock C` = 2,
                                  `Stock D` = 2, `Stock E` = 3))
  results$outputs[[3]] <- y_list
  #Eval metrics
  oos_testing_eval_metrics <-structure(list(rss =c(0.0050382653061225, 0.184325275397797,
                                                         0.812012012), cp = c(7.9, 37.92, 9.0133333),
                                            rmse = c(6.24519815538306, 16.3267418672557, 1.17945397913145
                                            ), mae = c(5.19, 8.81, 1.0800002),
                                            mphe = c(2.37338875, 4.25257161, 0.35636565),
                                            mpe = c(2.595, 4.405, 0.54),
                                            mape = c(0.989015, 1.76525, 0.497778),
                                            hr = c(0.6, 0.8, 1),
                                            mb = c(-1.95, 5.65, -0.866667)
                                            ), class = "data.frame", row.names = c("2001-09-15","2001-10-15", "2001-11-15"))
  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(glm.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }


    #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- c("2001-09-15", "2001-11-15")
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = c("2001-09-15", "2001-11-15"),
                                     alpha = c(hyper_expanded_grid$alpha[hyper_choice1], hyper_expanded_grid$alpha[hyper_choice2]),
                                     lambda.min.ratio = c(hyper_expanded_grid$lambda[hyper_choice1], hyper_expanded_grid$lambda[hyper_choice2]),
                                     best_lam = c(best_lam1[hyper_choice1], best_lam2[hyper_choice2]))


  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-03
  )

})

#Define your test  Excel sheet test glmnet 4
test_that("GLMNET - run_ml_backtest works with no rebalancing, 3m target, grid_search as tuning method and rmse as chosen eval metric - bigger sample",{

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df =
        structure(list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                              "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                              "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                              "Stock A-2001-11-15", "Stock A-2001-12-15", "Stock A-2002-01-15",
                              "Stock A-2002-02-15", "Stock A-2002-03-15", "Stock A-2002-04-15",
                              "Stock A-2002-05-15", "Stock A-2002-06-15", "Stock B-2001-03-15",
                              "Stock B-2001-04-15", "Stock B-2001-05-15", "Stock B-2001-06-15",
                              "Stock B-2001-07-15", "Stock B-2001-08-15", "Stock B-2001-09-15",
                              "Stock B-2001-10-15", "Stock B-2001-11-15", "Stock B-2001-12-15",
                              "Stock B-2002-01-15", "Stock B-2002-02-15", "Stock B-2002-03-15",
                              "Stock B-2002-04-15", "Stock B-2002-05-15", "Stock B-2002-06-15",
                              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15",
                              "Stock C-2001-06-15", "Stock C-2001-07-15", "Stock C-2001-08-15",
                              "Stock C-2001-09-15", "Stock C-2001-10-15", "Stock C-2001-11-15",
                              "Stock C-2001-12-15", "Stock C-2002-01-15", "Stock C-2002-02-15",
                              "Stock C-2002-03-15", "Stock C-2002-04-15", "Stock C-2002-05-15",
                              "Stock C-2002-06-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                              "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                              "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                              "Stock D-2001-11-15", "Stock D-2001-12-15", "Stock D-2002-01-15",
                              "Stock D-2002-02-15", "Stock D-2002-03-15", "Stock D-2002-04-15",
                              "Stock D-2002-05-15", "Stock D-2002-06-15", "Stock E-2001-03-15",
                              "Stock E-2001-04-15", "Stock E-2001-05-15", "Stock E-2001-06-15",
                              "Stock E-2001-07-15", "Stock E-2001-08-15", "Stock E-2001-09-15",
                              "Stock E-2001-10-15", "Stock E-2001-11-15", "Stock E-2001-12-15",
                              "Stock E-2002-01-15", "Stock E-2002-02-15", "Stock E-2002-03-15",
                              "Stock E-2002-04-15", "Stock E-2002-05-15", "Stock E-2002-06-15"
        ), tickers = c("Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                       "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                       "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock B",
                       "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                       "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                       "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                       "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                       "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                       "Stock C", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                       "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                       "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock E",
                       "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E",
                       "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E",
                       "Stock E", "Stock E", "Stock E"), dates =
          as.Date(structure(c(984614400,
          987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
          1003104000, 1005782400, 1008374400, 1011052800, 1013731200, 1016150400,
          1018828800, 1021420800, 1024099200, 984614400, 987292800, 989884800,
          992563200, 995155200, 997833600, 1000512000, 1003104000, 1005782400,
          1008374400, 1011052800, 1013731200, 1016150400, 1018828800, 1021420800,
          1024099200, 984614400, 987292800, 989884800, 992563200, 995155200,
          997833600, 1000512000, 1003104000, 1005782400, 1008374400, 1011052800,
          1013731200, 1016150400, 1018828800, 1021420800, 1024099200, 984614400,
          987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
          1003104000, 1005782400, 1008374400, 1011052800, 1013731200, 1016150400,
          1018828800, 1021420800, 1024099200, 984614400, 987292800, 989884800,
          992563200, 995155200, 997833600, 1000512000, 1003104000, 1005782400,
          1008374400, 1011052800, 1013731200, 1016150400, 1018828800, 1021420800,
          1024099200), class = c("POSIXct", "POSIXt"), tzone = "UTC"), format = "%Y-%m-%d"),
        Alpha = c(3, -20, -450, 5, -2, 1, 6, 1, -9, 3, 1, -2, 3,
                  20, 2, 3, 1, 7, 4, 2, 20, 1, 1, -2, -2, 8, -3, 3, 3, 12,
                  23, -7, 2, 9, 9, -20, -150, -20, 8, 17, 1, -3, 7, -9, 8,
                  30, 3, -2, 5, -2, 2, -1, -50, -25, 1, 4, 2, 307, 0, 3, 9,
                  19, 3, 0, 5, 3, -1, 2, -1, -20, -1, 4, 4, 8, -20, 13, 0,
                  50, 3, 2),
        Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 0, 0, -1,
                 1, 5, 9, 1, 5, 2, 4, 1, -12, -10, 3, 4, 1, 7, 3, 9, 1, 3,
                 -9, 3, 6, -3, -2, 1, 1, 4, 24, 19, -1, 10, 6, -40, 7, 0,
                 8, 0, 0, -2, 5, 2, 5, 1, 2, 5, 3, -2, -10, 8, 2, 8, 7, 1,
                 2, -9, 3, 1, 2, 1, -1, -20, 2, 7, 9, 9, 8, 20, 2, -10),
        Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, 3, 2, 2, 8, -2, 5, 9, -2,
                  4, -15, 3, 4, 4, 3, 7, 2, -3, 0, 10, 0, -8, 104, 10, -3,
                  2, 6, 20, 12, 13, -4, 105, 30, 12, 20, -105, 1, 7, 9, -9,
                  5, 2, 3, 3, -10, 0, -1, 4, 1, -2, -10, 1, -2, -19, 0, 3,
                  1, -500, 6, 4, 405, 0, 1, 31, 5, 87, 8, 92, 70, 0, 19)), row.names = c(NA, -80L), class = "data.frame"),
      target_m_df =
        structure(list(id = c("Stock A-2001-03-15", "Stock A-2001-04-15",
                              "Stock A-2001-05-15", "Stock A-2001-06-15", "Stock A-2001-07-15",
                              "Stock A-2001-08-15", "Stock A-2001-09-15", "Stock A-2001-10-15",
                              "Stock A-2001-11-15", "Stock A-2001-12-15", "Stock A-2002-01-15",
                              "Stock A-2002-02-15", "Stock A-2002-03-15", "Stock A-2002-04-15",
                              "Stock A-2002-05-15", "Stock A-2002-06-15", "Stock B-2001-03-15",
                              "Stock B-2001-04-15", "Stock B-2001-05-15", "Stock B-2001-06-15",
                              "Stock B-2001-07-15", "Stock B-2001-08-15", "Stock B-2001-09-15",
                              "Stock B-2001-10-15", "Stock B-2001-11-15", "Stock B-2001-12-15",
                              "Stock B-2002-01-15", "Stock B-2002-02-15", "Stock B-2002-03-15",
                              "Stock B-2002-04-15", "Stock B-2002-05-15", "Stock B-2002-06-15",
                              "Stock C-2001-03-15", "Stock C-2001-04-15", "Stock C-2001-05-15",
                              "Stock C-2001-06-15", "Stock C-2001-07-15", "Stock C-2001-08-15",
                              "Stock C-2001-09-15", "Stock C-2001-10-15", "Stock C-2001-11-15",
                              "Stock C-2001-12-15", "Stock C-2002-01-15", "Stock C-2002-02-15",
                              "Stock C-2002-03-15", "Stock C-2002-04-15", "Stock C-2002-05-15",
                              "Stock C-2002-06-15", "Stock D-2001-03-15", "Stock D-2001-04-15",
                              "Stock D-2001-05-15", "Stock D-2001-06-15", "Stock D-2001-07-15",
                              "Stock D-2001-08-15", "Stock D-2001-09-15", "Stock D-2001-10-15",
                              "Stock D-2001-11-15", "Stock D-2001-12-15", "Stock D-2002-01-15",
                              "Stock D-2002-02-15", "Stock D-2002-03-15", "Stock D-2002-04-15",
                              "Stock D-2002-05-15", "Stock D-2002-06-15", "Stock E-2001-03-15",
                              "Stock E-2001-04-15", "Stock E-2001-05-15", "Stock E-2001-06-15",
                              "Stock E-2001-07-15", "Stock E-2001-08-15", "Stock E-2001-09-15",
                              "Stock E-2001-10-15", "Stock E-2001-11-15", "Stock E-2001-12-15",
                              "Stock E-2002-01-15", "Stock E-2002-02-15", "Stock E-2002-03-15",
                              "Stock E-2002-04-15", "Stock E-2002-05-15", "Stock E-2002-06-15"
        ), tickers = c("Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                       "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock A",
                       "Stock A", "Stock A", "Stock A", "Stock A", "Stock A", "Stock B",
                       "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                       "Stock B", "Stock B", "Stock B", "Stock B", "Stock B", "Stock B",
                       "Stock B", "Stock B", "Stock B", "Stock C", "Stock C", "Stock C",
                       "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                       "Stock C", "Stock C", "Stock C", "Stock C", "Stock C", "Stock C",
                       "Stock C", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                       "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock D",
                       "Stock D", "Stock D", "Stock D", "Stock D", "Stock D", "Stock E",
                       "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E",
                       "Stock E", "Stock E", "Stock E", "Stock E", "Stock E", "Stock E",
                       "Stock E", "Stock E", "Stock E"), dates = as.Date(structure(c(984614400,
                                                                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                                                             1003104000, 1005782400, 1008374400, 1011052800, 1013731200, 1016150400,
                                                                             1018828800, 1021420800, 1024099200, 984614400, 987292800, 989884800,
                                                                             992563200, 995155200, 997833600, 1000512000, 1003104000, 1005782400,
                                                                             1008374400, 1011052800, 1013731200, 1016150400, 1018828800, 1021420800,
                                                                             1024099200, 984614400, 987292800, 989884800, 992563200, 995155200,
                                                                             997833600, 1000512000, 1003104000, 1005782400, 1008374400, 1011052800,
                                                                             1013731200, 1016150400, 1018828800, 1021420800, 1024099200, 984614400,
                                                                             987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                                                             1003104000, 1005782400, 1008374400, 1011052800, 1013731200, 1016150400,
                                                                             1018828800, 1021420800, 1024099200, 984614400, 987292800, 989884800,
                                                                             992563200, 995155200, 997833600, 1000512000, 1003104000, 1005782400,
                                                                             1008374400, 1011052800, 1013731200, 1016150400, 1018828800, 1021420800,
                                                                             1024099200), class = c("POSIXct", "POSIXt"), tzone = "UTC"), format = "%Y-%m-%d"),
        fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 8, 2, 9, -30,
                           -15, 2, 9, 1, 8, 2, 3, 5, -1, 35, -152, 3, 10, 1, 2, 28,
                           0, 60, 0, 2, 3, 7, 5, 1, -9, 2, 4, -20, 9, 10, 2, 2, 20,
                           9, 1, 8, 8, 8, 7, 2, -2, -10, -45, -3, 7, 192, -8, 2, -21,
                           21, 2, 5, 1, 8, 1, 2, 1, 4, -5, 0, 8, -20, 22, 15, -30, -12,
                           9),
        fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 9, 10,
                           7, 90, 7, -3, 81, 5, 3, 7, 3, 8, 2, 5, 1, 2, 9, 1, 9, 7,
                           8, 2, 8, 0, 5, 2, 8, 3, 5, 3, 40, 2, 7, 18, 3, 0, -29, 82,
                           2, 1, 3, 8, 3, 1, 1, 11, 4, 2, 7, 1, 7, 3, 94, 1, -3, 9,
                           9, 1, 2, 3, -9, -4, 4, 3, 9, 1, 23, 9, 3, 9, 8),
        fwd_sharpe_1m = c(7, 7, 3, 1, 1, 3, 1, 0, 10, -2, 3, -20, 2, 3, 2, 9, 4, 2, 8,
                          5, 4, 1, 1, 4, -5, 8, -8, -3, 92, 2, 3, -12, 2, 6, 4, 6,
                          5, 1, 1, 5, 3, 9, -29, -18, 8, 39, 0, -8, 4, 9, 0, 10, 1,
                          4, 12, 1, 92, 0, 2, 0, 85, 93, 83, 1, 7, 1, 3, 3, 0, 1, 3,
                          1, 9, 10, 0, -19, 0, 1, 1, 109)), row.names = c(NA, -80L), class = "data.frame"),
      training_sample_size = 8,
      validation_sample_size = 5,
      rebalancing_months = 11,
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "rmse",
      hyper_grid_domain = list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0.1, 0.9, length=10)), #Grid for lambda search
      tuning_method = c("grid_search"),
      ml_algorithm = "glmnet",
      verbose = FALSE,
      show_plots = FALSE
    )}))

  #Define initial objects
  hyper_expanded_grid <- expand.grid(list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0.1, 0.9, length=10)))
  shrinkage.pred_list <- list() #Init list
  validation_eval_hyper_choice <- data.frame(rss =c(NA),  #Validation loss df
                                             cp = c(NA),
                                             rmse = c(NA),
                                             mae = c(NA),
                                             mphe = c(NA),
                                             mpe = c(NA),
                                             row.names = c("2002-03-15"))
  rebalance_dates <- c("2002-03-15")
  n_rebalance_dates <- 1

  chosen_eval_metric_val <- list()

  #Start first rebalancing

  #Get objects to train and validate model
  features_training <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                             "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                             "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                             "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                             "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                             "Stock E-2001-05-15", "Stock A-2001-06-15", "Stock B-2001-06-15",
                                             "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15",
                                             "Stock A-2001-07-15", "Stock B-2001-07-15", "Stock C-2001-07-15",
                                             "Stock D-2001-07-15", "Stock E-2001-07-15"), tickers = c("Stock A",
                                                                                                      "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                                                                                                      "Stock C", "Stock D", "Stock E", "Stock A", "Stock B", "Stock C",
                                                                                                      "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                                                                      "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
                                             ), dates = structure(c(984614400, 984614400, 984614400, 984614400,
                                                                    984614400, 987292800, 987292800, 987292800, 987292800, 987292800,
                                                                    989884800, 989884800, 989884800, 989884800, 989884800, 992563200,
                                                                    992563200, 992563200, 992563200, 992563200, 995155200, 995155200,
                                                                    995155200, 995155200, 995155200), class = c("POSIXct", "POSIXt"
                                                                    ), tzone = "UTC"), Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3,
                                                                                                 -450, 4, 9, 2, -1, 5, 2, -20, -1, 2, -2, 20, -150, -50, -1),
                                      Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3,
                                               3, 1, 1, 2, 1, 13, -12, 1, 5, 2), Gamma = c(800, 9, 10, -9,
                                                                                           3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500, 20, -15, 6, 3, 6,
                                                                                           0, 3, 20, 3, 4)), row.names = c(NA, -25L), class = "data.frame")

  target_training <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                       1, 7, 2, 7, 8, 8, 1, 3, 5, 7, 1, 2, 5, 1, 2, 2),
                                    fwd_premium_3m = c(4,
                                                       5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1, 0, 3, 8, 3, 2, 6, 8,
                                                       3, 1, 3), fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3, 1, 5, 6, 10, 3, 1, 4, 5, 1, 0)), row.names = c(NA, -25L), class = "data.frame")

  features_validation <- structure(list(id = c("Stock A-2001-10-15", "Stock B-2001-10-15",
                                               "Stock C-2001-10-15", "Stock D-2001-10-15", "Stock E-2001-10-15",
                                               "Stock A-2001-11-15", "Stock B-2001-11-15", "Stock C-2001-11-15",
                                               "Stock D-2001-11-15", "Stock E-2001-11-15", "Stock A-2001-12-15",
                                               "Stock B-2001-12-15", "Stock C-2001-12-15", "Stock D-2001-12-15",
                                               "Stock E-2001-12-15"), tickers = c("Stock A", "Stock B", "Stock C",
                                                                                  "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                                                  "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
                                               ), dates = structure(c(1003104000, 1003104000, 1003104000, 1003104000,
                                                                      1003104000, 1005782400, 1005782400, 1005782400, 1005782400, 1005782400,
                                                                      1008374400, 1008374400, 1008374400, 1008374400, 1008374400),
                                                                    class = c("POSIXct",  "POSIXt"), tzone = "UTC"),
                                        Alpha = c(1, -2, 17, 4, 4, -9, -2,  1, 2, 4, 3, 8, -3, 307, 8),
                                        Beta = c(-5, 4, 19, 5, -20, 1, 1, -1, 3, 2, 0, 7, 10, -2, 7),
                                        Gamma = c(3, 3, -4, -1, 1, 27, 7, 105, 4, 31, 9, 2, 30, 1, 5)), row.names = c(NA, -15L), class = "data.frame")

  target_validation <- structure(list(fwd_premium_1m = c(3, -152, 4, -45, -5, 1, 3,
                                                         -20, -3, 0, 8, 10, 9, 7, 8),
                                      fwd_premium_3m = c(-1, 1, 40, 4,
                                                         4, 4, 2, 2, 2, 3, 9, 9, 7, 7, 9), fwd_sharpe_1m = c(0, 4, 5,
                                                                                                             1, 1, 10, -5, 3, 92, 9, -2, 8, 9, 0, 10)), row.names = c(NA, -15L), class = "data.frame")



  #Start first rebalancing
  chosen_eval_metric_val[[1]] <- data.frame(alpha = hyper_expanded_grid$alpha,
                                            lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio,
                                            best_lam = rep(NA,30), chosen_eval_metric = rep(NA, 30))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid)))
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]])
  best_lam <- vector(length =  nrow(hyper_expanded_grid))

  for(s in 1:length(hyper_expanded_grid$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_3m,
      alpha = hyper_expanded_grid$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam[s] <- glm.mod1$lambda[
      which.min(sqrt(colMeans((target_validation$fwd_premium_3m -
                                predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)])))^2)))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam[s])

    #RMSE CHOSEN
    chosen_eval_metric_val[[1]]$chosen_eval_metric[which(chosen_eval_metric_val[[1]]$alpha == unique(hyper_expanded_grid$alpha)[s])] <-
      (sqrt(colMeans((target_validation$fwd_premium_3m -
                   shrinkage.pred_df[,s])^2)))



  }

  chosen_eval_metric_val[[1]]$best_lam <- best_lam


  #RMSE IS MIN: PAY ATTENTION
  hyper_choice1 <- which.min(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(target_validation$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(target_validation$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1)^2*(sqrt(1+((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                         0.5*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                         (1-0.5)*(-1)*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/target_validation$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(target_validation$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice1])))/
    length(target_validation$fwd_premium_3m)

  validation_eval_hyper_choice$mb[1] <- mean(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])



  #Refit
  features_training_and_validation <- features_training_and_validation <-
    structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                          "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                          "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                          "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                          "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                          "Stock E-2001-05-15", "Stock A-2001-06-15", "Stock B-2001-06-15",
                          "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15",
                          "Stock A-2001-07-15", "Stock B-2001-07-15", "Stock C-2001-07-15",
                          "Stock D-2001-07-15", "Stock E-2001-07-15", "Stock A-2001-08-15",
                          "Stock B-2001-08-15", "Stock C-2001-08-15", "Stock D-2001-08-15",
                          "Stock E-2001-08-15", "Stock A-2001-09-15", "Stock B-2001-09-15",
                          "Stock C-2001-09-15", "Stock D-2001-09-15", "Stock E-2001-09-15",
                          "Stock A-2001-10-15", "Stock B-2001-10-15", "Stock C-2001-10-15",
                          "Stock D-2001-10-15", "Stock E-2001-10-15", "Stock A-2001-11-15",
                          "Stock B-2001-11-15", "Stock C-2001-11-15", "Stock D-2001-11-15",
                          "Stock E-2001-11-15", "Stock A-2001-12-15", "Stock B-2001-12-15",
                          "Stock C-2001-12-15", "Stock D-2001-12-15", "Stock E-2001-12-15"
    ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E",
                   "Stock A", "Stock B", "Stock C", "Stock D", "Stock E", "Stock A",
                   "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                   "Stock C", "Stock D", "Stock E", "Stock A", "Stock B", "Stock C",
                   "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                   "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E",
                   "Stock A", "Stock B", "Stock C", "Stock D", "Stock E", "Stock A",
                   "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                   "Stock C", "Stock D", "Stock E"), dates = structure(c(984614400,
                                                                         984614400, 984614400, 984614400, 984614400, 987292800, 987292800,
                                                                         987292800, 987292800, 987292800, 989884800, 989884800, 989884800,
                                                                         989884800, 989884800, 992563200, 992563200, 992563200, 992563200,
                                                                         992563200, 995155200, 995155200, 995155200, 995155200, 995155200,
                                                                         997833600, 997833600, 997833600, 997833600, 997833600, 1000512000,
                                                                         1000512000, 1000512000, 1000512000, 1000512000, 1003104000, 1003104000,
                                                                         1003104000, 1003104000, 1003104000, 1005782400, 1005782400, 1005782400,
                                                                         1005782400, 1005782400, 1008374400, 1008374400, 1008374400, 1008374400,
                                                                         1008374400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
    Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2,
              -1, 5, 2, -20, -1, 2, -2, 20, -150, -50, -1, 1, 1, -20, -25,
              -20, 6, 1, 8, 1, -1, 1, -2, 17, 4, 4, -9, -2, 1, 2, 4, 3,
              8, -3, 307, 8), Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9,
                                       5, 4, -2, 5, 3, 3, 1, 1, 2, 1, 13, -12, 1, 5, 2, 10, -10,
                                       4, 1, 1, 4, 3, 24, 2, -1, -5, 4, 19, 5, -20, 1, 1, -1, 3,
                                       2, 0, 7, 10, -2, 7), Gamma = c(800, 9, 10, -9, 3, 11, -2,
                                                                      -3, 5, 1, 4, 4, 2, 2, -500, 20, -15, 6, 3, 6, 0, 3, 20, 3,
                                                                      4, -523, 4, 12, -10, 405, 2, 4, 13, 0, 0, 3, 3, -4, -1, 1,
                                                                      27, 7, 105, 4, 31, 9, 2, 30, 1, 5)), row.names = c(NA, -50L
                                                                      ), class = "data.frame")

  target_training_and_validation <-
    structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                      1, 7, 2, 7, 8, 8, 1, 3, 5, 7, 1, 2, 5, 1, 2, 2, 1, -1, -9, -2,
                                      1, 10, 35, 2, -10, 4, 3, -152, 4, -45, -5, 1, 3, -20, -3, 0,
                                      8, 10, 9, 7, 8), fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3,
                                                                          9, 2, 7, 2, 8, 1, 0, 3, 8, 3, 2, 6, 8, 3, 1, 3, 5, 2, 5, 1, -9,
                                                                          -5, 5, 3, 11, -4, -1, 1, 40, 4, 4, 4, 2, 2, 2, 3, 9, 9, 7, 7,
                                                                          9), fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4,
                                                                                                0, 3, 1, 5, 6, 10, 3, 1, 4, 5, 1, 0, 3, 1, 1, 4, 1, 1, 1, 1,
                                                                                                12, 3, 0, 4, 5, 1, 1, 10, -5, 3, 92, 9, -2, 8, 9, 0, 10)), row.names = c(NA,
                                                                                                                                                                         -50L), class = "data.frame")


  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice1],
                                  lambda = best_lam[hyper_choice1])
  coef(glm.mod.refit)



  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice1],
                                  lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[hyper_choice1])

  coef(glm.mod.refit, s = best_lam[hyper_choice1])




  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")
  #Pred list
  prediction_list <- list(`2002-03-15` = c(`Stock A` = 4.31812, `Stock B` = 4.31789,
                                           `Stock C` = 4.335536, `Stock D` = 4.32093, `Stock E` = 4.33127
  ), `2002-04-15` = c(`Stock A` = 4.328470191, `Stock B` = 4.323483317,
                      `Stock C` = 4.318047351, `Stock D` = 4.335540625, `Stock E` = 4.363391146
  ), `2002-05-15` = c(`Stock A` = 4.336454692, `Stock B` = 4.297162842,
                      `Stock C` = 4.333986905, `Stock D` = 4.332456303, `Stock E` = 4.320469806

  ), `2002-06-15` = c(`Stock A` = 4.318037606, `Stock B` = 4.318924393,
                      `Stock C` = 4.31523075, `Stock D` = 4.317940781, `Stock E` = 4.292395093

  ))
  results$outputs[[1]] <- prediction_list
  #Error list
  error_list <- list(`2002-03-15` = c(`Stock A` = 85.6800000003359, `Stock B` = 2.68000000033586,
                                      `Stock C` = -4.34, `Stock D` = -1.32,
                                      `Stock E` = 4.67), `2002-04-15` = c(`Stock A` = 2.67,
                                                                                      `Stock B` = 3.68000000033586, `Stock C` = -33.3199999996641,
                                                                                      `Stock D` = 89.6600000003359, `Stock E` = -1.36),
                     `2002-05-15` = c(`Stock A` = -7.34, `Stock B` = -2.30,
                                      `Stock C` = 77.67, `Stock D` = -3.33,
                                      `Stock E` = 4.68000000033586), `2002-06-15` = c(`Stock A` = 76.6800000003359,
                                                                                      `Stock B` = 3.68000000033586, `Stock C` = -2.31999999966414,
                                                                                      `Stock D` = -7.31999999966414, `Stock E` = 3.71000000033586
                                      ))
  results$outputs[[2]] <- error_list
  #Y-list
  y_list <- list(`2002-03-15` = c(`Stock A` = 90, `Stock B` = 7, `Stock C` = 0,
                                  `Stock D` = 3, `Stock E` = 9), `2002-04-15` = c(`Stock A` = 7,
                                                                                  `Stock B` = 8, `Stock C` = -29, `Stock D` = 94, `Stock E` = 3
                                  ), `2002-05-15` = c(`Stock A` = -3, `Stock B` = 2, `Stock C` = 82,
                                                      `Stock D` = 1, `Stock E` = 9), `2002-06-15` = c(`Stock A` = 81,
                                                                                                      `Stock B` = 8, `Stock C` = 2, `Stock D` = -3, `Stock E` = 8))
  results$outputs[[3]] <- y_list
  #Eval metrics
  oos_testing_eval_metrics <-structure(list(rss =c(0.102979487794792, 0.0636603735032917,
                                                         0.101617245923652, 0.109837063555658),
                                            cp = c(94.1759999926782,
                                                             72.06, 78.84, 82.865),
                                            rmse = c(38.4462013729802,42.8373481906648, 35.0030627232721, 34.542356607649),
                                            mae = c(19.7360000000672,26.1360000000672, 19.0639999999328, 18.7360000000672),
                                            mphe = c(18.88444399, 25.27010730, 18.17074927, 17.84555031),
                                            mpe = c(9.868, 13.068, 9.532, 9.368),
                                            mape = c(Inf, 0.67717, 1.678, 1.09333),
                                            hr = c(0.8, 0.8, 0.8, 0.8),
                                            mb = c(17.48, 12.28, 13.88, 14.88)
                                            )
                                       , class = "data.frame", row.names = c("2002-03-15", "2002-04-15", "2002-05-15", "2002-06-15"))
  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(glm.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.0001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }




  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- c("2002-03-15")
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = c("2002-03-15"),
                                     alpha = c(hyper_expanded_grid$alpha[hyper_choice1]),
                                     lambda.min.ratio = c(hyper_expanded_grid$lambda.min.ratio[hyper_choice1]),
                                     best_lam = best_lam[hyper_choice1])

  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL



  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-1
  )

})

#Define your test Excel sheet test glmnet 5
test_that("GLMNET - run_ml_backtest works with rebalancing, 3m target, grid_search as tuning method and rmse as chosen eval metric",{

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
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
             dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400),
                               class = c("POSIXct", "POSIXt"), tzone = "UTC"), format = "%Y-%m-%d"),
             Alpha = c(3, -20, -450, 5, -2, 1,
                       6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                       8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                       -1, 4, 4),
             Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                      -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                      2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
             Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                       -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                       3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame"),
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
                       dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                       format = "%Y-%m-%d"),
                       fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                          2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                       fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                          3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                       fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                         10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame"),

      training_sample_size = 4,
      validation_sample_size = 3,
      rebalancing_months = 11,
      target_fwd_name = c("fwd_premium_3m"),
      parallel = FALSE,
      chosen_eval_metric  = "rmse",
      hyper_grid_domain = list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0.1, 0.9, length=10)), #Grid for lambda search
      tuning_method = c("grid_search"),
      verbose = FALSE,
      ml_algorithm = "glmnet",
      show_plots = FALSE
    )}))

  #Define initial objects
  hyper_expanded_grid <- expand.grid(list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0.1, 0.9, length=10)))
  shrinkage.pred_list <- list() #Init list
  validation_eval_hyper_choice <- data.frame(rss =c(NA, NA),  #Validation loss df
                                             cp = c(NA, NA),
                                             rmse = c(NA, NA),
                                             mae = c(NA, NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             row.names = c("2001-09-15", "2001-11-15"))
  rebalance_dates <- c("2001-09-15", "2001-11-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #Start first rebalancing

  #Get objects to train and validate model
  features_training <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                             "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(984614400, 984614400, 984614400, 984614400,
                         984614400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(3,  1, 2, 5, 5), Beta = c(4, 5, 6, 0, 2), Gamma = c(800, 9, 10, -9, 3)), row.names = c(NA, -5L), class = "data.frame")

  target_training <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5),
                                    fwd_premium_3m = c(4, 5, 0, 1, 9),
                                    fwd_sharpe_1m = c(7, 4, 2, 4, 7)), row.names = c(NA, -5L), class = "data.frame")

  features_validation <- structure(list(id = c("Stock A-2001-06-15", "Stock B-2001-06-15",
                                               "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(992563200, 992563200, 992563200, 992563200,
                         992563200), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(5,  2, -20, -1, 2), Beta = c(3, 1, 1, 2, 1), Gamma = c(20, -15, 6,  3, 6)), row.names = c(NA, -5L), class = "data.frame")

  target_validation <- structure(list(fwd_premium_1m = c(1, 3, 5, 7, 1),
                                      fwd_premium_3m = c(0, 3, 8, 3, 2),
                                      fwd_sharpe_1m = c(1, 5, 6, 10, 3)), row.names = c(NA,  -5L), class = "data.frame")



  #Start first rebalancing
  chosen_eval_metric_val[[1]] <- data.frame(alpha = hyper_expanded_grid$alpha,
                                            lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio,
                                            best_lam = rep(NA,30), chosen_eval_metric = rep(NA, 30))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid)))
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]])
  best_lam1 <- vector(length =  nrow(hyper_expanded_grid))

  for(s in 1:length(hyper_expanded_grid$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_3m,
      alpha = hyper_expanded_grid$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam1[s] <- glm.mod1$lambda[
      which.min(sqrt(colMeans((target_validation$fwd_premium_3m -
                                predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)])))^2)))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam1[s])

    #RMSE CHOSEN
    chosen_eval_metric_val[[1]]$chosen_eval_metric[which(chosen_eval_metric_val[[1]]$alpha == unique(hyper_expanded_grid$alpha)[s])] <-
      (sqrt(mean((target_validation$fwd_premium_3m -
                   shrinkage.pred_df[,s])^2)))



  }

  chosen_eval_metric_val[[1]]$best_lam <- best_lam1

  #RMSE IS MIN: PAY ATTENTION
  hyper_choice1 <- which.min(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(target_validation$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(target_validation$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1)^2*(sqrt(1+((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                         0.5*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                         (1-0.5)*(-1)*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/target_validation$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(target_validation$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice1])))/
    length(target_validation$fwd_premium_3m)

  validation_eval_hyper_choice$mb[1] <- mean(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])


  #Refit
  features_training_and_validation <-structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                                           "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                                           "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                                           "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                                           "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                                           "Stock E-2001-05-15", "Stock A-2001-06-15", "Stock B-2001-06-15",
                                                           "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E",
                 "Stock A", "Stock B", "Stock C", "Stock D", "Stock E", "Stock A",
                 "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                 "Stock C", "Stock D", "Stock E"), dates = structure(c(984614400,
                                                                       984614400, 984614400, 984614400, 984614400, 987292800, 987292800,
                                                                       987292800, 987292800, 987292800, 989884800, 989884800, 989884800,
                                                                       989884800, 989884800, 992563200, 992563200, 992563200, 992563200,
                                                                       992563200), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1, 5, 2, -20, -1, 2), Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3,  3, 1, 1, 2, 1),
  Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500, 20, -15, 6, 3, 6)), row.names = c(NA, -20L), class = "data.frame")

  target_training_and_validation <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                                      1, 7, 2, 7, 8, 8, 1, 3, 5, 7, 1),
                                                   fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1, 0, 3, 8, 3, 2),
                                                   fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3, 1, 5, 6, 10, 3)),
                                              row.names = c(NA, -20L), class = "data.frame")


  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice1],
                                  lambda = best_lam1[hyper_choice1])
  coef(glm.mod.refit)



  #2nd Rebalancing

  #Get objects to train and validate model
  features_training <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                             "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                             "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                             "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                             "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                             "Stock E-2001-05-15"),
                                      tickers = c("Stock A", "Stock B", "Stock C",
                                                  "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                  "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
                                      ),
                                      dates = structure(c(984614400, 984614400, 984614400, 984614400,
                                                          984614400, 987292800, 987292800, 987292800, 987292800, 987292800,
                                                          989884800, 989884800, 989884800, 989884800, 989884800), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                      Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1), Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3),
                                      Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500)), row.names = c(NA, -15L), class = "data.frame")


  target_training <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                       1, 7, 2, 7, 8, 8),
                                    fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1),
                                    fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3)), row.names = c(NA, -15L), class = "data.frame")

  features_validation <- structure(list(id = c("Stock A-2001-08-15", "Stock B-2001-08-15",
                                               "Stock C-2001-08-15", "Stock D-2001-08-15", "Stock E-2001-08-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(997833600, 997833600, 997833600, 997833600,
                         997833600), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(1, 1, -20, -25, -20), Beta = c(10, -10, 4, 1, 1), Gamma = c(-523, 4, 12, -10, 405)), row.names = c(NA, -5L), class = "data.frame")

  target_validation <-structure(list(fwd_premium_1m = c(1, -1, -9, -2, 1),
                                     fwd_premium_3m = c(5, 2, 5, 1, -9),
                                     fwd_sharpe_1m = c(3, 1, 1, 4, 1)), row.names = c(NA,  -5L), class = "data.frame")



  chosen_eval_metric_val[[2]] <- data.frame(alpha = hyper_expanded_grid$alpha,
                                            lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio,
                                            best_lam = rep(NA,30), chosen_eval_metric = rep(NA, 30))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid)))
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]])
  best_lam2 <- vector(length =  nrow(hyper_expanded_grid))

  for(s in 1:length(hyper_expanded_grid$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_3m,
      alpha = hyper_expanded_grid$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam2[s] <- glm.mod1$lambda[
      which.min(sqrt(colMeans((target_validation$fwd_premium_3m -
                                predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)])))^2)))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam2[s])

    #RMSE CHOSEN
    chosen_eval_metric_val[[2]]$chosen_eval_metric[which(chosen_eval_metric_val[[2]]$alpha == unique(hyper_expanded_grid$alpha)[s])] <-
      (sqrt(mean((target_validation$fwd_premium_3m -
                   shrinkage.pred_df[,s])^2)))



  }

  chosen_eval_metric_val[[2]]$best_lam <- best_lam2


  #RMSE IS MIN: PAY ATTENTION
  hyper_choice2 <- which.min(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(target_validation$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(target_validation$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))

  validation_eval_hyper_choice$mphe[2] <- mean((1)^2*(sqrt(1+((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                         0.5*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                         (1-0.5)*(-1)*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs(
    (target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/target_validation$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- length(which(sign(target_validation$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice2])))/
    length(target_validation$fwd_premium_3m)

  validation_eval_hyper_choice$mb[2] <- mean(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])


  #Refit
  features_training_and_validation <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                                            "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                                            "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                                            "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                                            "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                                            "Stock E-2001-05-15", "Stock A-2001-06-15", "Stock B-2001-06-15",
                                                            "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15",
                                                            "Stock A-2001-07-15", "Stock B-2001-07-15", "Stock C-2001-07-15",
                                                            "Stock D-2001-07-15", "Stock E-2001-07-15", "Stock A-2001-08-15",
                                                            "Stock B-2001-08-15", "Stock C-2001-08-15", "Stock D-2001-08-15",
                                                            "Stock E-2001-08-15"),
                                                     tickers = c("Stock A", "Stock B", "Stock C","Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                                 "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E",
                                                                 "Stock A", "Stock B", "Stock C", "Stock D", "Stock E", "Stock A",
                                                                 "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                                                                 "Stock C", "Stock D", "Stock E"),
                                                     dates = structure(c(984614400, 984614400, 984614400, 984614400, 984614400, 987292800, 987292800,
                                                                         987292800, 987292800, 987292800, 989884800, 989884800, 989884800,
                                                                         989884800, 989884800, 992563200, 992563200, 992563200, 992563200,
                                                                         992563200, 995155200, 995155200, 995155200, 995155200, 995155200,
                                                                         997833600, 997833600, 997833600, 997833600, 997833600), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                                     Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1, 5, 2, -20, -1, 2, -2, 20, -150, -50, -1, 1, 1, -20, -25, -20),
                                                     Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3, 3, 1, 1, 2, 1, 13, -12, 1, 5, 2, 10, -10, 4, 1, 1),
                                                     Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500, 20, -15, 6, 3, 6, 0, 3, 20, 3, 4, -523, 4, 12, -10, 405)),
                                                row.names = c(NA, -30L), class = "data.frame")

  target_training_and_validation <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                                      1, 7, 2, 7, 8, 8, 1, 3, 5, 7, 1, 2, 5, 1, 2, 2, 1, -1, -9, -2,
                                                                      1),
                                                   fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1, 0, 3, 8, 3, 2, 6, 8, 3, 1, 3, 5, 2, 5, 1, -9),
                                                   fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3, 1, 5, 6, 10, 3, 1,  4, 5, 1, 0, 3, 1, 1, 4, 1)),
                                              row.names = c(NA, -30L), class = "data.frame")

  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice2],
                                  lambda = best_lam2[hyper_choice2])


  coef(glm.mod.refit)

  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice2],
                                  lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[hyper_choice2])


  coef(glm.mod.refit)

  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")
  #Pred list
  prediction_list <- list(`2001-09-15` = c(`Stock A` = 3.95, `Stock B` = 3.95, `Stock C` = 3.95,
                                           `Stock D` = 3.95, `Stock E` = 3.95), `2001-10-15` = c(`Stock A` = 3.95,
                                                                                                 `Stock B` = 3.95, `Stock C` = 3.95, `Stock D` = 3.95, `Stock E` = 3.95
                                           ), `2001-11-15` = c(`Stock A` = 3.466667, `Stock B` = 3.466667,
                                                               `Stock C` = 3.466667, `Stock D` = 3.466667, `Stock E` = 3.466667
                                           ))
  results$outputs[[1]] <- prediction_list
  #Error list
  error_list <- list(`2001-09-15` = c(`Stock A` = -8.95, `Stock B` = 1.05, `Stock C` = -0.95,
                                      `Stock D` = 7.05, `Stock E` = -7.95),
                     `2001-10-15` = c(`Stock A` = -4.95, `Stock B` = -2.95, `Stock C` = 36.05, `Stock D` = 0.0499999999999998,
                                      `Stock E` = 0.0499999999999998),
                     `2001-11-15` = c(`Stock A` = 0.533333,  `Stock B` = -1.466667, `Stock C` = -1.466667, `Stock D` = -1.466667,  `Stock E` = -0.466667))
  results$outputs[[2]] <- error_list
  #Y-list
  y_list <- list(`2001-09-15` = c(`Stock A` = -5, `Stock B` = 5, `Stock C` = 3,
                                  `Stock D` = 11, `Stock E` = -4), `2001-10-15` = c(`Stock A` = -1,
                                                                                    `Stock B` = 1, `Stock C` = 40, `Stock D` = 4, `Stock E` = 4),
                 `2001-11-15` = c(`Stock A` = 4, `Stock B` = 2, `Stock C` = 2,
                                  `Stock D` = 2, `Stock E` = 3))
  results$outputs[[3]] <- y_list
  #Eval metrics
  oos_testing_eval_metrics <-structure(list(rss =c(0.0050382653061225, 0.184325275397797,
                                                         0.812011933933919), cp = c(7.9, 37.92, 9.0133342),
                                            rmse = c(6.24519815538306, 16.3267418672557, 1.17945397913145
                                            ),
                                            mae = c(5.19, 8.81, 1.0800002),
                                            mphe = c(4.39364382, 8.2462498, 0.51245492),
                                            mpe = c(2.595, 4.405, 0.540),
                                            mape = c(0.98902, 1.76525, 0.49778),
                                            hr = c(0.6, 0.8, 1),
                                            mb = c(-1.95, 5.65, -0.87)

  ),
  class = "data.frame", row.names = c("2001-09-15","2001-10-15", "2001-11-15"))
  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(glm.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.0001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }


  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- c("2001-09-15", "2001-11-15")
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = c("2001-09-15", "2001-11-15"),
                                     alpha = c(hyper_expanded_grid$alpha[hyper_choice1], hyper_expanded_grid$alpha[hyper_choice2]),
                                     lambda.min.ratio = c(hyper_expanded_grid$lambda.min.ratio[hyper_choice1], hyper_expanded_grid$lambda.min.ratio[hyper_choice2]),
                                     best_lam = c(best_lam1[hyper_choice1], best_lam2[hyper_choice1])
                                     )

  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                               "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL



  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-1
  )

})

#Define your test Excel sheet test glmnet 6
test_that("GLMNET - run_ml_backtest works with rebalancing at final, 3m target, grid_search as tuning method and cp as chosen eval metric",{

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
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
             dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400),
                               class = c("POSIXct", "POSIXt"), tzone = "UTC"), format = "%Y-%m-%d"),
             Alpha = c(3, -20, -450, 5, -2, 1,
                       6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                       8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                       -1, 4, 4),
             Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                      -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                      2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
             Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                       -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                       3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame"),
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
                       dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                       format = "%Y-%m-%d"),
                       fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                          2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                       fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                          3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                       fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                         10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame"),
      training_sample_size = 4,
      validation_sample_size = 3,
      rebalancing_months = 11,
      ml_algorithm = "glmnet",
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "cp",
      hyper_grid_domain = list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0.1, 0.9, length=10)), #Grid for lambda search
      tuning_method = c("grid_search"),
      verbose = FALSE,
      show_plots = FALSE
    )}))

  #Define initial objects
  hyper_expanded_grid <- expand.grid(list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0.1, 0.9, length=10)))
  shrinkage.pred_list <- list() #Init list
  validation_eval_hyper_choice <- data.frame(rss =c(NA, NA),  #Validation loss df
                                             cp = c(NA, NA),
                                             rmse = c(NA, NA),
                                             mae = c(NA, NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             row.names = c("2001-09-15", "2001-11-15"))
  rebalance_dates <- c("2001-09-15", "2001-11-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #Start first rebalancing

  #Get objects to train and validate model
  features_training <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                             "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(984614400, 984614400, 984614400, 984614400,
                         984614400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(3,  1, 2, 5, 5), Beta = c(4, 5, 6, 0, 2), Gamma = c(800, 9, 10, -9, 3)), row.names = c(NA, -5L), class = "data.frame")

  target_training <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5),
                                    fwd_premium_3m = c(4, 5, 0, 1, 9),
                                    fwd_sharpe_1m = c(7, 4, 2, 4, 7)), row.names = c(NA, -5L), class = "data.frame")

  features_validation <- structure(list(id = c("Stock A-2001-06-15", "Stock B-2001-06-15",
                                               "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(992563200, 992563200, 992563200, 992563200,
                         992563200), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(5,  2, -20, -1, 2), Beta = c(3, 1, 1, 2, 1), Gamma = c(20, -15, 6,  3, 6)), row.names = c(NA, -5L), class = "data.frame")

  target_validation <- structure(list(fwd_premium_1m = c(1, 3, 5, 7, 1),
                                      fwd_premium_3m = c(0, 3, 8, 3, 2),
                                      fwd_sharpe_1m = c(1, 5, 6, 10, 3)), row.names = c(NA,  -5L), class = "data.frame")



  #Start first rebalancing
  chosen_eval_metric_val[[1]] <- data.frame(alpha = hyper_expanded_grid$alpha,
                                            lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio,
                                            best_lam = rep(NA,30), chosen_eval_metric = rep(NA, 30))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid)))
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]])
  best_lam1 <- vector(length =  nrow(hyper_expanded_grid))

  for(s in 1:length(hyper_expanded_grid$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_3m,
      alpha = hyper_expanded_grid$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam1[s] <- glm.mod1$lambda[
      which.max(colMeans(target_validation$fwd_premium_3m *
                                predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]))))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam1[s])

    #CP CHOSEN
    chosen_eval_metric_val[[1]]$chosen_eval_metric[which(chosen_eval_metric_val[[1]]$alpha == unique(hyper_expanded_grid$alpha)[s])] <-
      (mean((target_validation$fwd_premium_3m *
                   shrinkage.pred_df[,s])))


  }

  chosen_eval_metric_val[[1]]$best_lam <- best_lam1

  #CP IS MAX: PAY ATTENTION
  hyper_choice1 <- which.max(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(target_validation$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(target_validation$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1)^2*(sqrt(1+((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                         0.5*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                         (1-0.5)*(-1)*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/target_validation$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(target_validation$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice1])))/
    length(target_validation$fwd_premium_3m)

  validation_eval_hyper_choice$mb[1] <- mean(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])


  #Refit
  features_training_and_validation <-structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                                           "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                                           "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                                           "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                                           "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                                           "Stock E-2001-05-15", "Stock A-2001-06-15", "Stock B-2001-06-15",
                                                           "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E",
                 "Stock A", "Stock B", "Stock C", "Stock D", "Stock E", "Stock A",
                 "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                 "Stock C", "Stock D", "Stock E"), dates = structure(c(984614400,
                                                                       984614400, 984614400, 984614400, 984614400, 987292800, 987292800,
                                                                       987292800, 987292800, 987292800, 989884800, 989884800, 989884800,
                                                                       989884800, 989884800, 992563200, 992563200, 992563200, 992563200,
                                                                       992563200), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1, 5, 2, -20, -1, 2), Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3,  3, 1, 1, 2, 1),
  Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500, 20, -15, 6, 3, 6)), row.names = c(NA, -20L), class = "data.frame")

  target_training_and_validation <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                                      1, 7, 2, 7, 8, 8, 1, 3, 5, 7, 1),
                                                   fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1, 0, 3, 8, 3, 2),
                                                   fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3, 1, 5, 6, 10, 3)),
                                              row.names = c(NA, -20L), class = "data.frame")


  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice1],
                                  lambda = hyper_expanded_grid$lambda[hyper_choice1])
  coef(glm.mod.refit)

  #2nd Rebalancing

  #Get objects to train and validate model
  features_training <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                             "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                             "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                             "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                             "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                             "Stock E-2001-05-15"),
                                      tickers = c("Stock A", "Stock B", "Stock C",
                                                  "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                  "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
                                      ),
                                      dates = structure(c(984614400, 984614400, 984614400, 984614400,
                                                          984614400, 987292800, 987292800, 987292800, 987292800, 987292800,
                                                          989884800, 989884800, 989884800, 989884800, 989884800), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                      Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1), Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3),
                                      Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500)), row.names = c(NA, -15L), class = "data.frame")


  target_training <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                       1, 7, 2, 7, 8, 8),
                                    fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1),
                                    fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3)), row.names = c(NA, -15L), class = "data.frame")

  features_validation <- structure(list(id = c("Stock A-2001-08-15", "Stock B-2001-08-15",
                                               "Stock C-2001-08-15", "Stock D-2001-08-15", "Stock E-2001-08-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(997833600, 997833600, 997833600, 997833600,
                         997833600), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(1, 1, -20, -25, -20), Beta = c(10, -10, 4, 1, 1), Gamma = c(-523, 4, 12, -10, 405)), row.names = c(NA, -5L), class = "data.frame")

  target_validation <-structure(list(fwd_premium_1m = c(1, -1, -9, -2, 1),
                                     fwd_premium_3m = c(5, 2, 5, 1, -9),
                                     fwd_sharpe_1m = c(3, 1, 1, 4, 1)), row.names = c(NA,  -5L), class = "data.frame")



  chosen_eval_metric_val[[2]] <- data.frame(alpha = hyper_expanded_grid$alpha,
                                            lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio,
                                            best_lam = rep(NA,30), chosen_eval_metric = rep(NA, 30))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid)))
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]])
  best_lam2 <- vector(length =  nrow(hyper_expanded_grid))

  for(s in 1:length(hyper_expanded_grid$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_3m,
      alpha = hyper_expanded_grid$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam2[s] <- glm.mod1$lambda[
      which.max(colMeans((target_validation$fwd_premium_3m *
                                predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)])))))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam2[s])

    #CP CHOSEN
    chosen_eval_metric_val[[2]]$chosen_eval_metric[which(chosen_eval_metric_val[[2]]$alpha == unique(hyper_expanded_grid$alpha)[s])] <-
      (mean((target_validation$fwd_premium_3m *
                   shrinkage.pred_df[,s])))


  }

  chosen_eval_metric_val[[2]]$best_lam <- best_lam2
  #CP IS MAX: PAY ATTENTION
  hyper_choice2 <- which.max(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(target_validation$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((target_validation$fwd_premium_3m - shrinkage.pred_df[[hyper_choice2]])^2))

  validation_eval_hyper_choice$cp[2] <- mean(target_validation$fwd_premium_3m*shrinkage.pred_df[[hyper_choice2]])

  validation_eval_hyper_choice$mae[2] <- mean(abs(target_validation$fwd_premium_3m - shrinkage.pred_df[[hyper_choice2]]))

  validation_eval_hyper_choice$mphe[2] <- mean((1)^2*(sqrt(1+((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                         0.5*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                         (1-0.5)*(-1)*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs(
    (target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/target_validation$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- length(which(sign(target_validation$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice2])))/
    length(target_validation$fwd_premium_3m)

  validation_eval_hyper_choice$mb[2] <- mean(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])


  #Refit
  features_training_and_validation <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                                            "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                                            "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                                            "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                                            "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                                            "Stock E-2001-05-15", "Stock A-2001-06-15", "Stock B-2001-06-15",
                                                            "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15",
                                                            "Stock A-2001-07-15", "Stock B-2001-07-15", "Stock C-2001-07-15",
                                                            "Stock D-2001-07-15", "Stock E-2001-07-15", "Stock A-2001-08-15",
                                                            "Stock B-2001-08-15", "Stock C-2001-08-15", "Stock D-2001-08-15",
                                                            "Stock E-2001-08-15"),
                                                     tickers = c("Stock A", "Stock B", "Stock C","Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                                 "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E",
                                                                 "Stock A", "Stock B", "Stock C", "Stock D", "Stock E", "Stock A",
                                                                 "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                                                                 "Stock C", "Stock D", "Stock E"),
                                                     dates = structure(c(984614400, 984614400, 984614400, 984614400, 984614400, 987292800, 987292800,
                                                                         987292800, 987292800, 987292800, 989884800, 989884800, 989884800,
                                                                         989884800, 989884800, 992563200, 992563200, 992563200, 992563200,
                                                                         992563200, 995155200, 995155200, 995155200, 995155200, 995155200,
                                                                         997833600, 997833600, 997833600, 997833600, 997833600), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                                     Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1, 5, 2, -20, -1, 2, -2, 20, -150, -50, -1, 1, 1, -20, -25, -20),
                                                     Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3, 3, 1, 1, 2, 1, 13, -12, 1, 5, 2, 10, -10, 4, 1, 1),
                                                     Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500, 20, -15, 6, 3, 6, 0, 3, 20, 3, 4, -523, 4, 12, -10, 405)),
                                                row.names = c(NA, -30L), class = "data.frame")

  target_training_and_validation <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                                      1, 7, 2, 7, 8, 8, 1, 3, 5, 7, 1, 2, 5, 1, 2, 2, 1, -1, -9, -2,
                                                                      1),
                                                   fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1, 0, 3, 8, 3, 2, 6, 8, 3, 1, 3, 5, 2, 5, 1, -9),
                                                   fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3, 1, 5, 6, 10, 3, 1,  4, 5, 1, 0, 3, 1, 1, 4, 1)),
                                              row.names = c(NA, -30L), class = "data.frame")

  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice2],
                                  lambda = best_lam2[hyper_choice2])


  coef(glm.mod.refit)

  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice2],
                                  lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[hyper_choice2])


  coef(glm.mod.refit)



  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")
  #Pred list
  prediction_list <- list(`2001-09-15` = c(`Stock A` = 3.95, `Stock B` = 3.95, `Stock C` = 3.95,
                                           `Stock D` = 3.95, `Stock E` = 3.95), `2001-10-15` = c(`Stock A` = 3.95,
                                                                                                 `Stock B` = 3.95, `Stock C` = 3.95, `Stock D` = 3.95, `Stock E` = 3.95
                                           ), `2001-11-15` = c(`Stock A` = 3.466667, `Stock B` = 3.466667,
                                                               `Stock C` = 3.466667, `Stock D` = 3.466667, `Stock E` = 3.466667
                                           ))
  results$outputs[[1]] <- prediction_list
  #Error list
  error_list <- list(`2001-09-15` = c(`Stock A` = -8.95, `Stock B` = 1.05, `Stock C` = -0.95,
                                      `Stock D` = 7.05, `Stock E` = -7.95), `2001-10-15` = c(`Stock A` = -4.95,
                                                                                             `Stock B` = -2.95, `Stock C` = 36.05, `Stock D` = 0.0499999999999998,
                                                                                             `Stock E` = 0.0499999999999998), `2001-11-15` = c(`Stock A` = 0.533333,
                                                                                                                                               `Stock B` = -1.466667, `Stock C` = -1.466667, `Stock D` = -1.466667,
                                                                                                                                               `Stock E` = -0.466667))
  results$outputs[[2]] <- error_list
  #Y-list
  y_list <- list(`2001-09-15` = c(`Stock A` = -5, `Stock B` = 5, `Stock C` = 3,
                                  `Stock D` = 11, `Stock E` = -4), `2001-10-15` = c(`Stock A` = -1,
                                                                                    `Stock B` = 1, `Stock C` = 40, `Stock D` = 4, `Stock E` = 4),
                 `2001-11-15` = c(`Stock A` = 4, `Stock B` = 2, `Stock C` = 2,
                                  `Stock D` = 2, `Stock E` = 3))
  results$outputs[[3]] <- y_list
  #Eval metrics
  oos_testing_eval_metrics <-structure(list(rss =c(0.0050382653061225, 0.184325275397797,
                                                         0.812011933933919), cp = c(7.9, 37.92, 9.0133342),
                                            rmse = c(6.24519815538306, 16.3267418672557, 1.17945397913145
                                            ),
                                            mae = c(5.19, 8.81, 1.0800002),
                                            mphe = c(4.39364382, 8.2462498, 0.51245492),
                                            mpe = c(2.595, 4.405, 0.54),
                                            mape = c(0.98902, 1.76525, 0.49778),
                                            hr = c(0.6, 0.8, 1),
                                            mb = c(-1.95, 5.65, -0.87)

  ), class = "data.frame", row.names = c("2001-09-15","2001-10-15", "2001-11-15"))
  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(glm.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.0001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }


  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- c("2001-09-15", "2001-11-15")
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = c("2001-09-15", "2001-11-15"),
                                     alpha = c(hyper_expanded_grid$alpha[hyper_choice1], hyper_expanded_grid$alpha[hyper_choice2]),
                                     lambda.min.ratio = c(hyper_expanded_grid$lambda.min.ratio[hyper_choice1], hyper_expanded_grid$lambda.min.ratio[hyper_choice2]),
                                     best_lam = c(best_lam1[hyper_choice1], best_lam2[hyper_choice2]))

  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-3
  )

})

#Define your test
test_that("RF (Parallel) - run_ml_backtest works with rebalancing, 3m target, grid as tuning method and hr as chosen eval metric -toy_preprocessed_features_and_targets",{

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))
  future::plan("multisession")

  set.seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      ml_algorithm = "rf",
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "hr",
      hyper_grid_domain = list(mtry = c(0, 0.5, 1), num.trees = c(200, 500),
                                    max.depth = c(2, 4, 6), min.bucket = c(1, 5, 10)),
      tuning_method = c("grid_search"),
      verbose = FALSE,
      show_plots = FALSE
    )}))



  #Define initial objects
  hyper_expanded_grid <- expand.grid(list(mtry = c(0, 0.5, 1), num.trees = c(200, 500),
                                          max.depth = c(2, 4, 6), min.bucket = c(1, 5, 10)))


  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]
  #Full data
  full_data_first_train <- cbind(targets_first_train$fwd_premium_3m, features_first_train[,-c(1:3)])
  colnames(full_data_first_train)[1] <- c("fwd_premium_3m")

  #Features val
  chosen_eval_metric_val[[1]] <- data.frame(mtry = hyper_expanded_grid$mtry, num.trees = hyper_expanded_grid$num.trees,
                                              max.depth = hyper_expanded_grid$max.depth, min.bucket = hyper_expanded_grid$min.bucket,
                                              chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid)))


  set.seed(123)

  #Use foreach to simulate result of parallelized hyper tuning
  first_rebal <-
    foreach::foreach(s = 1:nrow(hyper_expanded_grid), .options.future = list(seed = TRUE)) %dofuture% {
      #Train Model
      rf.mod1 <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_first_train),
                                mtry = hyper_expanded_grid$mtry[s] * (ncol(full_data_first_train) - 1),
                                num.trees = hyper_expanded_grid$num.trees[s],
                                max.depth = hyper_expanded_grid$max.depth[s],
                                min.bucket = hyper_expanded_grid$min.bucket[s]
      )

      out <- data.frame(matrix(NA, nrow = length(targets_first_val$fwd_premium_3m),
                               ncol = nrow(hyper_expanded_grid)))


      #Predict vlidation data
      out[,s] <-
        stats::predict(rf.mod1, data = janitor::clean_names(features_first_val[,-c(1:3)]))$predictions


      #HR CHOSEN
      return(list(predictions = out[,s],
                  metric = mean(out[,s] * targets_first_val$fwd_premium_3m > 0)))

    }


  #Pass objects
  shrinkage.pred_df <- sapply(first_rebal, function(x) as.numeric(x$predictions)) #Transform to df
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]]) #chance colnames

  chosen_eval_metric_val[[1]]$chosen_eval_metric <- sapply(first_rebal, function(x) x$metric)


  #rsquared IS MAX: PAY ATTENTION
  hyper_choice1 <- which.max(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(targets_first_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(targets_first_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1)^2*(sqrt(1+((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                         0.5*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                         (1-0.5)*(-1)*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/targets_first_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(targets_first_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice1])))/
    length(targets_first_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[1] <- mean(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])




  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]

  #Full data
  full_data_first_training_and_validation <- cbind(target_first_training_and_validation$fwd_premium_3m, features_first_training_and_validation[,-c(1:3)])
  colnames(full_data_first_training_and_validation)[1] <- c("fwd_premium_3m")

  #Refitted model
  rf.mod.refit <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_first_training_and_validation),
                                 mtry = hyper_expanded_grid$mtry[hyper_choice1] * (ncol(full_data_first_training_and_validation) - 1),
                                 num.trees = hyper_expanded_grid$num.trees[hyper_choice1],
                                 max.depth = hyper_expanded_grid$max.depth[hyper_choice1],
                                 min.bucket = hyper_expanded_grid$min.bucket[hyper_choice1])


  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(predict(rf.mod.refit, data = janitor::clean_names(features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)]))$predictions)
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  prediction_list[[2]] <- as.numeric(predict(rf.mod.refit, data = janitor::clean_names(features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)]))$predictions)
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]

  #Full data
  full_data_second_train <- cbind(targets_second_train$fwd_premium_3m, features_second_train[,-c(1:3)])
  colnames(full_data_second_train)[1] <- c("fwd_premium_3m")

  #Features val
  chosen_eval_metric_val[[2]] <- data.frame(mtry = hyper_expanded_grid$mtry, num.trees = hyper_expanded_grid$num.trees,
                                              max.depth = hyper_expanded_grid$max.depth, min.bucket = hyper_expanded_grid$min.bucket,
                                              chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid)))

  #Use foreach to simulate result of parallelized hyper tuning
  second_rebal <-
    foreach::foreach(s = 1:nrow(hyper_expanded_grid), .options.future = list(seed = TRUE)) %dofuture% {
      #Train Model
      rf.mod2 <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_second_train),
                                mtry = hyper_expanded_grid$mtry[s] * (ncol(full_data_second_train) - 1),
                                num.trees = hyper_expanded_grid$num.trees[s],
                                max.depth = hyper_expanded_grid$max.depth[s],
                                min.bucket = hyper_expanded_grid$min.bucket[s]
      )

      out <- data.frame(matrix(NA, nrow = length(targets_second_val$fwd_premium_3m),
                               ncol = nrow(hyper_expanded_grid)))


      #Predict vlidation data
      out[,s] <-
        stats::predict(rf.mod2, data = janitor::clean_names(features_second_val[,-c(1:3)]))$predictions


      #HR CHOSEN
      return(list(predictions = out[,s],
                  metric = mean(out[,s] * targets_second_val$fwd_premium_3m > 0)))

    }


  #Pass objects
  shrinkage.pred_df <- sapply(second_rebal, function(x) as.numeric(x$predictions)) #Transform to df
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]]) #chance colnames

  chosen_eval_metric_val[[2]]$chosen_eval_metric <- sapply(second_rebal, function(x) x$metric)

  #old option for sequential implementation
  #for(s in 1:nrow(hyper_expanded_grid)){
  #Train Model
  #rf.mod2 <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_second_train),
  #                          mtry = hyper_expanded_grid$mtry[s] * (ncol(full_data_second_train) - 1),
  #                          num.trees = hyper_expanded_grid$num.trees[s],
  #                          max.depth = hyper_expanded_grid$max.depth[s],
  #                          min.bucket = hyper_expanded_grid$min.bucket[s])

  #Predict to validation data

  #shrinkage.pred_df[,s] <-
  # stats::predict(rf.mod2, data = janitor::clean_names(features_second_val[,-c(1:3)]))$predictions


  #CROSSPRODUCT CHOSEN
  #chosen_eval_metric_val[[2]]$chosen_eval_metric[s] <- mean(shrinkage.pred_df[,s] * targets_second_val$fwd_premium_3m)

  #}



  #HR IS MAX: PAY ATTENTION
  hyper_choice2 <- which.max(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(targets_second_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(targets_second_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))

  validation_eval_hyper_choice$mphe[2] <- mean((1)^2*(sqrt(1+((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                         0.5*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                         (1-0.5)*(-1)*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs(
    (targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/targets_second_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- length(which(sign(targets_second_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice2])))/
    length(targets_second_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[2] <- mean(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])


  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]

  #Full data
  full_data_second_training_and_validation <- cbind(target_second_training_and_validation$fwd_premium_3m, features_second_training_and_validation[,-c(1:3)])
  colnames(full_data_second_training_and_validation)[1] <- c("fwd_premium_3m")


  #Refitted model
  rf.mod.refit <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_second_training_and_validation),
                                 mtry = hyper_expanded_grid$mtry[hyper_choice2] * (ncol(full_data_second_training_and_validation) - 1),
                                 num.trees = hyper_expanded_grid$num.trees[hyper_choice2],
                                 max.depth = hyper_expanded_grid$max.depth[hyper_choice2],
                                 min.bucket = hyper_expanded_grid$min.bucket[hyper_choice2])



  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]



  #Predict!
  prediction_list[[3]] <- as.numeric(predict(rf.mod.refit, data = janitor::clean_names(features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)]))$predictions)
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  prediction_list[[4]] <- as.numeric(predict(rf.mod.refit, data = janitor::clean_names(features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)]))$predictions)
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss =c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA), row.names =   c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(1^2*(sqrt(1+(y_list[[l]] - prediction_list[[l]])^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0,
                                                       0.5*(y_list[[l]] - prediction_list[[l]]),
                                                       (1-0.5)*(-1)*(y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))
    oos_testing_eval_metrics$hr[l] <- mean((y_list[[l]] * prediction_list[[l]])>0)
    oos_testing_eval_metrics$mb[l] <- mean(y_list[[l]] - prediction_list[[l]])



  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(rf.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.0001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }


  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     mtry = c(hyper_expanded_grid$mtry[hyper_choice1], hyper_expanded_grid$mtry[hyper_choice2]),
                                     num.trees = c(hyper_expanded_grid$num.trees[hyper_choice1], hyper_expanded_grid$num.trees[hyper_choice2]),
                                     max.depth = c(hyper_expanded_grid$max.depth[hyper_choice1], hyper_expanded_grid$max.depth[hyper_choice2]),
                                     min.bucket = c(hyper_expanded_grid$min.bucket[hyper_choice1], hyper_expanded_grid$min.bucket[hyper_choice2]))


  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL

  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-5
  )

  future::plan("sequential")

})


#Define your test
test_that("XGB (Parallel) - run_ml_backtest works with rebalancing, 3m target, grid as tuning method, pseudo_huber (not mentioned) as chosen eval metric and custom_objective pseudo huber error -toy_preprocessed_features_and_targets",{

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  suppressWarnings({
    future::plan("multisession")
  })

  set.seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      ml_algorithm = "xgb",
      custom_objective = "pseudo_huber_error",
      target_fwd_name = c("fwd_premium_3m"),
      hyper_grid_domain = list(min_child_weight = c(1), max_depth = c(3, 6),
                                    subsample = c(0.50, 0.75), colsample_bytree = c(0.50, 1),
                                    eta = c(0.05, 0.1), alpha = c(2, 5), gamma = c(0), nrounds = c(500)),
      tuning_method = c("grid_search"),
      huber_delta = 1.3,
      early_stop = 25,
      verbose = FALSE,
      show_plots = FALSE
    )}))



  #Define initial objects
  hyper_expanded_grid <- expand.grid(list(min_child_weight = c(1), max_depth = c(3, 6),
                                          subsample = c(0.50, 0.75), colsample_bytree = c(0.50, 1),
                                          eta = c(0.05, 0.1), alpha = c(2, 5), gamma = c(0), nrounds = c(500)))


  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]
  #Full data
  full_data_first_train <- cbind(targets_first_train$fwd_premium_3m, features_first_train[,-c(1:3)])
  colnames(full_data_first_train)[1] <- c("fwd_premium_3m")


  set.seed(123)

  #Use foreach to simulate result of parallelized hyper tuning
  first_rebal <-
    foreach::foreach(s = 1:nrow(hyper_expanded_grid), .options.future = list(seed = TRUE)) %dofuture% {

      #Create xgb.DMatrix object
      full_data_xgb_first_train <- xgboost::xgb.DMatrix(data = as.matrix(features_first_train[,-c(1:3)]),
                                                        label = targets_first_train$fwd_premium_3m)

      full_data_xgb_first_val <- xgboost::xgb.DMatrix(data = as.matrix(features_first_val[,-c(1:3)]),
                                                        label = targets_first_val$fwd_premium_3m)


      #Train Model
      xgb.mod1 <- xgboost::xgb.train(data = full_data_xgb_first_train,
                                    eta = hyper_expanded_grid$eta[s],
                                    min_child_weight = hyper_expanded_grid$min_child_weight[s],
                                    max_depth = hyper_expanded_grid$max_depth[s],
                                    nrounds = hyper_expanded_grid$nrounds[s],
                                    subsample = hyper_expanded_grid$subsample[s],
                                    colsample_bytree = hyper_expanded_grid$colsample_bytree[s],
                                    alpha = hyper_expanded_grid$alpha[s],
                                    early_stopping_rounds = 25,
                                    print_every_n = 500,
                                    gamma = hyper_expanded_grid$gamma[s],
                                    objective = "reg:pseudohubererror",
                                    huber_slope = 1.3,
                                    eval_metric = "mphe",
                                    verbose = 0,
                                    watchlist = (list(train = full_data_xgb_first_train,
                                                   validation = full_data_xgb_first_val))



      )

      out <- data.frame(matrix(NA, nrow = length(targets_first_val$fwd_premium_3m),
                               ncol = nrow(hyper_expanded_grid)))

      best_iteration <- vector(length = nrow(hyper_expanded_grid))


      #Predict vlidation data
      out[,s] <-
        stats::predict(xgb.mod1, newdata = as.matrix(features_first_val[,-c(1:3)]))

      best_iteration[s] <- xgb.mod1$best_iteration

      #PSEUDO HUBER CHOSEN
      return(list(predictions = out[,s],
                  metric = mean(1.3^2 * (sqrt(1+ ((targets_first_val$fwd_premium_3m - out[,s])/1.3)^2) - 1)),
                  best_iteration = best_iteration
      ))

    }

  #Features val
  chosen_eval_metric_val[[1]] <- data.frame(min_child_weight = hyper_expanded_grid$min_child_weight, max_depth = hyper_expanded_grid$max_depth,
                                              subsample = hyper_expanded_grid$subsample, colsample_bytree = hyper_expanded_grid$colsample_bytree,
                                              eta = hyper_expanded_grid$eta, alpha = hyper_expanded_grid$alpha, gamma = hyper_expanded_grid$gamma,
                                              nrounds = hyper_expanded_grid$nrounds,
                                              best_iteration = sapply(first_rebal, function(x) x$best_iteration) %>% diag(),
                                              chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid)))

  #Pass objects
  shrinkage.pred_df <- sapply(first_rebal, function(x) as.numeric(x$predictions)) #Transform to df
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]]) #chance colnames

  chosen_eval_metric_val[[1]]$chosen_eval_metric <- sapply(first_rebal, function(x) x$metric)


  #PSEUDO HUBER IS MIN: PAY ATTENTION
  hyper_choice1 <- which.min(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(targets_first_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(targets_first_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1.3)^2*(sqrt(1+((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                          (1.3))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                         0.5*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                         (1-0.5)*(-1)*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/targets_first_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(targets_first_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice1])))/
    length(targets_first_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[1] <- mean(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])


  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]

  #Full data
  full_data_first_training_and_validation <- cbind(target_first_training_and_validation$fwd_premium_3m, features_first_training_and_validation[,-c(1:3)])
  colnames(full_data_first_training_and_validation)[1] <- c("fwd_premium_3m")

  #Refitted model
  xgb.mod.refit <- xgboost::xgb.train(data = xgboost::xgb.DMatrix(data = as.matrix(features_first_training_and_validation[,-c(1:3)]),
                                                                  label = target_first_training_and_validation$fwd_premium_3m),
                                      objective = "reg:pseudohubererror",
                                      huber_slope = 1.3,
                                      min_child_weight = hyper_expanded_grid$min_child_weight[hyper_choice1],
                                      max_depth = hyper_expanded_grid$max_depth[hyper_choice1],
                                      subsample = hyper_expanded_grid$subsample[hyper_choice1],
                                      colsample_bytree = hyper_expanded_grid$colsample_bytree[hyper_choice1],
                                      eta = hyper_expanded_grid$eta[hyper_choice1],
                                      alpha = hyper_expanded_grid$alpha[hyper_choice1],
                                      gamma = hyper_expanded_grid$gamma[hyper_choice1],
                                      nrounds = chosen_eval_metric_val[[1]]$best_iteration[hyper_choice1]
                                      )




  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(predict(xgb.mod.refit, newdata = as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)])))
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  prediction_list[[2]] <- as.numeric(predict(xgb.mod.refit, newdata = as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)])))
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]

  #Full data
  full_data_second_train <- cbind(targets_second_train$fwd_premium_3m, features_second_train[,-c(1:3)])
  colnames(full_data_second_train)[1] <- c("fwd_premium_3m")


  #Second val

  #Use foreach to simulate result of parallelized hyper tuning
  second_rebal <-
    foreach::foreach(s = 1:nrow(hyper_expanded_grid), .options.future = list(seed = TRUE)) %dofuture% {

      #Create xgb.DMatrix object
      full_data_xgb_second_train <- xgboost::xgb.DMatrix(data = as.matrix(features_second_train[,-c(1:3)]),
                                                         label = targets_second_train$fwd_premium_3m)

      full_data_xgb_second_val <- xgboost::xgb.DMatrix(data = as.matrix(features_second_val[,-c(1:3)]),
                                                      label = targets_second_val$fwd_premium_3m)

      #Train Model
      xgb.mod2 <- xgboost::xgb.train(data = full_data_xgb_second_train,
                                    eta = hyper_expanded_grid$eta[s],
                                    min_child_weight = hyper_expanded_grid$min_child_weight[s],
                                    max_depth = hyper_expanded_grid$max_depth[s],
                                    nrounds = hyper_expanded_grid$nrounds[s],
                                    subsample = hyper_expanded_grid$subsample[s],
                                    colsample_bytree = hyper_expanded_grid$colsample_bytree[s],
                                    alpha = hyper_expanded_grid$alpha[s],
                                    gamma = hyper_expanded_grid$gamma[s],
                                    objective = "reg:pseudohubererror",
                                    eval_metric = "mphe",
                                    huber_slope = 1.3,
                                    early_stopping_rounds = 25,
                                    verbose = 0,
                                    watchlist = (list(train = full_data_xgb_second_train,
                                                      validation = full_data_xgb_second_val))



      )

      out <- data.frame(matrix(NA, nrow = length(targets_second_val$fwd_premium_3m),
                               ncol = nrow(hyper_expanded_grid)))

      best_iteration <- vector(length = nrow(hyper_expanded_grid))


      #Predict vlidation data
      out[,s] <-
        stats::predict(xgb.mod2, newdata = as.matrix(features_second_val[,-c(1:3)]))

      best_iteration[s] <- xgb.mod2$best_iteration


      #PSEUDO HUBER CHOSEN
      return(list(predictions = out[,s],
                  metric = mean(1.3^2 * (sqrt(1+ ((targets_second_val$fwd_premium_3m - out[,s])/1.3)^2) - 1)),
                  best_iteration = best_iteration
      ))

    }


  #Pass objects
  chosen_eval_metric_val[[2]] <- data.frame(min_child_weight = hyper_expanded_grid$min_child_weight, max_depth = hyper_expanded_grid$max_depth,
                                              subsample = hyper_expanded_grid$subsample, colsample_bytree = hyper_expanded_grid$colsample_bytree,
                                              eta = hyper_expanded_grid$eta, alpha = hyper_expanded_grid$alpha, gamma = hyper_expanded_grid$gamma,
                                              nrounds = hyper_expanded_grid$nrounds,
                                              best_iteration = sapply(second_rebal, function(x) x$best_iteration) %>% diag(),
                                              chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid)))


  shrinkage.pred_df <- sapply(second_rebal, function(x) as.numeric(x$predictions)) #Transform to df
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]]) #chance colnames

  chosen_eval_metric_val[[2]]$chosen_eval_metric <- sapply(second_rebal, function(x) x$metric)


  #PSEUDO HUBER IS MIN: PAY ATTENTION
  hyper_choice2 <- which.min(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(targets_second_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(targets_second_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))

  validation_eval_hyper_choice$mphe[2] <- mean((1.3)^2*(sqrt(1+((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                          (1.3))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                         0.5*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                         (1-0.5)*(-1)*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs(
    (targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/targets_second_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- length(which(sign(targets_second_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice2])))/
    length(targets_second_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[2] <- mean(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])


  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]

  #Full data
  full_data_second_training_and_validation <- cbind(target_second_training_and_validation$fwd_premium_3m, features_second_training_and_validation[,-c(1:3)])
  colnames(full_data_second_training_and_validation)[1] <- c("fwd_premium_3m")

  #Refitted model
  xgb.mod.refit <- xgboost::xgb.train(data = xgboost::xgb.DMatrix(data = as.matrix(features_second_training_and_validation[,-c(1:3)]),
                                                                  label = target_second_training_and_validation$fwd_premium_3m),
                                      objective = "reg:pseudohubererror",
                                      huber_slope = 1.3,
                                      min_child_weight = hyper_expanded_grid$min_child_weight[hyper_choice2],
                                      max_depth = hyper_expanded_grid$max_depth[hyper_choice2],
                                      subsample = hyper_expanded_grid$subsample[hyper_choice2],
                                      colsample_bytree = hyper_expanded_grid$colsample_bytree[hyper_choice2],
                                      eta = hyper_expanded_grid$eta[hyper_choice2],
                                      alpha = hyper_expanded_grid$alpha[hyper_choice2],
                                      gamma = hyper_expanded_grid$gamma[hyper_choice2],
                                      nrounds = chosen_eval_metric_val[[2]]$best_iteration[hyper_choice2])




  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]





  #Predict!
  prediction_list[[3]] <- as.numeric(predict(xgb.mod.refit, newdata = as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)])))
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  prediction_list[[4]] <- as.numeric(predict(xgb.mod.refit, newdata = as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)])))
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss =c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA), row.names =   c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(1.3^2*(sqrt(1+((y_list[[l]] - prediction_list[[l]])/(1.3))^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0,
                                                       0.5*(y_list[[l]] - prediction_list[[l]]),
                                                       (1-0.5)*(-1)*(y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))
    oos_testing_eval_metrics$hr[l] <- mean((y_list[[l]] * prediction_list[[l]])>0)
    oos_testing_eval_metrics$mb[l] <- mean(y_list[[l]] - prediction_list[[l]])



  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(xgb.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.0001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }

  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     min_child_weight = c(hyper_expanded_grid$min_child_weight[hyper_choice1], hyper_expanded_grid$min_child_weight[hyper_choice2]),
                                     max_depth = c(hyper_expanded_grid$max_depth[hyper_choice1], hyper_expanded_grid$max_depth[hyper_choice2]),
                                     subsample = c(hyper_expanded_grid$subsample[hyper_choice1], hyper_expanded_grid$subsample[hyper_choice2]),
                                     colsample_bytree = c(hyper_expanded_grid$colsample_bytree[hyper_choice1], hyper_expanded_grid$colsample_bytree[hyper_choice2]),
                                     eta = c(hyper_expanded_grid$eta[hyper_choice1], hyper_expanded_grid$eta[hyper_choice2]),
                                     alpha = c(hyper_expanded_grid$alpha[hyper_choice1], hyper_expanded_grid$alpha[hyper_choice2]),
                                     gamma = c(hyper_expanded_grid$gamma[hyper_choice1], hyper_expanded_grid$gamma[hyper_choice2]),
                                     nrounds = c(hyper_expanded_grid$nrounds[hyper_choice1], hyper_expanded_grid$nrounds[hyper_choice2]),
                                     best_iteration = c(chosen_eval_metric_val[[1]]$best_iteration[hyper_choice1],
                                                        chosen_eval_metric_val[[2]]$best_iteration[hyper_choice2])

  )



  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL

  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-5
  )

  suppressWarnings({
    future::plan("sequential")
  })

})

#Define your test
test_that("NN1 (Sequential - Parallel = TRUE) - run_ml_backtest works with rebalancing, 3m target, grid as tuning method, pseudo_huber and hr as chosen eval metric -toy_preprocessed_features_and_targets",{

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))
  future::plan("sequential")

  keras_architecture_parameters <- create_keras_architecture("Adam")
  keras_architecture_parameters <- add_layer(keras_architecture_parameters, units = 32, activation = "relu", batch_norm_option = TRUE)

  tensorflow::set_random_seed(100)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      ml_algorithm = "nn",
      early_stop = 25,
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "rmse",
      keras_architecture_parameters = keras_architecture_parameters,
      hyper_grid_domain = list(regularizer_l1 = 10^-(seq(2, 5, length=2)), regularizer_l2 = 0,
                                    droprate = c(0.50, 0.75), lr = 10^-seq(4,5, length = 2), size_of_batch = 512, number_of_epochs = 100),
      tuning_method = c("grid_search"),
      verbose = TRUE,
      show_plots = TRUE
    )}))


  max_epochs <- 100
  batch_size <- 512
  n_ensemble <- 10
  units = 32
  early_stop <- 25


  #Define initial objects
  hyper_expanded_grid <- expand.grid(list(regularizer_l1 = 10^-(seq(2, 5, length=2)), regularizer_l2 = 0,
                                          droprate = c(0.50, 0.75), lr = 10^-seq(4,5, length = 2), size_of_batch = 512, number_of_epochs = 100
                                          )
                                     )


  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]
  #Full data
  full_data_first_train <- cbind(targets_first_train$fwd_premium_3m, features_first_train[,-c(1:3)])
  colnames(full_data_first_train)[1] <- c("fwd_premium_3m")


  tensorflow::set_random_seed(100)
  #Use foreach to simulate result of parallelized hyper tuning
  first_rebal <-
    foreach::foreach(s = 1:nrow(hyper_expanded_grid), .options.future = list(seed = TRUE)) %dofuture% {

      model_nn_1 <- keras::keras_model_sequential()
      model_nn_1 %>%
        keras::layer_dense(units = units[1], activation = 'relu', input_shape =  ncol(features_first_train[,-c(1:3)]), #Shape = # of features
                           kernel_regularizer = keras::regularizer_l1_l2(
                             l1 = hyper_expanded_grid$l1[s], l2 =  hyper_expanded_grid$l2[s])) %>% #L1 and L2 Regularization
                             keras::layer_batch_normalization() %>% #Batch normalization
      keras::layer_dropout(rate = hyper_expanded_grid$droprate[s]) %>% #Adds dropout
      keras::layer_dense(units = 1) #No activation means linear: f(x) = x


      #Train Model
      #Backpropagation
      model_nn_1 %>% keras::compile( #Model Specification
        #Loss function
        loss ="mean_squared_error",
        #Optimization method and learning rate
        optimizer = keras::optimizer_adam(learning_rate = hyper_expanded_grid$lr[s]),
        #Custom eval metric
        metrics = "mean_squared_error"
        )


      #Fit
      fit_nn_1 <- model_nn_1 %>% #Keras models, unlike many R objects, are mutable objects. Piping after calling a model will alter it. Sucessive trainings then do not start from scratch.
        keras::fit(x = as.matrix(features_first_train[,-c(1:3)]), #Training features
                   y = targets_first_train$fwd_premium_3m, #Training label
                   epochs = hyper_expanded_grid$number_of_epochs[s], #Number of epochs
                   batch_size = hyper_expanded_grid$size_of_batch[s], #Batch size (should be a multiple of 2)
                   verbose = TRUE,
                   callbacks = list(keras::callback_early_stopping(monitor = "val_loss",
                                                            patience = early_stop, #Early stop (nº epochs with no improvement)
                                                            restore_best_weights = TRUE, #Restore best weights after stopping
                                                            mode = "min")), #Min for RMSE, MAE and HUBER
                   validation_data = list(as.matrix(features_first_val[,-c(1:3)]), targets_first_val$fwd_premium_3m) #Validation data
        )

      #Create objs
      out <- data.frame(matrix(NA, nrow = length(targets_first_val$fwd_premium_3m),
                               ncol = nrow(hyper_expanded_grid)))
      best_epoch <- vector(length = nrow(hyper_expanded_grid))



      #Predict vlidation data
      out[,s] <-
        stats::predict(model_nn_1, as.matrix(features_first_val[,-c(1:3)]))

      best_epoch[s] <- which.min(fit_nn_1$metrics$val_loss)


      #RMSE CHOSEN
      return(list(predictions = as.numeric(out[,s]),
                  metric = sqrt(mean((out[,s] - targets_first_val$fwd_premium_3m)^2)),
                  best_epoch = best_epoch))

    }


  #Pass objects
  #Features val
  chosen_eval_metric_val[[1]] <- data.frame(regularizer_l1 = hyper_expanded_grid$regularizer_l1,
                                            regularizer_l2 = hyper_expanded_grid$regularizer_l2,
                                            droprate = hyper_expanded_grid$droprate,
                                            lr = hyper_expanded_grid$lr,
                                            size_of_batch = hyper_expanded_grid$size_of_batch,
                                            number_of_epochs = hyper_expanded_grid$number_of_epochs,
                                            best_iteration = sapply(first_rebal, function(x) x$best_epoch) %>% diag(),
                                            chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid)))

  shrinkage.pred_df <- sapply(first_rebal, function(x) as.numeric(x$predictions)) #Transform to df
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]]) #chance colnames

  chosen_eval_metric_val[[1]]$chosen_eval_metric <- sapply(first_rebal, function(x) x$metric)


  #RMSE IS MIN: PAY ATTENTION
  hyper_choice1 <- which.min(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(targets_first_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(targets_first_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1)^2*(sqrt(1+((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                (1))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                     0.5*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                     (1-0.5)*(-1)*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/targets_first_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(targets_first_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice1])))/
    length(targets_first_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[1] <- mean(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])




  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]

  #Full data
  full_data_first_training_and_validation <- cbind(target_first_training_and_validation$fwd_premium_3m, features_first_training_and_validation[,-c(1:3)])
  colnames(full_data_first_training_and_validation)[1] <- c("fwd_premium_3m")

  #Refitted model
  model_nn_1 <- keras::keras_model_sequential()
  model_nn_1 %>%
    keras::layer_dense(units = units[1], activation = 'relu', input_shape =  ncol(features_first_training_and_validation[,-c(1:3)]), #Shape = # of features
                       kernel_regularizer = keras::regularizer_l1_l2(
                         l1 = hyper_expanded_grid$l1[hyper_choice1], l2 =  hyper_expanded_grid$l2[hyper_choice1])) %>% #L1 and L2 Regularization
    keras::layer_batch_normalization() %>% #Batch normalization
    keras::layer_dropout(rate = hyper_expanded_grid$droprate[hyper_choice1]) %>% #Adds dropout
    keras::layer_dense(units = 1) #No activation means linear: f(x) = x


  #Train Model
  #Backpropagation
  model_nn_1 %>% keras::compile( #Model Specification
    #Loss function
    loss ="mean_squared_error",
    #Optimization method and learning rate
    optimizer = keras::optimizer_adam(learning_rate = hyper_expanded_grid$lr[hyper_choice1]),
    #Custom eval metric
    metrics = "mean_squared_error"
  )


  #Fit
  fit_nn_1 <- model_nn_1 %>% #Keras models, unlike many R objects, are mutable objects. Piping after calling a model will alter it. Sucessive trainings then do not start from scratch.
    keras::fit(x = as.matrix(features_first_training_and_validation[,-c(1:3)]), #Training features
               y = target_first_training_and_validation$fwd_premium_3m, #Training label
               epochs =  chosen_eval_metric_val[[1]]$best_iteration[hyper_choice1], #Number of epochs
               batch_size = batch_size, #Batch size (should be a multiple of 2)
               verbose = TRUE)


  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(predict(model_nn_1, x = as.matrix(
    features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)])))
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  prediction_list[[2]] <- as.numeric(predict(model_nn_1, x = as.matrix(
    features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)])))
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]

  #Full data
  full_data_second_train <- cbind(targets_second_train$fwd_premium_3m, features_second_train[,-c(1:3)])
  colnames(full_data_second_train)[1] <- c("fwd_premium_3m")


  tictoc::tic()
  #Use foreach to simulate result of parallelized hyper tuning
  second_rebal <-
    foreach::foreach(s = 1:nrow(hyper_expanded_grid), .options.future = list(seed = TRUE)) %dofuture% {

      model_nn_2 <- keras::keras_model_sequential()
      model_nn_2 %>%
        keras::layer_dense(units = units[1], activation = 'relu', input_shape =  ncol(features_second_train[,-c(1:3)]), #Shape = # of features
                           kernel_regularizer = keras::regularizer_l1_l2(
                             l1 = hyper_expanded_grid$l1[s], l2 =  hyper_expanded_grid$l2[s])) %>% #L1 and L2 Regularization
        keras::layer_batch_normalization() %>% #Batch normalization
        keras::layer_dropout(rate = hyper_expanded_grid$droprate[s]) %>% #Adds dropout
        keras::layer_dense(units = 1) #No activation means linear: f(x) = x


      #Train Model
      #Backpropagation
      model_nn_2 %>% keras::compile( #Model Specification
        #Loss function
        loss ="mean_squared_error",
        #Optimization method and learning rate
        optimizer = keras::optimizer_adam(learning_rate = hyper_expanded_grid$lr[s]),
        #Custom eval metric
        metrics = "mean_squared_error"
      )


      #Fit
      fit_nn_2 <- model_nn_2 %>% #Keras models, unlike many R objects, are mutable objects. Piping after calling a model will alter it. Sucessive trainings then do not start from scratch.
        keras::fit(x = as.matrix(features_second_train[,-c(1:3)]), #Training features
                   y = targets_second_train$fwd_premium_3m, #Training label
                   epochs = max_epochs, #Number of epochs
                   batch_size = batch_size, #Batch size (should be a multiple of 2)
                   verbose = TRUE,
                   callbacks = list(keras::callback_early_stopping(monitor = "val_loss",
                                                                   patience = early_stop, #Early stop (nº epochs with no improvement)
                                                                   restore_best_weights = TRUE, #Restore best weights after stopping
                                                                   mode = "min")), #Min for RMSE, MAE and HUBER
                   validation_data = list(as.matrix(features_second_val[,-c(1:3)]), targets_second_val$fwd_premium_3m) #Validation data
        )

      #Create out obj
      out <- data.frame(matrix(NA, nrow = length(targets_second_val$fwd_premium_3m),
                               ncol = nrow(hyper_expanded_grid)))


      #Predict vlidation data
      out[,s] <-
        stats::predict(model_nn_2, x = as.matrix(features_second_val[,-c(1:3)]))


      #RMSE CHOSEN
      return(list(predictions = as.numeric(out[,s]),
                  metric = sqrt(mean((out[,s] - targets_second_val$fwd_premium_3m)^2)),
                  best_epoch = which.min(fit_nn_2$metrics$val_loss)))

    }
  tictoc::toc()

  #Pass objects
  #Features val
  chosen_eval_metric_val[[2]] <- data.frame(regularizer_l1 = hyper_expanded_grid$regularizer_l1,
                                            regularizer_l2 = hyper_expanded_grid$regularizer_l2,
                                            droprate = hyper_expanded_grid$droprate,
                                            lr = hyper_expanded_grid$lr,
                                            size_of_batch = hyper_expanded_grid$size_of_batch,
                                            number_of_epochs = hyper_expanded_grid$number_of_epochs,
                                            best_iteration = sapply(second_rebal, function(x) x$best_epoch),
                                            chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid)))


  #Pass objects
  shrinkage.pred_df <- sapply(second_rebal, function(x) as.numeric(x$predictions)) #Transform to df
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]]) #chance colnames

  chosen_eval_metric_val[[2]]$chosen_eval_metric <- sapply(second_rebal, function(x) x$metric)

  #old option for sequential implementation
  #for(s in 1:nrow(hyper_expanded_grid)){
  #Train Model
  #rf.mod2 <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_second_train),
  #                          mtry = hyper_expanded_grid$mtry[s] * (ncol(full_data_second_train) - 1),
  #                          num.trees = hyper_expanded_grid$num.trees[s],
  #                          max.depth = hyper_expanded_grid$max.depth[s],
  #                          min.bucket = hyper_expanded_grid$min.bucket[s])

  #Predict to validation data

  #shrinkage.pred_df[,s] <-
  # stats::predict(rf.mod2, data = janitor::clean_names(features_second_val[,-c(1:3)]))$predictions


  #CROSSPRODUCT CHOSEN
  #chosen_eval_metric_val[[2]]$chosen_eval_metric[s] <- mean(shrinkage.pred_df[,s] * targets_second_val$fwd_premium_3m)

  #}



  #RMSE IS MIN: PAY ATTENTION
  hyper_choice2 <- which.min(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(targets_second_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(targets_second_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))

  validation_eval_hyper_choice$mphe[2] <- mean((1)^2*(sqrt(1+((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                (1))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                     0.5*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                     (1-0.5)*(-1)*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs(
    (targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/targets_second_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- length(which(sign(targets_second_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice2])))/
    length(targets_second_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[2] <- mean(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])


  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]

  #Full data
  full_data_second_training_and_validation <- cbind(target_second_training_and_validation$fwd_premium_3m, features_second_training_and_validation[,-c(1:3)])
  colnames(full_data_second_training_and_validation)[1] <- c("fwd_premium_3m")


  #Refitted model
  #Refitted model
  model_nn_2 <- keras::keras_model_sequential()
  model_nn_2 %>%
    keras::layer_dense(units = units[1], activation = 'relu', input_shape =  ncol(features_first_training_and_validation[,-c(1:3)]), #Shape = # of features
                       kernel_regularizer = keras::regularizer_l1_l2(
                         l1 = hyper_expanded_grid$l1[hyper_choice2], l2 =  hyper_expanded_grid$l2[hyper_choice2])) %>% #L1 and L2 Regularization
    keras::layer_batch_normalization() %>% #Batch normalization
    keras::layer_dropout(rate = hyper_expanded_grid$droprate[hyper_choice2]) %>% #Adds dropout
    keras::layer_dense(units = 1) #No activation means linear: f(x) = x


  #Train Model
  #Backpropagation
  model_nn_2 %>% keras::compile( #Model Specification
    #Loss function
    loss ="mean_squared_error",
    #Optimization method and learning rate
    optimizer = keras::optimizer_adam(learning_rate = hyper_expanded_grid$lr[hyper_choice2]),
    #Custom eval metric
    metrics = "mean_squared_error"
  )


  #Fit
  fit_nn_2 <- model_nn_2 %>% #Keras models, unlike many R objects, are mutable objects. Piping after calling a model will alter it. Sucessive trainings then do not start from scratch.
    keras::fit(x = as.matrix(features_second_training_and_validation[,-c(1:3)]), #Training features
               y = target_second_training_and_validation$fwd_premium_3m, #Training label
               epochs =  chosen_eval_metric_val[[2]]$best_iteration[hyper_choice2], #Number of epochs
               batch_size = batch_size, #Batch size (should be a multiple of 2)
               verbose = TRUE)



  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]



  #Predict!
  prediction_list[[3]] <- as.numeric(predict(model_nn_2, x = as.matrix(
    features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)])))
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  prediction_list[[4]] <- as.numeric(predict(model_nn_2, x = as.matrix(
    features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)])))
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss =c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA), row.names =   c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(1^2*(sqrt(1+(y_list[[l]] - prediction_list[[l]])^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0,
                                                   0.5*(y_list[[l]] - prediction_list[[l]]),
                                                   (1-0.5)*(-1)*(y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))
    oos_testing_eval_metrics$hr[l] <- mean((y_list[[l]] * prediction_list[[l]])>0)
    oos_testing_eval_metrics$mb[l] <- mean(y_list[[l]] - prediction_list[[l]])



  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  results$outputs[[5]] <- ml_backtest_results@final_model


  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     regularizer_l1 = c(hyper_expanded_grid$regularizer_l1[hyper_choice1], hyper_expanded_grid$regularizer_l1[hyper_choice2]),
                                     regularizer_l2 = c(hyper_expanded_grid$regularizer_l2[hyper_choice1], hyper_expanded_grid$regularizer_l2[hyper_choice2]),
                                     droprate = c(hyper_expanded_grid$droprate[hyper_choice1], hyper_expanded_grid$droprate[hyper_choice2]),
                                     lr = c(hyper_expanded_grid$lr[hyper_choice1], hyper_expanded_grid$lr[hyper_choice2]),
                                     size_of_batch = c(hyper_expanded_grid$size_of_batch[hyper_choice1], hyper_expanded_grid$size_of_batch[hyper_choice2]),
                                     number_of_epochs = c(hyper_expanded_grid$number_of_epochs[hyper_choice1], hyper_expanded_grid$number_of_epochs[hyper_choice2]),
                                     best_iteration = c(chosen_eval_metric_val[[1]]$best_iteration[hyper_choice1], chosen_eval_metric_val[[2]]$best_iteration[hyper_choice2])
                                     )


  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")


  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-3
  )

  future::plan("sequential")

})

#Define your test
test_that("RF (Sequential - Parallel = TRUE) - run_ml_backtest works with rebalancing, 3m target, grid as tuning method and cp as chosen eval metric -toy_preprocessed_features_and_targets",{

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))
  future::plan("sequential")

  set.seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      ml_algorithm = "rf",
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "cp",
      hyper_grid_domain = list(mtry = c(0, 0.5, 1), num.trees = c(200, 500),
                                    max.depth = c(2, 4, 6), min.bucket = c(1, 5, 10)),
      tuning_method = c("grid_search"),
      quantile_tau = 0.25,
      verbose = FALSE,
      show_plots = FALSE
    )}))



  #Define initial objects
  hyper_expanded_grid <- expand.grid(list(mtry = c(0, 0.5, 1), num.trees = c(200, 500),
                                          max.depth = c(2, 4, 6), min.bucket = c(1, 5, 10)))


  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]
  #Full data
  full_data_first_train <- cbind(targets_first_train$fwd_premium_3m, features_first_train[,-c(1:3)])
  colnames(full_data_first_train)[1] <- c("fwd_premium_3m")

  #Features val
  chosen_eval_metric_val[[1]] <- data.frame(mtry = hyper_expanded_grid$mtry, num.trees = hyper_expanded_grid$num.trees,
                                              max.depth = hyper_expanded_grid$max.depth, min.bucket = hyper_expanded_grid$min.bucket,
                                              chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid)))

  set.seed(123)

  #Use foreach to simulate result of parallelized hyper tuning

  first_rebal <- suppressWarnings({
    foreach::foreach(s = 1:nrow(hyper_expanded_grid), .options.future = list(seed = TRUE)) %dofuture% {
      #Train Model
      rf.mod1 <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_first_train),
                                mtry = hyper_expanded_grid$mtry[s] * (ncol(full_data_first_train) - 1),
                                num.trees = hyper_expanded_grid$num.trees[s],
                                max.depth = hyper_expanded_grid$max.depth[s],
                                min.bucket = hyper_expanded_grid$min.bucket[s]
      )

      out <- data.frame(matrix(NA, nrow = length(targets_first_val$fwd_premium_3m),
                               ncol = nrow(hyper_expanded_grid)))


      #Predict vlidation data
      out[,s] <-
        stats::predict(rf.mod1, data = janitor::clean_names(features_first_val[,-c(1:3)]))$predictions


      #CROSSPRODUCT CHOSEN
      return(list(predictions = out[,s],
                  metric = mean(out[,s] * targets_first_val$fwd_premium_3m)))

    }
  })

  #Pass objects
  shrinkage.pred_df <- sapply(first_rebal, function(x) as.numeric(x$predictions)) #Transform to df
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]]) #chance colnames

  chosen_eval_metric_val[[1]]$chosen_eval_metric <- sapply(first_rebal, function(x) x$metric)


  #rsquared IS MAX: PAY ATTENTION
  hyper_choice1 <- which.max(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(targets_first_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(targets_first_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1)^2*(sqrt(1+((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                         0.25*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                         (1-0.25)*(-1)*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/targets_first_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(targets_first_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice1])))/
    length(targets_first_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[1] <- mean(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])




  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]

  #Full data
  full_data_first_training_and_validation <- cbind(target_first_training_and_validation$fwd_premium_3m, features_first_training_and_validation[,-c(1:3)])
  colnames(full_data_first_training_and_validation)[1] <- c("fwd_premium_3m")

  #Refitted model
  rf.mod.refit <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_first_training_and_validation),
                                 mtry = hyper_expanded_grid$mtry[hyper_choice1] * (ncol(full_data_first_training_and_validation) - 1),
                                 num.trees = hyper_expanded_grid$num.trees[hyper_choice1],
                                 max.depth = hyper_expanded_grid$max.depth[hyper_choice1],
                                 min.bucket = hyper_expanded_grid$min.bucket[hyper_choice1])


  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(predict(rf.mod.refit, data = janitor::clean_names(features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)]))$predictions)
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  prediction_list[[2]] <- as.numeric(predict(rf.mod.refit, data = janitor::clean_names(features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)]))$predictions)
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]

  #Full data
  full_data_second_train <- cbind(targets_second_train$fwd_premium_3m, features_second_train[,-c(1:3)])
  colnames(full_data_second_train)[1] <- c("fwd_premium_3m")

  #Features val
  chosen_eval_metric_val[[2]] <- data.frame(mtry = hyper_expanded_grid$mtry, num.trees = hyper_expanded_grid$num.trees,
                                              max.depth = hyper_expanded_grid$max.depth, min.bucket = hyper_expanded_grid$min.bucket,
                                              chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid)))


  #Use foreach to simulate result of parallelized hyper tuning
  second_rebal <- suppressWarnings({
    foreach::foreach(s = 1:nrow(hyper_expanded_grid), .options.future = list(seed = TRUE)) %dofuture% {
      #Train Model
      rf.mod2 <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_second_train),
                                mtry = hyper_expanded_grid$mtry[s] * (ncol(full_data_second_train) - 1),
                                num.trees = hyper_expanded_grid$num.trees[s],
                                max.depth = hyper_expanded_grid$max.depth[s],
                                min.bucket = hyper_expanded_grid$min.bucket[s]
      )

      out <- data.frame(matrix(NA, nrow = length(targets_second_val$fwd_premium_3m),
                               ncol = nrow(hyper_expanded_grid)))


      #Predict vlidation data
      out[,s] <-
        stats::predict(rf.mod2, data = janitor::clean_names(features_second_val[,-c(1:3)]))$predictions


      #CROSSPRODUCT CHOSEN
      return(list(predictions = out[,s],
                  metric = mean(out[,s] * targets_second_val$fwd_premium_3m)))

    }

})

  #Pass objects
  shrinkage.pred_df <- sapply(second_rebal, function(x) as.numeric(x$predictions)) #Transform to df
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]]) #chance colnames

  chosen_eval_metric_val[[2]]$chosen_eval_metric <- sapply(second_rebal, function(x) x$metric)

  #old option for sequential implementation
  #for(s in 1:nrow(hyper_expanded_grid)){
  #Train Model
  #rf.mod2 <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_second_train),
  #                          mtry = hyper_expanded_grid$mtry[s] * (ncol(full_data_second_train) - 1),
  #                          num.trees = hyper_expanded_grid$num.trees[s],
  #                          max.depth = hyper_expanded_grid$max.depth[s],
  #                          min.bucket = hyper_expanded_grid$min.bucket[s])

  #Predict to validation data

  #shrinkage.pred_df[,s] <-
  # stats::predict(rf.mod2, data = janitor::clean_names(features_second_val[,-c(1:3)]))$predictions


  #CROSSPRODUCT CHOSEN
  #chosen_eval_metric_val[[2]]$chosen_eval_metric[s] <- mean(shrinkage.pred_df[,s] * targets_second_val$fwd_premium_3m)

  #}



  #CP IS MAX: PAY ATTENTION
  hyper_choice2 <- which.max(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(targets_second_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(targets_second_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))


  validation_eval_hyper_choice$mphe[2] <- mean((1)^2*(sqrt(1+((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                         0.25*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                         (1-0.25)*(-1)*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs(
    (targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/targets_second_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- length(which(sign(targets_second_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice2])))/
    length(targets_second_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[2] <- mean(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])






  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]

  #Full data
  full_data_second_training_and_validation <- cbind(target_second_training_and_validation$fwd_premium_3m, features_second_training_and_validation[,-c(1:3)])
  colnames(full_data_second_training_and_validation)[1] <- c("fwd_premium_3m")


  #Refitted model
  rf.mod.refit <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_second_training_and_validation),
                                 mtry = hyper_expanded_grid$mtry[hyper_choice2] * (ncol(full_data_second_training_and_validation) - 1),
                                 num.trees = hyper_expanded_grid$num.trees[hyper_choice2],
                                 max.depth = hyper_expanded_grid$max.depth[hyper_choice2],
                                 min.bucket = hyper_expanded_grid$min.bucket[hyper_choice2])



  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]



  #Predict!
  prediction_list[[3]] <- as.numeric(predict(rf.mod.refit, data = janitor::clean_names(features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)]))$predictions)
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  prediction_list[[4]] <- as.numeric(predict(rf.mod.refit, data = janitor::clean_names(features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)]))$predictions)
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss =c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA), row.names =   c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(1^2*(sqrt(1+(y_list[[l]] - prediction_list[[l]])^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0,
                                                       0.25*(y_list[[l]] - prediction_list[[l]]),
                                                       (1-0.25)*(-1)*(y_list[[l]] - prediction_list[[l]])))

    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))
    oos_testing_eval_metrics$hr[l] <- length(which(sign(y_list[[l]]) == sign(prediction_list[[l]])))/length(y_list[[l]])
    oos_testing_eval_metrics$mb[l] <- mean((y_list[[l]] - prediction_list[[l]]))


  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(rf.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.0001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }

    #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     mtry = c(hyper_expanded_grid$mtry[hyper_choice1], hyper_expanded_grid$mtry[hyper_choice2]),
                                     num.trees = c(hyper_expanded_grid$num.trees[hyper_choice1], hyper_expanded_grid$num.trees[hyper_choice2]),
                                     max.depth = c(hyper_expanded_grid$max.depth[hyper_choice1], hyper_expanded_grid$max.depth[hyper_choice2]),
                                     min.bucket = c(hyper_expanded_grid$min.bucket[hyper_choice1], hyper_expanded_grid$min.bucket[hyper_choice2]))


  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-5
  )

  future::plan("sequential")

})

#Define your test
test_that("RF (Sequential - Parallel = FALSE) - run_ml_backtest works with rebalancing, 3m target, grid as tuning method and hr as chosen eval metric -toy_preprocessed_features_and_targets",{

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  set.seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      ml_algorithm = "rf",
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "hr",
      hyper_grid_domain = list(mtry = c(0, 0.5, 1), num.trees = c(200, 500),
                                    max.depth = c(2, 4, 6), min.bucket = c(1, 5, 10)),
      tuning_method = c("grid_search"),
      verbose = FALSE,
      show_plots = FALSE,
      parallel = FALSE
    )}))



  #Define initial objects
  hyper_expanded_grid <- expand.grid(list(mtry = c(0, 0.5, 1), num.trees = c(200, 500),
                                          max.depth = c(2, 4, 6), min.bucket = c(1, 5, 10)))


  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]
  #Full data
  full_data_first_train <- cbind(targets_first_train$fwd_premium_3m, features_first_train[,-c(1:3)])
  colnames(full_data_first_train)[1] <- c("fwd_premium_3m")

  #Features val
  chosen_eval_metric_val[[1]] <- data.frame(mtry = hyper_expanded_grid$mtry, num.trees = hyper_expanded_grid$num.trees,
                                              max.depth = hyper_expanded_grid$max.depth, min.bucket = hyper_expanded_grid$min.bucket,
                                              chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid)))

   shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(targets_first_val$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid)))

  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]])

  set.seed(123)
  for(s in 1:nrow(hyper_expanded_grid)){
    #Train Model
    rf.mod1 <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_first_train),
                              mtry = hyper_expanded_grid$mtry[s] * (ncol(full_data_first_train) - 1),
                              num.trees = hyper_expanded_grid$num.trees[s],
                              max.depth = hyper_expanded_grid$max.depth[s],
                              min.bucket = hyper_expanded_grid$min.bucket[s])

    #Predict to validation data

    shrinkage.pred_df[,s] <-
      stats::predict(rf.mod1, data = janitor::clean_names(features_first_val[,-c(1:3)]))$predictions


    #HR CHOSEN
    chosen_eval_metric_val[[1]]$chosen_eval_metric[s] <- mean((shrinkage.pred_df[,s] * targets_first_val$fwd_premium_3m) > 0)
  }




  #HR IS MAX: PAY ATTENTION
  hyper_choice1 <- which.max(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(targets_first_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(targets_first_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1)^2*(sqrt(1+((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                         0.5*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                         (1-0.5)*(-1)*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/targets_first_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- mean((targets_first_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])>0)

  validation_eval_hyper_choice$mb[1] <- mean(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])



  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]

  #Full data
  full_data_first_training_and_validation <- cbind(target_first_training_and_validation$fwd_premium_3m, features_first_training_and_validation[,-c(1:3)])
  colnames(full_data_first_training_and_validation)[1] <- c("fwd_premium_3m")

  #Refitted model
  rf.mod.refit <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_first_training_and_validation),
                                 mtry = hyper_expanded_grid$mtry[hyper_choice1] * (ncol(full_data_first_training_and_validation) - 1),
                                 num.trees = hyper_expanded_grid$num.trees[hyper_choice1],
                                 max.depth = hyper_expanded_grid$max.depth[hyper_choice1],
                                 min.bucket = hyper_expanded_grid$min.bucket[hyper_choice1])


  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(predict(rf.mod.refit, data = janitor::clean_names(features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)]))$predictions)
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  prediction_list[[2]] <- as.numeric(predict(rf.mod.refit, data = janitor::clean_names(features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)]))$predictions)
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]

  #Full data
  full_data_second_train <- cbind(targets_second_train$fwd_premium_3m, features_second_train[,-c(1:3)])
  colnames(full_data_second_train)[1] <- c("fwd_premium_3m")

  #Features val
  chosen_eval_metric_val[[2]] <- data.frame(mtry = hyper_expanded_grid$mtry, num.trees = hyper_expanded_grid$num.trees,
                                              max.depth = hyper_expanded_grid$max.depth, min.bucket = hyper_expanded_grid$min.bucket,
                                              chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid)))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(targets_second_val$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid)))

  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]])


  for(s in 1:nrow(hyper_expanded_grid)){
    #Train Model
    rf.mod2 <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_second_train),
                              mtry = hyper_expanded_grid$mtry[s] * (ncol(full_data_second_train) - 1),
                              num.trees = hyper_expanded_grid$num.trees[s],
                              max.depth = hyper_expanded_grid$max.depth[s],
                              min.bucket = hyper_expanded_grid$min.bucket[s])


    #Predict vlidation data
    shrinkage.pred_df[,s] <-
      stats::predict(rf.mod2, data = janitor::clean_names(features_second_val[,-c(1:3)]))$predictions


    #HR CHOSEN
    chosen_eval_metric_val[[2]]$chosen_eval_metric[s] <- mean((shrinkage.pred_df[,s] * targets_second_val$fwd_premium_3m)>0)

  }


  #old option for sequential implementation
  #for(s in 1:nrow(hyper_expanded_grid)){
  #Train Model
  #rf.mod2 <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_second_train),
  #                          mtry = hyper_expanded_grid$mtry[s] * (ncol(full_data_second_train) - 1),
  #                          num.trees = hyper_expanded_grid$num.trees[s],
  #                          max.depth = hyper_expanded_grid$max.depth[s],
  #                          min.bucket = hyper_expanded_grid$min.bucket[s])

  #Predict to validation data

  #shrinkage.pred_df[,s] <-
  # stats::predict(rf.mod2, data = janitor::clean_names(features_second_val[,-c(1:3)]))$predictions


  #CROSSPRODUCT CHOSEN
  #chosen_eval_metric_val[[2]]$chosen_eval_metric[s] <- mean(shrinkage.pred_df[,s] * targets_second_val$fwd_premium_3m)

  #}



  #CP IS MAX: PAY ATTENTION
  hyper_choice2 <- which.max(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(targets_second_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(targets_second_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))

  validation_eval_hyper_choice$mphe[2] <- mean((1)^2*(sqrt(1+((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                         0.5*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                         (1-0.5)*(-1)*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/targets_second_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- mean((targets_second_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])>0)

  validation_eval_hyper_choice$mb[2] <- mean(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])


  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]

  #Full data
  full_data_second_training_and_validation <- cbind(target_second_training_and_validation$fwd_premium_3m, features_second_training_and_validation[,-c(1:3)])
  colnames(full_data_second_training_and_validation)[1] <- c("fwd_premium_3m")


  #Refitted model
  rf.mod.refit <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_second_training_and_validation),
                                 mtry = hyper_expanded_grid$mtry[hyper_choice2] * (ncol(full_data_second_training_and_validation) - 1),
                                 num.trees = hyper_expanded_grid$num.trees[hyper_choice2],
                                 max.depth = hyper_expanded_grid$max.depth[hyper_choice2],
                                 min.bucket = hyper_expanded_grid$min.bucket[hyper_choice2])



  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]



  #Predict!
  prediction_list[[3]] <- as.numeric(predict(rf.mod.refit, data = janitor::clean_names(features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)]))$predictions)
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  prediction_list[[4]] <- as.numeric(predict(rf.mod.refit, data = janitor::clean_names(features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)]))$predictions)
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss =c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA), row.names =   c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(1^2*(sqrt(1+(y_list[[l]] - prediction_list[[l]])^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0,
                                                       0.5*(y_list[[l]] - prediction_list[[l]]),
                                                       (1-0.5)*(-1)*(y_list[[l]] - prediction_list[[l]])))

    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))

    oos_testing_eval_metrics$hr[l] <- mean((y_list[[l]] * prediction_list[[l]]) > 0)

    oos_testing_eval_metrics$mb[l] <- mean(y_list[[l]] - prediction_list[[l]])


  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(rf.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.0001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }

   #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     mtry = c(hyper_expanded_grid$mtry[hyper_choice1], hyper_expanded_grid$mtry[hyper_choice2]),
                                     num.trees = c(hyper_expanded_grid$num.trees[hyper_choice1], hyper_expanded_grid$num.trees[hyper_choice2]),
                                     max.depth = c(hyper_expanded_grid$max.depth[hyper_choice1], hyper_expanded_grid$max.depth[hyper_choice2]),
                                     min.bucket = c(hyper_expanded_grid$min.bucket[hyper_choice1], hyper_expanded_grid$min.bucket[hyper_choice2]))


  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-3
  )


})

#Define your test Excel sheet test glmnet 7
test_that("GLMNET - run_ml_backtest works with rebalancing at final, 3m target, grid_search as tuning method and mphe as chosen eval metric",{

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
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
             dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400),
                               class = c("POSIXct", "POSIXt"), tzone = "UTC"), format = "%Y-%m-%d"),
             Alpha = c(3, -20, -450, 5, -2, 1,
                       6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                       8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                       -1, 4, 4),
             Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                      -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                      2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
             Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                       -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                       3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame"),
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
                       dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                       format = "%Y-%m-%d"),
                       fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                          2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                       fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                          3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                       fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                         10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame"),

      training_sample_size = 4,
      validation_sample_size = 3,
      rebalancing_months = 11,
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "mphe",
      ml_algorithm = "glmnet",
      hyper_grid_domain = list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0.1, 0.9, length=10)), #Grid for lambda search
      tuning_method = c("grid_search"),
      huber_delta = 1.25,
      verbose = FALSE,
      show_plots = FALSE
    )}))

  #Define initial objects
  hyper_expanded_grid <- expand.grid(list(alpha = c(0, 0.5, 1), lambda.min.ratio = seq(0.1, 0.9, length=10)))

  validation_eval_hyper_choice <- data.frame(rss =c(NA, NA),  #Validation loss df
                                             cp = c(NA, NA),
                                             rmse = c(NA, NA),
                                             mae = c(NA, NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             row.names = c("2001-09-15", "2001-11-15"))
  rebalance_dates <- c("2001-09-15", "2001-11-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #Start first rebalancing

  #Get objects to train and validate model
  features_training <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                             "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(984614400, 984614400, 984614400, 984614400,
                         984614400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(3,  1, 2, 5, 5), Beta = c(4, 5, 6, 0, 2), Gamma = c(800, 9, 10, -9, 3)), row.names = c(NA, -5L), class = "data.frame")

  target_training <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5),
                                    fwd_premium_3m = c(4, 5, 0, 1, 9),
                                    fwd_sharpe_1m = c(7, 4, 2, 4, 7)), row.names = c(NA, -5L), class = "data.frame")

  features_validation <- structure(list(id = c("Stock A-2001-06-15", "Stock B-2001-06-15",
                                               "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(992563200, 992563200, 992563200, 992563200,
                         992563200), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(5,  2, -20, -1, 2), Beta = c(3, 1, 1, 2, 1), Gamma = c(20, -15, 6,  3, 6)), row.names = c(NA, -5L), class = "data.frame")

  target_validation <- structure(list(fwd_premium_1m = c(1, 3, 5, 7, 1),
                                      fwd_premium_3m = c(0, 3, 8, 3, 2),
                                      fwd_sharpe_1m = c(1, 5, 6, 10, 3)), row.names = c(NA,  -5L), class = "data.frame")


  #Start first rebalancing
  chosen_eval_metric_val[[1]] <- data.frame(alpha = hyper_expanded_grid$alpha,
                                            lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio,
                                            best_lam = rep(NA,30), chosen_eval_metric = rep(NA, 30))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid)))
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]])
  best_lam1 <- vector(length =  nrow(hyper_expanded_grid))

  for(s in 1:length(hyper_expanded_grid$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_3m,
      alpha = hyper_expanded_grid$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam1[s] <- glm.mod1$lambda[
      which.min(
        colMeans(1.25^2 * (sqrt((1 + ((target_validation$fwd_premium_3m -
                             predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)])))/1.25)^2)) - 1
                )
      ))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam1[s])

    #MPHE CHOSEN
    chosen_eval_metric_val[[1]]$chosen_eval_metric[which(chosen_eval_metric_val[[1]]$alpha == unique(hyper_expanded_grid$alpha)[s])] <-
      mean(1.25^2 * (sqrt((1 + ((target_validation$fwd_premium_3m - shrinkage.pred_df[,s])/1.25)^2)) - 1))

  }

  chosen_eval_metric_val[[1]]$best_lam <- best_lam1

  #MPHE IS MIN: PAY ATTENTION
  hyper_choice1 <- which.min(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(target_validation$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(target_validation$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1.25)^2*(sqrt(1+((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                        (1.25))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                         0.5*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                         (1-0.5)*(-1)*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/target_validation$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- mean((target_validation$fwd_premium_3m * shrinkage.pred_df[,hyper_choice1]) > 0)

  validation_eval_hyper_choice$mb[1] <- mean(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])





  #Refit
  features_training_and_validation <-structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                                           "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                                           "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                                           "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                                           "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                                           "Stock E-2001-05-15", "Stock A-2001-06-15", "Stock B-2001-06-15",
                                                           "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E",
                 "Stock A", "Stock B", "Stock C", "Stock D", "Stock E", "Stock A",
                 "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                 "Stock C", "Stock D", "Stock E"), dates = structure(c(984614400,
                                                                       984614400, 984614400, 984614400, 984614400, 987292800, 987292800,
                                                                       987292800, 987292800, 987292800, 989884800, 989884800, 989884800,
                                                                       989884800, 989884800, 992563200, 992563200, 992563200, 992563200,
                                                                       992563200), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1, 5, 2, -20, -1, 2), Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3,  3, 1, 1, 2, 1),
  Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500, 20, -15, 6, 3, 6)), row.names = c(NA, -20L), class = "data.frame")

  target_training_and_validation <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                                      1, 7, 2, 7, 8, 8, 1, 3, 5, 7, 1),
                                                   fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1, 0, 3, 8, 3, 2),
                                                   fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3, 1, 5, 6, 10, 3)),
                                              row.names = c(NA, -20L), class = "data.frame")


  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice1],
                                  lambda = hyper_expanded_grid$lambda[hyper_choice1])
  coef(glm.mod.refit)

  #2nd Rebalancing

  #Get objects to train and validate model
  features_training <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                             "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                             "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                             "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                             "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                             "Stock E-2001-05-15"),
                                      tickers = c("Stock A", "Stock B", "Stock C",
                                                  "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                  "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
                                      ),
                                      dates = structure(c(984614400, 984614400, 984614400, 984614400,
                                                          984614400, 987292800, 987292800, 987292800, 987292800, 987292800,
                                                          989884800, 989884800, 989884800, 989884800, 989884800), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                      Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1), Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3),
                                      Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500)), row.names = c(NA, -15L), class = "data.frame")


  target_training <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                       1, 7, 2, 7, 8, 8),
                                    fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1),
                                    fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3)), row.names = c(NA, -15L), class = "data.frame")

  features_validation <- structure(list(id = c("Stock A-2001-08-15", "Stock B-2001-08-15",
                                               "Stock C-2001-08-15", "Stock D-2001-08-15", "Stock E-2001-08-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(997833600, 997833600, 997833600, 997833600,
                         997833600), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(1, 1, -20, -25, -20), Beta = c(10, -10, 4, 1, 1), Gamma = c(-523, 4, 12, -10, 405)), row.names = c(NA, -5L), class = "data.frame")

  target_validation <-structure(list(fwd_premium_1m = c(1, -1, -9, -2, 1),
                                     fwd_premium_3m = c(5, 2, 5, 1, -9),
                                     fwd_sharpe_1m = c(3, 1, 1, 4, 1)), row.names = c(NA,  -5L), class = "data.frame")



  #Start 2nd rebalancing
  chosen_eval_metric_val[[2]] <- data.frame(alpha = hyper_expanded_grid$alpha,
                                            lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio,
                                            best_lam = rep(NA,30), chosen_eval_metric = rep(NA, 30))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid)))
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]])
  best_lam2 <- vector(length =  nrow(hyper_expanded_grid))

  for(s in 1:length(hyper_expanded_grid$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_3m,
      alpha = hyper_expanded_grid$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam2[s] <- glm.mod1$lambda[
      which.min(
        colMeans(1.25^2 * (sqrt((1 + ((target_validation$fwd_premium_3m -
                                         predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)])))/1.25)^2)) - 1
        )
        ))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam2[s])

    #MPHE CHOSEN
    chosen_eval_metric_val[[2]]$chosen_eval_metric[which(chosen_eval_metric_val[[2]]$alpha == unique(hyper_expanded_grid$alpha)[s])] <-
      mean(1.25^2 * (sqrt((1 + ((target_validation$fwd_premium_3m - shrinkage.pred_df[,s])/1.25)^2)) - 1))

  }

  chosen_eval_metric_val[[2]]$best_lam <- best_lam2


  #mphe IS MIN: PAY ATTENTION
  hyper_choice2 <- which.min(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(target_validation$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(target_validation$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))

  validation_eval_hyper_choice$mphe[2] <- mean((1.25)^2*(sqrt(1+((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                        (1.25))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                         0.5*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                         (1-0.5)*(-1)*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/target_validation$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- mean((target_validation$fwd_premium_3m * shrinkage.pred_df[,hyper_choice2]) > 0)

  validation_eval_hyper_choice$mb[2] <- mean(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])

  #Refit
  features_training_and_validation <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                                            "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                                            "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                                            "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                                            "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                                            "Stock E-2001-05-15", "Stock A-2001-06-15", "Stock B-2001-06-15",
                                                            "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15",
                                                            "Stock A-2001-07-15", "Stock B-2001-07-15", "Stock C-2001-07-15",
                                                            "Stock D-2001-07-15", "Stock E-2001-07-15", "Stock A-2001-08-15",
                                                            "Stock B-2001-08-15", "Stock C-2001-08-15", "Stock D-2001-08-15",
                                                            "Stock E-2001-08-15"),
                                                     tickers = c("Stock A", "Stock B", "Stock C","Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                                 "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E",
                                                                 "Stock A", "Stock B", "Stock C", "Stock D", "Stock E", "Stock A",
                                                                 "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                                                                 "Stock C", "Stock D", "Stock E"),
                                                     dates = structure(c(984614400, 984614400, 984614400, 984614400, 984614400, 987292800, 987292800,
                                                                         987292800, 987292800, 987292800, 989884800, 989884800, 989884800,
                                                                         989884800, 989884800, 992563200, 992563200, 992563200, 992563200,
                                                                         992563200, 995155200, 995155200, 995155200, 995155200, 995155200,
                                                                         997833600, 997833600, 997833600, 997833600, 997833600), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                                     Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1, 5, 2, -20, -1, 2, -2, 20, -150, -50, -1, 1, 1, -20, -25, -20),
                                                     Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3, 3, 1, 1, 2, 1, 13, -12, 1, 5, 2, 10, -10, 4, 1, 1),
                                                     Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500, 20, -15, 6, 3, 6, 0, 3, 20, 3, 4, -523, 4, 12, -10, 405)),
                                                row.names = c(NA, -30L), class = "data.frame")

  target_training_and_validation <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                                      1, 7, 2, 7, 8, 8, 1, 3, 5, 7, 1, 2, 5, 1, 2, 2, 1, -1, -9, -2,
                                                                      1),
                                                   fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1, 0, 3, 8, 3, 2, 6, 8, 3, 1, 3, 5, 2, 5, 1, -9),
                                                   fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3, 1, 5, 6, 10, 3, 1,  4, 5, 1, 0, 3, 1, 1, 4, 1)),
                                              row.names = c(NA, -30L), class = "data.frame")

  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice2],
                                  lambda = hyper_expanded_grid$lambda[hyper_choice2])


  coef(glm.mod.refit)

  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid$alpha[hyper_choice2],
                                  lambda.min.ratio = hyper_expanded_grid$lambda.min.ratio[hyper_choice2])


  coef(glm.mod.refit)


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")
  #Pred list
  prediction_list <- list(`2001-09-15` = c(`Stock A` = 3.95, `Stock B` = 3.95, `Stock C` = 3.95,
                                           `Stock D` = 3.95, `Stock E` = 3.95), `2001-10-15` = c(`Stock A` = 3.95,
                                                                                                 `Stock B` = 3.95, `Stock C` = 3.95, `Stock D` = 3.95, `Stock E` = 3.95
                                           ), `2001-11-15` = c(`Stock A` = 3.466667, `Stock B` = 3.466667,
                                                               `Stock C` = 3.466667, `Stock D` = 3.466667, `Stock E` = 3.466667
                                           ))
  results$outputs[[1]] <- prediction_list
  #Error list
  error_list <- list(`2001-09-15` = c(`Stock A` = -8.95, `Stock B` = 1.05, `Stock C` = -0.95,
                                      `Stock D` = 7.05, `Stock E` = -7.95), `2001-10-15` = c(`Stock A` = -4.95,
                                                                                             `Stock B` = -2.95, `Stock C` = 36.05, `Stock D` = 0.0499999999999998,
                                                                                             `Stock E` = 0.0499999999999998), `2001-11-15` = c(`Stock A` = 0.533333,
                                                                                                                                               `Stock B` = -1.466667, `Stock C` = -1.466667, `Stock D` = -1.466667,
                                                                                                                                               `Stock E` = -0.466667))
  results$outputs[[2]] <- error_list
  #Y-list
  y_list <- list(`2001-09-15` = c(`Stock A` = -5, `Stock B` = 5, `Stock C` = 3,
                                  `Stock D` = 11, `Stock E` = -4), `2001-10-15` = c(`Stock A` = -1,
                                                                                    `Stock B` = 1, `Stock C` = 40, `Stock D` = 4, `Stock E` = 4),
                 `2001-11-15` = c(`Stock A` = 4, `Stock B` = 2, `Stock C` = 2,
                                  `Stock D` = 2, `Stock E` = 3))
  results$outputs[[3]] <- y_list
  #Eval metrics
  oos_testing_eval_metrics <-structure(list(rss =c(0.0050382653061225, 0.184325275397797,
                                                         0.812011933933919), cp = c(7.9, 37.92, 9.0133342),
                                            rmse = c(6.24519815538306, 16.3267418672557, 1.17945397913145
                                            ), mae = c(5.19, 8.81, 1.0800002),
                                            mphe = c(5.29925, 10.15824, 0.55613),
                                            mpe = c(2.595, 4.405, 0.54),
                                            mape = c(0.98902, 1.76525, 0.49778),
                                            hr = c(0.6, 0.8, 1),
                                            mb = c(-1.95, 5.65, -0.87)
                                            ),
                                       class = "data.frame", row.names = c("2001-09-15", "2001-10-15", "2001-11-15"))
  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(glm.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }


  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- c("2001-09-15", "2001-11-15")
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = c("2001-09-15", "2001-11-15"),
                                     alpha = c(hyper_expanded_grid$alpha[hyper_choice1], hyper_expanded_grid$alpha[hyper_choice2]),
                                     lambda.min.ratio = c(hyper_expanded_grid$lambda[hyper_choice1], hyper_expanded_grid$lambda[hyper_choice2]),
                                     best_lam = c(best_lam1[hyper_choice1], best_lam2[hyper_choice2])
                                     )

  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-3
  )

})

###Random Search
#Define your test Excel sheet test glmnet 8
test_that("GLMNET - run_ml_backtest works with rebalancing at final, 3m target, random_search (uniform) as tuning method and rmse as chosen eval metric",{


  ################Uniform Distribution
  #########################################
  set.seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
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
             dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                 987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                 1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                 995155200, 997833600, 1000512000, 1003104000, 1005782400),
                               class = c("POSIXct", "POSIXt"), tzone = "UTC"), format = "%Y-%m-%d"),
             Alpha = c(3, -20, -450, 5, -2, 1,
                       6, 1, -9, 1, 7, 4, 2, 20, 1, 1, -2, -2, 2, 9, 9, -20, -150, -20,
                       8, 17, 1, 5, -2, 2, -1, -50, -25, 1, 4, 2, 5, 3, -1, 2, -1, -20,
                       -1, 4, 4),
             Beta = c(4, 7, 5, 3, 13, 10, 4, -5, 1, 5, 2, 4, 1,
                      -12, -10, 3, 4, 1, 6, -3, -2, 1, 1, 4, 24, 19, -1, 0, -2, 5,
                      2, 5, 1, 2, 5, 3, 2, -9, 3, 1, 2, 1, -1, -20, 2),
             Gamma = c(800, 11, 4, 20, 0, -523, 2, 3, 27, 9, -2, 4, -15, 3, 4, 4, 3, 7, 10,
                       -3, 2, 6, 20, 12, 13, -4, 105, -9, 5, 2, 3, 3, -10, 0, -1, 4,
                       3, 1, -500, 6, 4, 405, 0, 1, 31)), row.names = c(NA, -45L), class = "data.frame"),
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
                       dates = as.Date(structure(c(984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400, 984614400,
                                           987292800, 989884800, 992563200, 995155200, 997833600, 1000512000,
                                           1003104000, 1005782400, 984614400, 987292800, 989884800, 992563200,
                                           995155200, 997833600, 1000512000, 1003104000, 1005782400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                       format = "%Y-%m-%d"),
                       fwd_premium_1m = c(0, 6, 7, 1, 2, 1, 10, 3, 1, 1, 8, 2, 3, 5, -1, 35, -152, 3, 2, 3, 7, 5, 1, -9,
                                          2, 4, -20, 8, 8, 8, 7, 2, -2, -10, -45, -3, 5, 1, 8, 1, 2, 1, 4, -5, 0),
                       fwd_premium_3m = c(4, 4, 2, 0, 6, 5, -5, -1, 4, 5, 3, 7, 3, 8, 2, 5, 1, 2, 0, 5, 2, 8, 3, 5, 3, 40, 2, 1, 3, 8,
                                          3, 1, 1, 11, 4, 2, 9, 9, 1, 2, 3, -9, -4, 4, 3),
                       fwd_sharpe_1m = c(7,  7, 3, 1, 1, 3, 1, 0, 10, 4, 2, 8, 5, 4, 1, 1, 4, -5, 2, 6, 4,  6, 5, 1, 1, 5, 3, 4, 9, 0,
                                         10, 1, 4, 12, 1, 92, 7, 1, 3, 3, 0, 1, 3, 1, 9)), row.names = c(NA, -45L), class = "data.frame"),
      training_sample_size = 4,
      validation_sample_size = 3,
      rebalancing_months = 11,
      ml_algorithm = "glmnet",
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "rmse",
      hyper_grid_domain = list(alpha = list(distribution_choice = "uniform", pars = c(min = 0,max = 1)),
                                    lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.99))), #Random Search
      tuning_method = c("random_search"),
      n_iter = 5,
      verbose = FALSE,
      parallel = FALSE,
      show_plots = FALSE
    )}))

  #Define initial objects
  set.seed(123)
  hyper_expanded_grid1 <- list(alpha = runif(n = 5, min = 0, max = 1), lambda.min.ratio = runif(n = 5, min = 0.1, max = 0.99))
  hyper_expanded_grid1 <- expand.grid(alpha = hyper_expanded_grid1$alpha, lambda.min.ratio = hyper_expanded_grid1$lambda.min.ratio)



  validation_eval_hyper_choice <- data.frame(rss =c(NA, NA),  #Validation loss df
                                             cp = c(NA, NA),
                                             rmse = c(NA, NA),
                                             mae = c(NA, NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             row.names = c("2001-09-15", "2001-11-15"))
  rebalance_dates <- c("2001-09-15", "2001-11-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #Start first rebalancing

  #Get objects to train and validate model
  features_training <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                             "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(984614400, 984614400, 984614400, 984614400,
                         984614400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(3,  1, 2, 5, 5), Beta = c(4, 5, 6, 0, 2), Gamma = c(800, 9, 10, -9, 3)), row.names = c(NA, -5L), class = "data.frame")

  target_training <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5),
                                    fwd_premium_3m = c(4, 5, 0, 1, 9),
                                    fwd_sharpe_1m = c(7, 4, 2, 4, 7)), row.names = c(NA, -5L), class = "data.frame")

  features_validation <- structure(list(id = c("Stock A-2001-06-15", "Stock B-2001-06-15",
                                               "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(992563200, 992563200, 992563200, 992563200,
                         992563200), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(5,  2, -20, -1, 2), Beta = c(3, 1, 1, 2, 1), Gamma = c(20, -15, 6,  3, 6)), row.names = c(NA, -5L), class = "data.frame")

  target_validation <- structure(list(fwd_premium_1m = c(1, 3, 5, 7, 1),
                                      fwd_premium_3m = c(0, 3, 8, 3, 2),
                                      fwd_sharpe_1m = c(1, 5, 6, 10, 3)), row.names = c(NA,  -5L), class = "data.frame")



  #Start first rebalancing
  chosen_eval_metric_val[[1]] <- data.frame(alpha = hyper_expanded_grid1$alpha,
                                            lambda.min.ratio = hyper_expanded_grid1$lambda.min.ratio,
                                            best_lam = rep(NA,25), chosen_eval_metric = rep(NA, 25))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid1)))

  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]])
  best_lam1 <- vector(length =  nrow(hyper_expanded_grid1))

  for(s in 1:length(hyper_expanded_grid1$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_3m,
      alpha = hyper_expanded_grid1$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid1$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam1[s] <- glm.mod1$lambda[
      which.min(sqrt(colMeans((target_validation$fwd_premium_3m - predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)])))^2)))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <- predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam1[s])

    #RMSE CHOSEN
    chosen_eval_metric_val[[1]]$chosen_eval_metric[which(chosen_eval_metric_val[[1]]$alpha == unique(hyper_expanded_grid1$alpha)[s])] <-
      sqrt(mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,s])^2))



  }

  chosen_eval_metric_val[[1]]$best_lam <- best_lam1


  #RMSE IS MIN: PAY ATTENTION
  hyper_choice1 <- which.min(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((target_validation$fwd_premium_3m - shrinkage.pred_df[, hyper_choice1])^2)/sum(target_validation$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(target_validation$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1)^2*(sqrt(1+((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                         0.5*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                         (1-0.5)*(-1)*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                     target_validation$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- mean((target_validation$fwd_premium_3m * shrinkage.pred_df[,hyper_choice1]) > 0)

  validation_eval_hyper_choice$mb[1] <- mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))





  #Refit
  features_training_and_validation <-structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                                           "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                                           "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                                           "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                                           "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                                           "Stock E-2001-05-15", "Stock A-2001-06-15", "Stock B-2001-06-15",
                                                           "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E",
                 "Stock A", "Stock B", "Stock C", "Stock D", "Stock E", "Stock A",
                 "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                 "Stock C", "Stock D", "Stock E"), dates = structure(c(984614400,
                                                                       984614400, 984614400, 984614400, 984614400, 987292800, 987292800,
                                                                       987292800, 987292800, 987292800, 989884800, 989884800, 989884800,
                                                                       989884800, 989884800, 992563200, 992563200, 992563200, 992563200,
                                                                       992563200), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1, 5, 2, -20, -1, 2), Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3,  3, 1, 1, 2, 1),
  Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500, 20, -15, 6, 3, 6)), row.names = c(NA, -20L), class = "data.frame")

  target_training_and_validation <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                                      1, 7, 2, 7, 8, 8, 1, 3, 5, 7, 1),
                                                   fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1, 0, 3, 8, 3, 2),
                                                   fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3, 1, 5, 6, 10, 3)),
                                              row.names = c(NA, -20L), class = "data.frame")


  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid1$alpha[hyper_choice1],
                                  lambda = hyper_expanded_grid1$lambda[hyper_choice1])
  coef(glm.mod.refit)

  #2nd Rebalancing

  #Get objects to train and validate model
  features_training <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                             "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                             "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                             "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                             "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                             "Stock E-2001-05-15"),
                                      tickers = c("Stock A", "Stock B", "Stock C",
                                                  "Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                  "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
                                      ),
                                      dates = structure(c(984614400, 984614400, 984614400, 984614400,
                                                          984614400, 987292800, 987292800, 987292800, 987292800, 987292800,
                                                          989884800, 989884800, 989884800, 989884800, 989884800), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                      Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1), Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3),
                                      Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500)), row.names = c(NA, -15L), class = "data.frame")


  target_training <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                       1, 7, 2, 7, 8, 8),
                                    fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1),
                                    fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3)), row.names = c(NA, -15L), class = "data.frame")

  features_validation <- structure(list(id = c("Stock A-2001-08-15", "Stock B-2001-08-15",
                                               "Stock C-2001-08-15", "Stock D-2001-08-15", "Stock E-2001-08-15"
  ), tickers = c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E"
  ), dates = structure(c(997833600, 997833600, 997833600, 997833600,
                         997833600), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  Alpha = c(1, 1, -20, -25, -20), Beta = c(10, -10, 4, 1, 1), Gamma = c(-523, 4, 12, -10, 405)), row.names = c(NA, -5L), class = "data.frame")

  target_validation <-structure(list(fwd_premium_1m = c(1, -1, -9, -2, 1),
                                     fwd_premium_3m = c(5, 2, 5, 1, -9),
                                     fwd_sharpe_1m = c(3, 1, 1, 4, 1)), row.names = c(NA,  -5L), class = "data.frame")

  hyper_expanded_grid2 <- list(alpha = runif(n = 5, min = 0, max = 1), lambda.min.ratio = runif(n = 5, min = 0.1, max = 0.99))
  hyper_expanded_grid2 <- expand.grid(alpha = hyper_expanded_grid2$alpha, lambda.min.ratio = hyper_expanded_grid2$lambda.min.ratio)

  #Start first rebalancing
  chosen_eval_metric_val[[2]] <- data.frame(alpha = hyper_expanded_grid2$alpha,
                                            lambda.min.ratio = hyper_expanded_grid2$lambda.min.ratio,
                                            best_lam = rep(NA,25), chosen_eval_metric = rep(NA, 25))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(target_validation$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid2)))

  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]])
  best_lam2 <- vector(length =  nrow(hyper_expanded_grid2))

  for(s in 1:length(hyper_expanded_grid2$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_training[,-c(1:3)],
      y = target_training$fwd_premium_3m,
      alpha = hyper_expanded_grid2$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid2$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam2[s] <- glm.mod1$lambda[
      which.min(sqrt(colMeans((target_validation$fwd_premium_3m - predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)])))^2)))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <- predict(glm.mod1, newx = as.matrix(features_validation[,-c(1:3)]), s = best_lam2[s])

    #RMSE CHOSEN
    chosen_eval_metric_val[[2]]$chosen_eval_metric[which(chosen_eval_metric_val[[2]]$alpha == unique(hyper_expanded_grid2$alpha)[s])] <-
      sqrt(mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,s])^2))



  }

  chosen_eval_metric_val[[2]]$best_lam <- best_lam2



  #RMSE IS MIN: PAY ATTENTION
  hyper_choice2 <- which.min(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(target_validation$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(target_validation$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))

  validation_eval_hyper_choice$mphe[2] <- mean((1)^2*(sqrt(1+((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                         0.5*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                         (1-0.5)*(-1)*(target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))


  validation_eval_hyper_choice$mape[2] <- mean(abs((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                     target_validation$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- mean((target_validation$fwd_premium_3m * shrinkage.pred_df[,hyper_choice2]) > 0)

  validation_eval_hyper_choice$mb[2] <- mean((target_validation$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))


  #Refit
  features_training_and_validation <- structure(list(id = c("Stock A-2001-03-15", "Stock B-2001-03-15",
                                                            "Stock C-2001-03-15", "Stock D-2001-03-15", "Stock E-2001-03-15",
                                                            "Stock A-2001-04-15", "Stock B-2001-04-15", "Stock C-2001-04-15",
                                                            "Stock D-2001-04-15", "Stock E-2001-04-15", "Stock A-2001-05-15",
                                                            "Stock B-2001-05-15", "Stock C-2001-05-15", "Stock D-2001-05-15",
                                                            "Stock E-2001-05-15", "Stock A-2001-06-15", "Stock B-2001-06-15",
                                                            "Stock C-2001-06-15", "Stock D-2001-06-15", "Stock E-2001-06-15",
                                                            "Stock A-2001-07-15", "Stock B-2001-07-15", "Stock C-2001-07-15",
                                                            "Stock D-2001-07-15", "Stock E-2001-07-15", "Stock A-2001-08-15",
                                                            "Stock B-2001-08-15", "Stock C-2001-08-15", "Stock D-2001-08-15",
                                                            "Stock E-2001-08-15"),
                                                     tickers = c("Stock A", "Stock B", "Stock C","Stock D", "Stock E", "Stock A", "Stock B", "Stock C", "Stock D",
                                                                 "Stock E", "Stock A", "Stock B", "Stock C", "Stock D", "Stock E",
                                                                 "Stock A", "Stock B", "Stock C", "Stock D", "Stock E", "Stock A",
                                                                 "Stock B", "Stock C", "Stock D", "Stock E", "Stock A", "Stock B",
                                                                 "Stock C", "Stock D", "Stock E"),
                                                     dates = structure(c(984614400, 984614400, 984614400, 984614400, 984614400, 987292800, 987292800,
                                                                         987292800, 987292800, 987292800, 989884800, 989884800, 989884800,
                                                                         989884800, 989884800, 992563200, 992563200, 992563200, 992563200,
                                                                         992563200, 995155200, 995155200, 995155200, 995155200, 995155200,
                                                                         997833600, 997833600, 997833600, 997833600, 997833600), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                                     Alpha = c(3, 1, 2, 5, 5, -20, 7, 9, -2, 3, -450, 4, 9, 2, -1, 5, 2, -20, -1, 2, -2, 20, -150, -50, -1, 1, 1, -20, -25, -20),
                                                     Beta = c(4, 5, 6, 0, 2, 7, 2, -3, -2, -9, 5, 4, -2, 5, 3, 3, 1, 1, 2, 1, 13, -12, 1, 5, 2, 10, -10, 4, 1, 1),
                                                     Gamma = c(800, 9, 10, -9, 3, 11, -2, -3, 5, 1, 4, 4, 2, 2, -500, 20, -15, 6, 3, 6, 0, 3, 20, 3, 4, -523, 4, 12, -10, 405)),
                                                row.names = c(NA, -30L), class = "data.frame")

  target_training_and_validation <- structure(list(fwd_premium_1m = c(0, 1, 2, 8, 5, 6, 8, 3, 8,
                                                                      1, 7, 2, 7, 8, 8, 1, 3, 5, 7, 1, 2, 5, 1, 2, 2, 1, -1, -9, -2,
                                                                      1),
                                                   fwd_premium_3m = c(4, 5, 0, 1, 9, 4, 3, 5, 3, 9, 2, 7, 2, 8, 1, 0, 3, 8, 3, 2, 6, 8, 3, 1, 3, 5, 2, 5, 1, -9),
                                                   fwd_sharpe_1m = c(7, 4, 2, 4, 7, 7, 2, 6, 9, 1, 3, 8, 4, 0, 3, 1, 5, 6, 10, 3, 1,  4, 5, 1, 0, 3, 1, 1, 4, 1)),
                                              row.names = c(NA, -30L), class = "data.frame")

  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid2$alpha[hyper_choice2],
                                  lambda = hyper_expanded_grid2$lambda[hyper_choice2])


  coef(glm.mod.refit)

  glm.mod.refit <- glmnet::glmnet(x = features_training_and_validation[,-c(1:3)],
                                  y = target_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid2$alpha[hyper_choice2],
                                  lambda.min.ratio = hyper_expanded_grid2$lambda.min.ratio[hyper_choice2])


  coef(glm.mod.refit)


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")
  #Pred list
  prediction_list <- list(`2001-09-15` = c(`Stock A` = 3.95, `Stock B` = 3.95, `Stock C` = 3.95,
                                           `Stock D` = 3.95, `Stock E` = 3.95), `2001-10-15` = c(`Stock A` = 3.95,
                                                                                                 `Stock B` = 3.95, `Stock C` = 3.95, `Stock D` = 3.95, `Stock E` = 3.95
                                           ), `2001-11-15` = c(`Stock A` = 3.466667, `Stock B` = 3.466667,
                                                               `Stock C` = 3.466667, `Stock D` = 3.466667, `Stock E` = 3.466667
                                           ))
  results$outputs[[1]] <- prediction_list
  #Error list
  error_list <- list(`2001-09-15` = c(`Stock A` = -8.95, `Stock B` = 1.05, `Stock C` = -0.95,
                                      `Stock D` = 7.05, `Stock E` = -7.95), `2001-10-15` = c(`Stock A` = -4.95,
                                                                                             `Stock B` = -2.95, `Stock C` = 36.05, `Stock D` = 0.0499999999999998,
                                                                                             `Stock E` = 0.0499999999999998), `2001-11-15` = c(`Stock A` = 0.533333,
                                                                                                                                               `Stock B` = -1.466667, `Stock C` = -1.466667, `Stock D` = -1.466667,
                                                                                                                                               `Stock E` = -0.466667))
  results$outputs[[2]] <- error_list
  #Y-list
  y_list <- list(`2001-09-15` = c(`Stock A` = -5, `Stock B` = 5, `Stock C` = 3,
                                  `Stock D` = 11, `Stock E` = -4), `2001-10-15` = c(`Stock A` = -1,
                                                                                    `Stock B` = 1, `Stock C` = 40, `Stock D` = 4, `Stock E` = 4),
                 `2001-11-15` = c(`Stock A` = 4, `Stock B` = 2, `Stock C` = 2,
                                  `Stock D` = 2, `Stock E` = 3))
  results$outputs[[3]] <- y_list
  #Eval metrics
  oos_testing_eval_metrics <-structure(list(rss =c(0.0050382653061225, 0.184325275397797,
                                                         0.812011933933919), cp = c(7.9, 37.92, 9.0133342),
                                            rmse = c(6.24519815538306, 16.3267418672557, 1.17945397913145
                                            ), mae = c(5.19, 8.81, 1.0800002),
                                            mphe = c(4.39364382, 8.2462498, 0.51245492),
                                            mpe = c(2.595, 4.405, 0.54),
                                            mape = c(0.98902, 1.76525, 0.49778),
                                            hr = c(0.6, 0.8, 1.0),
                                            mb = c(-1.95, 5.65, -0.86667))
                                       , class = "data.frame", row.names = c("2001-09-15", "2001-10-15", "2001-11-15"))
  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(glm.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }


  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- c("2001-09-15", "2001-11-15")
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = c("2001-09-15", "2001-11-15"),
                                     alpha = c(hyper_expanded_grid1$alpha[hyper_choice1], hyper_expanded_grid2$alpha[hyper_choice2]),
                                     lambda.min.ratio = c(hyper_expanded_grid1$lambda.min.ratio[hyper_choice1], hyper_expanded_grid2$lambda.min.ratio[hyper_choice2]),
                                     best_lam = c(best_lam1[hyper_choice1], best_lam2[hyper_choice2]))

  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL

  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-5
  )

})

#Define your test
test_that("RF (Parallel) - run_ml_backtest works with rebalancing, 3m target, random_search (uniform and lognormal) as tuning method and rmse as chosen eval metric -toy_preprocessed_features_and_targets",{

  future::plan("multisession")
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  set.seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      ml_algorithm = "rf",
      n_iter = 3,
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "rmse",
      hyper_grid_domain = list(mtry = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 1)),
                                    num.trees = list(distribution_choice = "lognormal", pars = c(meanlog = 6L, sdlog = 1L)),
                                    max.depth = list(distribution_choice = "uniform", pars = c(min = 2L, max = 8L)),
                                    min.bucket = list(distribution_choice = "uniform", pars = c(min = 1, max = 10))),

      tuning_method = c("random_search"),
      huber_delta = 1.5,
      verbose = FALSE,
      show_plots = FALSE
    )}))


  #Define initial objects
  set.seed(123)
  hyper_expanded_grid1 <- list(mtry = runif(n = 3, min = 0.1, max = 1), num.trees = round(rlnorm(n = 3, meanlog = 6L, sdlog = 1L), 0),
                               max.depth = round(runif(n = 3, min = 2, max = 8),0), min.bucket = runif(n = 3, min = 1, max = 10))


  hyper_expanded_grid1 <- lapply(hyper_expanded_grid1, function(x) unique(x))

  hyper_expanded_grid1 <- expand.grid(mtry = hyper_expanded_grid1$mtry, num.trees = hyper_expanded_grid1$num.trees,
                                      max.depth = hyper_expanded_grid1$max.depth, min.bucket = hyper_expanded_grid1$min.bucket)



  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]
  #Full data
  full_data_first_train <- cbind(targets_first_train$fwd_premium_3m, features_first_train[,-c(1:3)])
  colnames(full_data_first_train)[1] <- c("fwd_premium_3m")

  #Features val
  chosen_eval_metric_val[[1]] <- data.frame(mtry = hyper_expanded_grid1$mtry, num.trees = hyper_expanded_grid1$num.trees,
                                              max.depth = hyper_expanded_grid1$max.depth, min.bucket = hyper_expanded_grid1$min.bucket,
                                              chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid1)))


  #Use foreach to simulate result of parallelized hyper tuning
  first_rebal <-
    foreach::foreach(s = 1:nrow(hyper_expanded_grid1), .options.future = list(seed = TRUE)) %dofuture% {
      #Train Model
      rf.mod1 <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_first_train),
                                mtry = hyper_expanded_grid1$mtry[s] * (ncol(full_data_first_train) - 1),
                                num.trees = hyper_expanded_grid1$num.trees[s],
                                max.depth = hyper_expanded_grid1$max.depth[s],
                                min.bucket = hyper_expanded_grid1$min.bucket[s]
      )

      out <- data.frame(matrix(NA, nrow = length(targets_first_val$fwd_premium_3m),
                               ncol = nrow(hyper_expanded_grid1)))


      #Predict vlidation data
      out[,s] <-
        stats::predict(rf.mod1, data = janitor::clean_names(features_first_val[,-c(1:3)]))$predictions


      #RMSE CHOSEN
      return(list(predictions = out[,s],
                  metric = sqrt(mean(
                    (out[,s] - targets_first_val$fwd_premium_3m)^2
                  ))))

    }


  #Pass objects
  shrinkage.pred_df <- sapply(first_rebal, function(x) as.numeric(x$predictions)) #Transform to df
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]]) #chance colnames

  chosen_eval_metric_val[[1]]$chosen_eval_metric <- sapply(first_rebal, function(x) x$metric)

  #old sequential code
  #shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(targets_first_val$fwd_premium_3m),
  #                                       ncol = nrow(hyper_expanded_grid1)))

  #colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]])

  #for(s in 1:nrow(hyper_expanded_grid1)){
  #Train Model
  #  rf.mod1 <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_first_train),
  #                            mtry = hyper_expanded_grid1$mtry[s] * (ncol(full_data_first_train) - 1),
  #                            num.trees = hyper_expanded_grid1$num.trees[s],
  #                            max.depth = hyper_expanded_grid1$max.depth[s],
  #                            min.bucket = hyper_expanded_grid1$min.bucket[s])

  #Predict to validation data

  # shrinkage.pred_df[,s] <-
  #   stats::predict(rf.mod1, data = janitor::clean_names(features_first_val[,-c(1:3)]))$predictions


  #CROSSPRODUCT CHOSEN
  # chosen_eval_metric_val[[1]]$chosen_eval_metric[s] <- sqrt(mean((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,s])^2))

  # }


  #rmse IS Min: PAY ATTENTION
  hyper_choice1 <- which.min(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(targets_first_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(targets_first_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean(
    (1.5)^2*(sqrt(1+((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/(1.5))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                         0.5*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                         (1-0.5)*(-1)*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/targets_first_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(targets_first_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice1])))/
    length(targets_first_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[1] <- mean(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])


  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]

  #Full data
  full_data_first_training_and_validation <- cbind(target_first_training_and_validation$fwd_premium_3m, features_first_training_and_validation[,-c(1:3)])
  colnames(full_data_first_training_and_validation)[1] <- c("fwd_premium_3m")

  #Refitted model
  rf.mod.refit <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_first_training_and_validation),
                                 mtry = hyper_expanded_grid1$mtry[hyper_choice1] * (ncol(full_data_first_training_and_validation) - 1),
                                 num.trees = hyper_expanded_grid1$num.trees[hyper_choice1],
                                 max.depth = hyper_expanded_grid1$max.depth[hyper_choice1],
                                 min.bucket = hyper_expanded_grid1$min.bucket[hyper_choice1])


  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(predict(rf.mod.refit,
                                             data = janitor::clean_names(
                                               features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)]))$predictions)
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]

  prediction_list[[2]] <- as.numeric(predict(rf.mod.refit,
                                             data = janitor::clean_names(
                                               features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)]))$predictions)
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  hyper_expanded_grid2 <- list(mtry = runif(n = 3, min = 0.1, max = 1), num.trees = round(rlnorm(n = 3, meanlog = 6, sdlog = 1),0),
                               max.depth = round(runif(n = 3, min = 2, max = 8),0), min.bucket = runif(n = 3, min = 1, max = 10))

  hyper_expanded_grid2 <- lapply(hyper_expanded_grid2, function(x) unique(x))

  hyper_expanded_grid2 <- expand.grid(mtry = hyper_expanded_grid2$mtry, num.trees = hyper_expanded_grid2$num.trees,
                                      max.depth = hyper_expanded_grid2$max.depth, min.bucket = hyper_expanded_grid2$min.bucket)


  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]

  #Full data
  full_data_second_train <- cbind(targets_second_train$fwd_premium_3m, features_second_train[,-c(1:3)])
  colnames(full_data_second_train)[1] <- c("fwd_premium_3m")

  #Features val
  chosen_eval_metric_val[[2]] <- data.frame(mtry = hyper_expanded_grid2$mtry, num.trees = round(hyper_expanded_grid2$num.trees,0),
                                              max.depth = round(hyper_expanded_grid2$max.depth,0), min.bucket = hyper_expanded_grid2$min.bucket,
                                              chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid2)))


  #Use foreach to simulate result of parallelized hyper tuning
  second_rebal <-
    foreach::foreach(s = 1:nrow(hyper_expanded_grid2), .options.future = list(seed = TRUE)) %dofuture% {
      #Train Model
      rf.mod2 <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_second_train),
                                mtry = hyper_expanded_grid2$mtry[s] * (ncol(full_data_second_train) - 1),
                                num.trees = hyper_expanded_grid2$num.trees[s],
                                max.depth = hyper_expanded_grid2$max.depth[s],
                                min.bucket = hyper_expanded_grid2$min.bucket[s]
      )

      out <- data.frame(matrix(NA, nrow = length(targets_second_val$fwd_premium_3m),
                               ncol = nrow(hyper_expanded_grid2)))


      #Predict vlidation data
      out[,s] <-
        stats::predict(rf.mod2, data = janitor::clean_names(features_second_val[,-c(1:3)]))$predictions


      #RMSE CHOSEN
      return(list(predictions = out[,s],
                  metric = sqrt(mean(
                    (out[,s] - targets_second_val$fwd_premium_3m)^2
                  ))))

    }


  #Pass objects
  shrinkage.pred_df <- sapply(second_rebal, function(x) as.numeric(x$predictions)) #Transform to df
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]]) #chance colnames
  chosen_eval_metric_val[[2]]$chosen_eval_metric <- sapply(second_rebal, function(x) x$metric)

  #RMSE IS MIN: PAY ATTENTION
  hyper_choice2 <- which.min(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(targets_second_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(targets_second_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))

  validation_eval_hyper_choice$mphe[2] <- mean((1.5)^2*(sqrt(1+((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                          (1.5))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                         0.5*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                         (1-0.5)*(-1)*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs(
    (targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/targets_second_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- length(which(sign(targets_second_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice2])))/
    length(targets_second_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[2] <- mean(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])


  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]

  #Full data
  full_data_second_training_and_validation <- cbind(target_second_training_and_validation$fwd_premium_3m, features_second_training_and_validation[,-c(1:3)])
  colnames(full_data_second_training_and_validation)[1] <- c("fwd_premium_3m")


  #Refitted model
  rf.mod.refit <- ranger::ranger(fwd_premium_3m~., data = janitor::clean_names(full_data_second_training_and_validation),
                                 mtry = hyper_expanded_grid2$mtry[hyper_choice2] * (ncol(full_data_second_training_and_validation) - 1),
                                 num.trees = hyper_expanded_grid2$num.trees[hyper_choice2],
                                 max.depth = hyper_expanded_grid2$max.depth[hyper_choice2],
                                 min.bucket = hyper_expanded_grid2$min.bucket[hyper_choice2])



  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]



  #Predict!
  prediction_list[[3]] <- as.numeric(predict(rf.mod.refit,
                                             data = janitor::clean_names(
                                               features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)]))$predictions)
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]

  prediction_list[[4]] <- as.numeric(predict(rf.mod.refit,
                                             data = janitor::clean_names(
                                               features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)]))$predictions)
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss =c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA),
                                         mphe = c(NA,NA,NA,NA),
                                         mpe = c(NA,NA,NA,NA),
                                         row.names =   c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(1.5^2*(sqrt(1+((y_list[[l]] - prediction_list[[l]])/1.5)^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0,
                                                       0.5*(y_list[[l]] - prediction_list[[l]]),
                                                       (1-0.5)*(-1)*(y_list[[l]] - prediction_list[[l]])))

    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))
    oos_testing_eval_metrics$hr[l] <- mean((y_list[[l]] * prediction_list[[l]])>=0)
    oos_testing_eval_metrics$mb[l] <- mean((y_list[[l]] - prediction_list[[l]]))



  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  results$outputs[[5]] <- ml_backtest_results@final_model


  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     mtry = c(hyper_expanded_grid1$mtry[hyper_choice1], hyper_expanded_grid2$mtry[hyper_choice2]),
                                     num.trees = c(hyper_expanded_grid1$num.trees[hyper_choice1], hyper_expanded_grid2$num.trees[hyper_choice2]),
                                     max.depth = c(hyper_expanded_grid1$max.depth[hyper_choice1], hyper_expanded_grid2$max.depth[hyper_choice2]),
                                     min.bucket = c(hyper_expanded_grid1$min.bucket[hyper_choice1], hyper_expanded_grid2$min.bucket[hyper_choice2]))


  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")


  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL

  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-5
  )

  future::plan("sequential")

})

#Define your test
test_that("GLMNET - run_ml_backtest works with rebalancing, 3m target, random_search as tuning method and rss as chosen eval metric -toy_preprocessed_features_and_targets",{

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  set.seed(123)
  hyper_grid_domain <- create_hyper_grid_domain(tuning_method = "random_search", ml_algorithm = "glmnet")
  hyper_grid_domain <- add_hyperparameter(hyper_grid_domain,
                                          new_hyperparameters = list(alpha = list(distribution_choice = "uniform", pars = c(min = 0,max = 1)),
                                                                     lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.9))))

  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      ml_algorithm = "glmnet",
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "rss",
      hyper_grid_domain = hyper_grid_domain, #Random Search
      tuning_method = c("random_search"),
      n_iter = 5,
      parallel = FALSE,
      verbose = FALSE,
      show_plots = FALSE
    )}))


  #Define initial objects
  set.seed(123)
  hyper_expanded_grid1 <- list(alpha = runif(n = 5, min = 0, max = 1), lambda.min.ratio = runif(n = 5, min = 0.1, max = 0.9))
  hyper_expanded_grid1$alpha <- unique(hyper_expanded_grid1$alpha)
  hyper_expanded_grid1$lambda.min.ratio <- unique(hyper_expanded_grid1$lambda.min.ratio)
  hyper_expanded_grid1 <- expand.grid(hyper_expanded_grid1)

  hyper_expanded_grid2 <- list(alpha = runif(n = 5, min = 0, max = 1), lambda.min.ratio = runif(n = 5, min = 0.1, max = 0.9))
  hyper_expanded_grid2$alpha <- unique(hyper_expanded_grid2$alpha)
  hyper_expanded_grid2$lambda.min.ratio <- unique(hyper_expanded_grid2$lambda.min.ratio)
  hyper_expanded_grid2 <- expand.grid(hyper_expanded_grid2)


  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]
  #Features val
  #Start first rebalancing
  chosen_eval_metric_val[[1]] <- data.frame(alpha = hyper_expanded_grid1$alpha,
                                            lambda.min.ratio = hyper_expanded_grid1$lambda.min.ratio,
                                            best_lam = rep(NA,25), chosen_eval_metric = rep(NA, 25))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(targets_first_val$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid1)))

  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]])
  best_lam1 <- vector(length =  nrow(hyper_expanded_grid1))

  for(s in 1:length(hyper_expanded_grid1$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_first_train[,-c(1:3)],
      y = targets_first_train$fwd_premium_3m,
      alpha = hyper_expanded_grid1$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid1$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam1[s] <- glm.mod1$lambda[
      which.max(1 - (colSums((targets_first_val$fwd_premium_3m -
                                predict(glm.mod1, newx = as.matrix(features_first_val[,-c(1:3)])))^2)/sum(targets_first_val$fwd_premium_3m^2)))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_first_val[,-c(1:3)]), s = best_lam1[s])

    #RSQUARED CHOSEN
    chosen_eval_metric_val[[1]]$chosen_eval_metric[s] <-
      (1 - (sum((targets_first_val$fwd_premium_3m -
                   shrinkage.pred_df[,s])^2)/sum(targets_first_val$fwd_premium_3m^2)))



  }
  chosen_eval_metric_val[[1]]$best_lam <- best_lam1

  #rsquared IS MAX: PAY ATTENTION
  hyper_choice1 <- which.max(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(targets_first_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(targets_first_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1)^2*(sqrt(1+((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                         0.5*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                         (1-0.5)*(-1)*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/targets_first_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(targets_first_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice1])))/
    length(targets_first_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[1] <- mean(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])




  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_first_training_and_validation[,-c(1:3)],
                                  y = target_first_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid1$alpha[hyper_choice1],
                                  lambda.min.ratio = hyper_expanded_grid1$lambda.min.ratio[hyper_choice1])
  coef(glm.mod.refit)


  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(predict(glm.mod.refit, newx = as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)]),
                                             s = best_lam1[hyper_choice1]))
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  prediction_list[[2]] <- as.numeric(predict(glm.mod.refit, newx = as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)]),
                                     s = best_lam1[hyper_choice1]))
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]



  chosen_eval_metric_val[[2]] <- data.frame(alpha = hyper_expanded_grid2$alpha,
                                            lambda.min.ratio = hyper_expanded_grid2$lambda.min.ratio,
                                            best_lam = rep(NA,25), chosen_eval_metric = rep(NA, 25))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(targets_second_val$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid2)))

  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]])
  best_lam2 <- vector(length =  nrow(hyper_expanded_grid2))

  for(s in 1:length(hyper_expanded_grid2$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_second_train[,-c(1:3)],
      y = targets_second_train$fwd_premium_3m,
      alpha = hyper_expanded_grid2$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid2$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam2[s] <- glm.mod1$lambda[
      which.max(1 - (colSums((targets_second_val$fwd_premium_3m -
                                predict(glm.mod1, newx = as.matrix(features_second_val[,-c(1:3)])))^2)/sum(targets_second_val$fwd_premium_3m^2)))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_second_val[,-c(1:3)]), s = best_lam2[s])

    #RSQUARED CHOSEN
    chosen_eval_metric_val[[2]]$chosen_eval_metric[s] <-
      (1 - (sum((targets_second_val$fwd_premium_3m -
                   shrinkage.pred_df[,s])^2)/sum(targets_second_val$fwd_premium_3m^2)))



  }
  chosen_eval_metric_val[[2]]$best_lam <- best_lam2

  #r2 IS MAX: PAY ATTENTION
  hyper_choice2 <- which.max(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(targets_second_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(targets_second_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))

  validation_eval_hyper_choice$mphe[2] <- mean((1)^2*(sqrt(1+((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                        (1))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                         0.5*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                         (1-0.5)*(-1)*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs(
    (targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/targets_second_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- length(which(sign(targets_second_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice2])))/
    length(targets_second_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[2] <- mean(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])



  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]


  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_second_training_and_validation[,-c(1:3)],
                                  y = target_second_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid2$alpha[hyper_choice2],
                                  lambda.min.ratio = hyper_expanded_grid2$lambda.min.ratio[hyper_choice2])
  coef(glm.mod.refit)



  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]



  #Predict!
  prediction_list[[3]] <- as.numeric(predict(glm.mod.refit, newx = as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)]),
                                             s = best_lam2[hyper_choice2]))
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  prediction_list[[4]] <- as.numeric(predict(glm.mod.refit, newx = as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)]),
                                             s = best_lam2[hyper_choice2]))
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss =c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA), row.names = c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(1^2*(sqrt(1+(y_list[[l]] - prediction_list[[l]])^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0,
                                                       0.5*(y_list[[l]] - prediction_list[[l]]),
                                                       (1-0.5)*(-1)*(y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))
    oos_testing_eval_metrics$hr[l] <- mean((y_list[[l]] * prediction_list[[l]])>0)
    oos_testing_eval_metrics$mb[l] <- mean(y_list[[l]] - prediction_list[[l]])

  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  results$outputs[[5]] <- ml_backtest_results@final_model


  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     alpha = c(hyper_expanded_grid1$alpha[hyper_choice1], hyper_expanded_grid2$alpha[hyper_choice2]),
                                     lambda.min.ratio = c(hyper_expanded_grid1$lambda.min.ratio[hyper_choice1], hyper_expanded_grid2$lambda.min.ratio[hyper_choice2]),
                                     best_lam = c(best_lam1[hyper_choice1], best_lam2[hyper_choice2]))

  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics",
                              "final_model", "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-5
  )

})

#Define your test
test_that("XGB (Parallel) - run_ml_backtest works with rebalancing, 3m target, random as tuning method, rmse as chosen eval metric -toy_preprocessed_features_and_targets",{

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  suppressWarnings({
    future::plan("multisession")
  })

  set.seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      ml_algorithm = "xgb",
      custom_objective = "squared_error",
      target_fwd_name = c("fwd_premium_3m"),
      hyper_grid_domain_list =
        list(min_child_weight = list(distribution_choice = "constant", value = 3),
             max_depth = list(distribution_choice = "uniform", pars = c(min = 1L, max = 2L)),
             subsample = list(distribution_choice = "uniform", pars = c(min = 0.25, max = 0.50)),
             colsample_bytree = list(distribution_choice = "uniform", pars = c(min = 0.25, max = 0.50)),
             eta = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.2)),
             alpha = list(distribution_choice = "uniform", pars = c(min = 2, max = 5)),
             gamma = list(distribution_choice = "constant", value = 0),
             nrounds = list(distribution_choice = "uniform", pars = c(min = 200L, max = 500L))
        ),
      tuning_method = c("random_search"),
      n_iter = 2,
      huber_delta = 1.3,
      early_stop = 25,
      verbose = FALSE,
      show_plots = FALSE
    )}))


  #Define initial objects
  set.seed(123)
  hyper_expanded_grid1 <-  list(min_child_weight = c(3),
                                max_depth = runif(2, 1, 2),
                                subsample = runif(2, 0.25, 0.50),
                                colsample_bytree = runif(2, 0.25, 0.50),
                                eta = runif(2, 0.1, 0.2),
                                alpha = runif(2, 2, 5),
                                gamma = 0,
                                nrounds = runif(2, 200, 500)
  )

  hyper_expanded_grid1$nrounds <- round(hyper_expanded_grid1$nrounds, 0)
  hyper_expanded_grid1$max_depth <- round(hyper_expanded_grid1$max_depth, 0)

  hyper_expanded_grid1 <- lapply(hyper_expanded_grid1, function(x) unique(x))

  hyper_expanded_grid1 <- expand.grid(min_child_weight = hyper_expanded_grid1$min_child_weight, max_depth = hyper_expanded_grid1$max_depth,
                                      subsample = hyper_expanded_grid1$subsample, colsample_bytree = hyper_expanded_grid1$colsample_bytree,
                                      eta = hyper_expanded_grid1$eta, alpha = hyper_expanded_grid1$alpha, gamma = hyper_expanded_grid1$gamma,
                                      nrounds = hyper_expanded_grid1$nrounds)


  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]
  #Full data
  full_data_first_train <- cbind(targets_first_train$fwd_premium_3m, features_first_train[,-c(1:3)])
  colnames(full_data_first_train)[1] <- c("fwd_premium_3m")



  #Use foreach to simulate result of parallelized hyper tuning
  first_rebal <-
    foreach::foreach(s = 1:nrow(hyper_expanded_grid1), .options.future = list(seed = TRUE)) %dofuture% {

      #Create xgb.DMatrix object
      full_data_xgb_first_train <- xgboost::xgb.DMatrix(data = as.matrix(features_first_train[,-c(1:3)]),
                                                        label = targets_first_train$fwd_premium_3m)

      full_data_xgb_first_val <- xgboost::xgb.DMatrix(data = as.matrix(features_first_val[,-c(1:3)]),
                                                      label = targets_first_val$fwd_premium_3m)


      #Train Model
      xgb.mod1 <- xgboost::xgb.train(data = full_data_xgb_first_train,
                                     eta = hyper_expanded_grid1$eta[s],
                                     min_child_weight = hyper_expanded_grid1$min_child_weight[s],
                                     max_depth = hyper_expanded_grid1$max_depth[s],
                                     nrounds = hyper_expanded_grid1$nrounds[s],
                                     subsample = hyper_expanded_grid1$subsample[s],
                                     colsample_bytree = hyper_expanded_grid1$colsample_bytree[s],
                                     alpha = hyper_expanded_grid1$alpha[s],
                                     early_stopping_rounds = 25,
                                     print_every_n = 500,
                                     gamma = hyper_expanded_grid1$gamma[s],
                                     objective = "reg:squarederror",
                                     huber_slope = 1.3,
                                     eval_metric = "rmse",
                                     verbose = 0,
                                     watchlist = (list(train = full_data_xgb_first_train,
                                                       validation = full_data_xgb_first_val))



      )

      out <- data.frame(matrix(NA, nrow = length(targets_first_val$fwd_premium_3m),
                               ncol = nrow(hyper_expanded_grid1)))

      best_iteration <- vector(length = nrow(hyper_expanded_grid1))


      #Predict vlidation data
      out[,s] <-
        stats::predict(xgb.mod1, newdata = as.matrix(features_first_val[,-c(1:3)]))

      best_iteration[s] <- xgb.mod1$best_iteration

      #RMSE CHOSEN
      return(list(predictions = out[,s],
                  metric = sqrt(mean((targets_first_val$fwd_premium_3m - out[,s])^2)),
                  best_iteration = best_iteration
      ))

    }

  #Features val
  chosen_eval_metric_val[[1]] <- data.frame(min_child_weight = hyper_expanded_grid1$min_child_weight, max_depth = hyper_expanded_grid1$max_depth,
                                            subsample = hyper_expanded_grid1$subsample, colsample_bytree = hyper_expanded_grid1$colsample_bytree,
                                            eta = hyper_expanded_grid1$eta, alpha = hyper_expanded_grid1$alpha, gamma = hyper_expanded_grid1$gamma,
                                            nrounds = hyper_expanded_grid1$nrounds,
                                            best_iteration = sapply(first_rebal, function(x) x$best_iteration) %>% diag(),
                                            chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid1)))
  #Pass objects
  shrinkage.pred_df <- sapply(first_rebal, function(x) as.numeric(x$predictions)) #Transform to df
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]]) #chance colnames

  chosen_eval_metric_val[[1]]$chosen_eval_metric <- sapply(first_rebal, function(x) x$metric)


  #RMSE IS MIN: PAY ATTENTION
  hyper_choice1 <- which.min(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(targets_first_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(targets_first_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1.3)^2*(sqrt(1+((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                  (1.3))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                     0.5*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                     (1-0.5)*(-1)*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/targets_first_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(targets_first_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice1])))/
    length(targets_first_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[1] <- mean(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])


  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]

  #Full data
  full_data_first_training_and_validation <- cbind(target_first_training_and_validation$fwd_premium_3m, features_first_training_and_validation[,-c(1:3)])
  colnames(full_data_first_training_and_validation)[1] <- c("fwd_premium_3m")

  #Refitted model
  xgb.mod.refit <- xgboost::xgb.train(data = xgboost::xgb.DMatrix(data = as.matrix(features_first_training_and_validation[,-c(1:3)]),
                                                                  label = target_first_training_and_validation$fwd_premium_3m),
                                      objective = "reg:squarederror",
                                      huber_slope = 1.3,
                                      min_child_weight = hyper_expanded_grid1$min_child_weight[hyper_choice1],
                                      max_depth = hyper_expanded_grid1$max_depth[hyper_choice1],
                                      subsample = hyper_expanded_grid1$subsample[hyper_choice1],
                                      colsample_bytree = hyper_expanded_grid1$colsample_bytree[hyper_choice1],
                                      eta = hyper_expanded_grid1$eta[hyper_choice1],
                                      alpha = hyper_expanded_grid1$alpha[hyper_choice1],
                                      gamma = hyper_expanded_grid1$gamma[hyper_choice1],
                                      nrounds = chosen_eval_metric_val[[1]]$best_iteration[hyper_choice1],
                                      eval_metric = "rmse"
  )




  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(predict(xgb.mod.refit, newdata = as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)])))
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  prediction_list[[2]] <- as.numeric(predict(xgb.mod.refit, newdata = as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)])))
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  hyper_expanded_grid2 <-  list(min_child_weight = c(3),
                                max_depth = runif(2, 1, 2),
                                subsample = runif(2, 0.25, 0.50),
                                colsample_bytree = runif(2, 0.25, 0.50),
                                eta = runif(2, 0.1, 0.2),
                                alpha = runif(2, 2, 5),
                                gamma = 0,
                                nrounds = runif(2, 200, 500)
  )

  hyper_expanded_grid2$nrounds <- round(hyper_expanded_grid2$nrounds, 0)
  hyper_expanded_grid2$max_depth <- round(hyper_expanded_grid2$max_depth, 0)

  hyper_expanded_grid2 <- lapply(hyper_expanded_grid2, function(x) unique(x))


  hyper_expanded_grid2 <- expand.grid(min_child_weight = hyper_expanded_grid2$min_child_weight, max_depth = hyper_expanded_grid2$max_depth,
                                      subsample = hyper_expanded_grid2$subsample, colsample_bytree = hyper_expanded_grid2$colsample_bytree,
                                      eta = hyper_expanded_grid2$eta, alpha = hyper_expanded_grid2$alpha, gamma = hyper_expanded_grid2$gamma,
                                      nrounds = hyper_expanded_grid2$nrounds)
  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]

  #Full data
  full_data_second_train <- cbind(targets_second_train$fwd_premium_3m, features_second_train[,-c(1:3)])
  colnames(full_data_second_train)[1] <- c("fwd_premium_3m")


  #Second val

  #Use foreach to simulate result of parallelized hyper tuning
  second_rebal <-
    foreach::foreach(s = 1:nrow(hyper_expanded_grid2), .options.future = list(seed = TRUE)) %dofuture% {

      #Create xgb.DMatrix object
      full_data_xgb_second_train <- xgboost::xgb.DMatrix(data = as.matrix(features_second_train[,-c(1:3)]),
                                                         label = targets_second_train$fwd_premium_3m)

      full_data_xgb_second_val <- xgboost::xgb.DMatrix(data = as.matrix(features_second_val[,-c(1:3)]),
                                                       label = targets_second_val$fwd_premium_3m)

      #Train Model
      xgb.mod2 <- xgboost::xgb.train(data = full_data_xgb_second_train,
                                     eta = hyper_expanded_grid2$eta[s],
                                     min_child_weight = hyper_expanded_grid2$min_child_weight[s],
                                     max_depth = hyper_expanded_grid2$max_depth[s],
                                     nrounds = hyper_expanded_grid2$nrounds[s],
                                     subsample = hyper_expanded_grid2$subsample[s],
                                     colsample_bytree = hyper_expanded_grid2$colsample_bytree[s],
                                     alpha = hyper_expanded_grid2$alpha[s],
                                     gamma = hyper_expanded_grid2$gamma[s],
                                     objective = "reg:squarederror",
                                     eval_metric = "rmse",
                                     huber_slope = 1.3,
                                     early_stopping_rounds = 25,
                                     verbose = 0,
                                     watchlist = (list(train = full_data_xgb_second_train,
                                                       validation = full_data_xgb_second_val))



      )

      out <- data.frame(matrix(NA, nrow = length(targets_second_val$fwd_premium_3m),
                               ncol = nrow(hyper_expanded_grid2)))

      best_iteration <- vector(length = nrow(hyper_expanded_grid2))


      #Predict vlidation data
      out[,s] <-
        stats::predict(xgb.mod2, newdata = as.matrix(features_second_val[,-c(1:3)]))

      best_iteration[s] <- xgb.mod2$best_iteration


      #RMSE CHOSEN
      return(list(predictions = out[,s],
                  metric = sqrt(mean((targets_second_val$fwd_premium_3m - out[,s])^2)),
                  best_iteration = best_iteration
      ))

    }


  #Pass objects
  chosen_eval_metric_val[[2]] <- data.frame(min_child_weight = hyper_expanded_grid2$min_child_weight, max_depth = hyper_expanded_grid2$max_depth,
                                            subsample = hyper_expanded_grid2$subsample, colsample_bytree = hyper_expanded_grid2$colsample_bytree,
                                            eta = hyper_expanded_grid2$eta, alpha = hyper_expanded_grid2$alpha, gamma = hyper_expanded_grid2$gamma,
                                            nrounds = hyper_expanded_grid2$nrounds,
                                            best_iteration = sapply(second_rebal, function(x) x$best_iteration) %>% diag(),
                                            chosen_eval_metric = rep(NA, nrow(hyper_expanded_grid2)))


  shrinkage.pred_df <- sapply(second_rebal, function(x) as.numeric(x$predictions)) #Transform to df
  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]]) #chance colnames

  chosen_eval_metric_val[[2]]$chosen_eval_metric <- sapply(second_rebal, function(x) x$metric)


  #PSEUDO HUBER IS MIN: PAY ATTENTION
  hyper_choice2 <- which.min(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(targets_second_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(targets_second_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))

  validation_eval_hyper_choice$mphe[2] <- mean((1.3)^2*(sqrt(1+((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                  (1.3))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                     0.5*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                     (1-0.5)*(-1)*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs(
    (targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/targets_second_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- length(which(sign(targets_second_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice2])))/
    length(targets_second_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[2] <- mean(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])


  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]

  #Full data
  full_data_second_training_and_validation <- cbind(target_second_training_and_validation$fwd_premium_3m, features_second_training_and_validation[,-c(1:3)])
  colnames(full_data_second_training_and_validation)[1] <- c("fwd_premium_3m")

  #Refitted model
  xgb.mod.refit <- xgboost::xgb.train(data = xgboost::xgb.DMatrix(data = as.matrix(features_second_training_and_validation[,-c(1:3)]),
                                                                  label = target_second_training_and_validation$fwd_premium_3m),
                                      objective = "reg:squarederror",
                                      huber_slope = 1.3,
                                      min_child_weight = hyper_expanded_grid2$min_child_weight[hyper_choice2],
                                      max_depth = hyper_expanded_grid2$max_depth[hyper_choice2],
                                      subsample = hyper_expanded_grid2$subsample[hyper_choice2],
                                      colsample_bytree = hyper_expanded_grid2$colsample_bytree[hyper_choice2],
                                      eta = hyper_expanded_grid2$eta[hyper_choice2],
                                      alpha = hyper_expanded_grid2$alpha[hyper_choice2],
                                      gamma = hyper_expanded_grid2$gamma[hyper_choice2],
                                      nrounds = chosen_eval_metric_val[[2]]$best_iteration[hyper_choice2])




  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]





  #Predict!
  prediction_list[[3]] <- as.numeric(predict(xgb.mod.refit, newdata = as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)])))
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  prediction_list[[4]] <- as.numeric(predict(xgb.mod.refit, newdata = as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)])))
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss =c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA), row.names =   c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(1.3^2*(sqrt(1+((y_list[[l]] - prediction_list[[l]])/(1.3))^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0,
                                                   0.5*(y_list[[l]] - prediction_list[[l]]),
                                                   (1-0.5)*(-1)*(y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))
    oos_testing_eval_metrics$hr[l] <- mean((y_list[[l]] * prediction_list[[l]])>0)
    oos_testing_eval_metrics$mb[l] <- mean(y_list[[l]] - prediction_list[[l]])



  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(xgb.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.0001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }

  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     min_child_weight = c(hyper_expanded_grid1$min_child_weight[hyper_choice1], hyper_expanded_grid2$min_child_weight[hyper_choice2]),
                                     max_depth = c(hyper_expanded_grid1$max_depth[hyper_choice1], hyper_expanded_grid2$max_depth[hyper_choice2]),
                                     subsample = c(hyper_expanded_grid1$subsample[hyper_choice1], hyper_expanded_grid2$subsample[hyper_choice2]),
                                     colsample_bytree = c(hyper_expanded_grid1$colsample_bytree[hyper_choice1], hyper_expanded_grid2$colsample_bytree[hyper_choice2]),
                                     eta = c(hyper_expanded_grid1$eta[hyper_choice1], hyper_expanded_grid2$eta[hyper_choice2]),
                                     alpha = c(hyper_expanded_grid1$alpha[hyper_choice1], hyper_expanded_grid2$alpha[hyper_choice2]),
                                     gamma = c(hyper_expanded_grid1$gamma[hyper_choice1], hyper_expanded_grid2$gamma[hyper_choice2]),
                                     nrounds = c(hyper_expanded_grid1$nrounds[hyper_choice1], hyper_expanded_grid2$nrounds[hyper_choice2]),
                                     best_iteration = c(chosen_eval_metric_val[[1]]$best_iteration[hyper_choice1],
                                                        chosen_eval_metric_val[[2]]$best_iteration[hyper_choice2])

  )



  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL



  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-5
  )

  suppressWarnings({
    future::plan("sequential")
  })

})



###Bayesian Opt
#Define your test
test_that("GLMNET - run_ml_backtest works with rebalancing, 3m target, bayesian_opt as tuning method and rmse as chosen eval metric -toy_preprocessed_features_and_targets",{

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))
  #For second rebalancing, bayesian_opt could not converge because FUN was evaluating same results. So a hypothetical cov is added just to test bayes opt dynamic
  toy_preprocessed_features$artificial_var <- toy_preprocessed_targets$fwd_return_1m

  set.seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "rmse",
      hyper_grid_domain = list(alpha = c(0,1),
                                    lambda.min.ratio = c(0, 0.9)), #Bayesian Opt
      tuning_method = c("bayesian_opt"),
      n_iter = 10,
      ml_algorithm = "glmnet",
      init_points = 5,
      k_iter = 1,
      acq = "ucb",
      huber_delta = 0.5,
      verbose = FALSE,
      show_plots = FALSE,
      parallel = FALSE
    )}))

  chosen_loss <- "rmse"
  #Define initial objects
  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             mape = c(NA,NA),
                                             hr = c(NA,NA),
                                             mb = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]

  #Bayesian Opt
  evaluate_hyper_objective_function <-
    #GLMNET (Elastic Net)
    function(alpha,lambda.min.ratio){
      #Fit GLM model

      glmnet_fit <- glmnet::glmnet(as.matrix(features_first_train[,-c(1:3)]), #train matrix
                                   targets_first_train$fwd_premium_3m, #target vector
                                   alpha = alpha, #alpha hyperparameter
                                   lambda.min.ratio = lambda.min.ratio) #lambda hyperparameter

      #Identify best lambda
      best_lam <- glmnet_fit$lambda[
        which.min(
        sqrt(colMeans((targets_first_val$fwd_premium_3m - predict(glmnet_fit, newx = as.matrix(features_first_val[,-c(1:3)])))^2))
      )
      ]

      #Predict
      pred <- predict(glmnet_fit,#GLM model
                      newx = as.matrix(features_first_val[,-c(1:3)]),
                      s = best_lam) #Features test

      #Error
      error <- targets_first_val$fwd_premium_3m - pred

      #Calculate loss metrics
      validation_sample_rsquared <- 1 - sum(error^2)/sum(targets_first_val$fwd_premium_3m^2) #R2
      validation_sample_crossproduct <- mean(pred*targets_first_val$fwd_premium_3m) #Cross-Product
      validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
      validation_sample_mae <- mean(abs(error)) #mae
      validation_sample_pseudo_huber <- mean(0.5^2*(sqrt(1+(error/0.5)^2)-1)) #Pseudo Huber
      validation_sample_pinball <- mean(ifelse(error >= 0, 0.5*error, (1-0.5)*-error)) #Pinball
      validation_sample_mape <- mean(abs(error/targets_first_val$fwd_premium_3m)) #mae
      validation_sample_hr <- mean(targets_first_val$fwd_premium_3m*pred>=0)
      validation_sample_mb <- mean(error)





      return(list(Score = -validation_sample_rmse, #RMSE
                  rss =validation_sample_rsquared,
                  cp = validation_sample_crossproduct,
                  rmse = validation_sample_rmse,
                  mae = validation_sample_mae,
                  mphe = validation_sample_pseudo_huber,
                  mpe = validation_sample_pinball,
                  mape = validation_sample_mape,
                  hr = validation_sample_hr,
                  mb = validation_sample_mb,
                  best_lam = best_lam

      ))

    }


  #cl <- parallel::makeCluster(2)
  #doParallel::registerDoParallel(cl)
  #parallel::clusterExport(cl, c("features_first_train",
  #                    "features_first_val",
  #                    "targets_first_train",
  #                    "targets_first_val"))
  #parallel::clusterEvalQ(cl, expr={
  #  library("glmnet")
  #})

  #doFuture::registerDoFuture()
  #plan(multisession)
  #parallel::clusterExport(c("features_first_train",
  #                              "features_first_val",
  #                              "targets_first_train",
  #                              "targets_first_val"))
  #parallel::clusterEvalQ(cl, expr={
  #  library("glmnet")
  #})


  #tictoc::tic()

  set.seed(123)
  bayes_opt1 <- #doFuture::withDoRNG(
    ParBayesianOptimization::bayesOpt(
      FUN = evaluate_hyper_objective_function, #FUN
      bounds = list(alpha = c(0,1),
                    lambda.min.ratio = c(0, 0.9)), #Boundaries
      initPoints = 5, #Number of randomly chosen points to sample the target function before B.O.
      acq = "ucb", #Acquisition function to be used
      iters.n = 10, #Number of times BO is to be repeated
      verbose = FALSE,
      parallel = FALSE
    )
  #)
  #tictoc::toc()
  #parallel::stopCluster(cl)

  #Features val
  chosen_eval_metric_val[[1]] <- data.frame(alpha = bayes_opt1$scoreSummary$alpha,
                                              lambda.min.ratio = bayes_opt1$scoreSummary$lambda.min.ratio,
                                              best_lam = rep(NA, length(bayes_opt1$scoreSummary$alpha)),
                                              chosen_eval_metric = rep(NA, length(bayes_opt1$scoreSummary$alpha)))

  best_lam1 <- bayes_opt1$scoreSummary$best_lam[which.max(bayes_opt1$scoreSummary$Score)]

  chosen_eval_metric_val[[1]]$best_lam <- bayes_opt1$scoreSummary$best_lam


  chosen_eval_metric_val[[1]]$chosen_eval_metric = as.numeric(bayes_opt1$scoreSummary$rmse)




  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rmse[1] <- bayes_opt1$scoreSummary$rmse[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$rss[1] <- bayes_opt1$scoreSummary$rss[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$cp[1] <- bayes_opt1$scoreSummary$cp[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mae[1] <- bayes_opt1$scoreSummary$mae[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mphe[1] <- bayes_opt1$scoreSummary$mphe[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mpe[1] <- bayes_opt1$scoreSummary$mpe[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mape[1] <- bayes_opt1$scoreSummary$mape[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$hr[1] <- bayes_opt1$scoreSummary$hr[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mb[1] <- bayes_opt1$scoreSummary$mb[which.max(bayes_opt1$scoreSummary$Score)]




  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_first_training_and_validation[,-c(1:3)],
                                  y = target_first_training_and_validation$fwd_premium_3m,
                                  alpha = unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['alpha'],
                                  lambda.min.ratio = unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['lambda.min.ratio']
                                  )
  coef(glm.mod.refit)


  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(
    predict(glm.mod.refit,
            newx = as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)]),
            s = best_lam1))
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  prediction_list[[2]] <- as.numeric(
    predict(glm.mod.refit,
            newx = as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)]),
            s = best_lam1))
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]
  #Features val

  #Bayesian Opt
  #Bayesian Opt
  evaluate_hyper_objective_function <-
    #GLMNET (Elastic Net)
    function(alpha,lambda.min.ratio){
      #Fit GLM model

      glmnet_fit <- glmnet::glmnet(as.matrix(features_second_train[,-c(1:3)]), #train matrix
                                   targets_second_train$fwd_premium_3m, #target vector
                                   alpha = alpha, #alpha hyperparameter
                                   lambda.min.ratio = lambda.min.ratio) #lambda hyperparameter

      #Identify best lambda
      best_lam <- glmnet_fit$lambda[
        which.min(
          sqrt(colMeans((targets_second_val$fwd_premium_3m - predict(glmnet_fit, newx = as.matrix(features_second_val[,-c(1:3)])))^2))
        )
      ]

      #Predict
      pred <- predict(glmnet_fit,#GLM model
                      newx = as.matrix(features_second_val[,-c(1:3)]),
                      s = best_lam) #Features test

      #Error
      error <- targets_second_val$fwd_premium_3m - pred

      #Calculate loss metrics
      validation_sample_rsquared <- 1 - sum(error^2)/sum(targets_second_val$fwd_premium_3m^2) #R2
      validation_sample_crossproduct <- mean(pred*targets_second_val$fwd_premium_3m) #Cross-Product
      validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
      validation_sample_mae <- mean(abs(error)) #mae
      validation_sample_pseudo_huber <- mean(0.5^2*(sqrt(1+(error/0.5)^2)-1)) #Pseudo Huber
      validation_sample_pinball <- mean(ifelse(error >= 0, 0.5*error, (1-0.5)*-error)) #Pinball
      validation_sample_mape <- mean(abs(error/targets_second_val$fwd_premium_3m)) #mae
      validation_sample_hr <- mean(targets_second_val$fwd_premium_3m*pred>=0)
      validation_sample_mb <- mean(error)


      return(list(Score = -validation_sample_rmse, #RMSE
                  rss =validation_sample_rsquared,
                  cp = validation_sample_crossproduct,
                  rmse = validation_sample_rmse,
                  mae = validation_sample_mae,
                  mphe = validation_sample_pseudo_huber,
                  mpe = validation_sample_pinball,
                  mape = validation_sample_mape,
                  hr = validation_sample_hr,
                  mb = validation_sample_mb,
                  best_lam = best_lam

      ))

    }

  bayes_opt2 <- ParBayesianOptimization::bayesOpt(
    FUN = evaluate_hyper_objective_function, #FUN
    bounds = list(alpha = c(0,1),
                  lambda.min.ratio = c(0, 0.9)), #Boundaries
    initPoints = 5, #Number of randomly chosen points to sample the target function before B.O.
    acq = "ucb", #Acquisition function to be used
    iters.n = 10, #Number of times BO is to be repeated
    verbose = FALSE,
    parallel = FALSE
  )

  #Features val
  chosen_eval_metric_val[[2]] <- data.frame(alpha = bayes_opt2$scoreSummary$alpha,
                                            lambda.min.ratio = bayes_opt2$scoreSummary$lambda.min.ratio,
                                            best_lam = rep(NA, length(bayes_opt2$scoreSummary$alpha)),
                                            chosen_eval_metric = rep(NA, length(bayes_opt2$scoreSummary$alpha)))

  best_lam2 <- bayes_opt2$scoreSummary$best_lam[which.max(bayes_opt2$scoreSummary$Score)]

  chosen_eval_metric_val[[2]]$best_lam <- bayes_opt2$scoreSummary$best_lam

  chosen_eval_metric_val[[2]]$chosen_eval_metric = as.numeric(bayes_opt2$scoreSummary$rmse)


  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rmse[2] <- bayes_opt2$scoreSummary$rmse[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$rss[2] <- bayes_opt2$scoreSummary$rss[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$cp[2] <- bayes_opt2$scoreSummary$cp[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mae[2] <- bayes_opt2$scoreSummary$mae[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mphe[2] <- bayes_opt2$scoreSummary$mphe[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mpe[2] <- bayes_opt2$scoreSummary$mpe[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mape[2] <- bayes_opt2$scoreSummary$mape[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$hr[2] <- bayes_opt2$scoreSummary$hr[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mb[2] <- bayes_opt2$scoreSummary$mb[which.max(bayes_opt2$scoreSummary$Score)]



  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]


  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_second_training_and_validation[,-c(1:3)],
                                  y = target_second_training_and_validation$fwd_premium_3m,
                                  alpha = unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['alpha'],
                                  lambda.min.ratio = unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['lambda.min.ratio'])
  coef(glm.mod.refit)


  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]



  #Predict!
  prediction_list[[3]] <- as.numeric(predict(glm.mod.refit, newx = as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)]),
                                             s = best_lam2))
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  prediction_list[[4]] <- as.numeric(predict(glm.mod.refit, newx = as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)]),
                                             s = best_lam2))
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss =c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA), row.names =   c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(0.5^2*(sqrt(1+((y_list[[l]] - prediction_list[[l]])/0.5)^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0, 0.5*(y_list[[l]] - prediction_list[[l]]),
                                                       (1-0.5)*(-1)*(y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))
    oos_testing_eval_metrics$hr[l] <- mean(((y_list[[l]] * prediction_list[[l]])>=0))
    oos_testing_eval_metrics$mb[l] <- mean(((y_list[[l]] - prediction_list[[l]])))


  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(glm.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.0001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }

  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     alpha = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['alpha'],
                                               unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['alpha']),

                                     lambda.min.ratio = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['lambda.min.ratio'],
                                                          unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['lambda.min.ratio']),

                                     best_lam = c(best_lam1, best_lam2)
  )


  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL



  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-2
  )

})

#Define your test
test_that("RF (Parallel) - run_ml_backtest works with rebalancing, 3m target, bayesian_opt as tuning method and mphe as chosen eval metric -toy_preprocessed_features_and_targets",{

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  doFuture::registerDoFuture()
  future::plan("multisession")

  set.seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "mphe",
      hyper_grid_domain = list(mtry = c(0,1), #Bayesian Opt
                                    num.trees = c(100L, 1000L), #Num trees
                                    max.depth = c(2L, 8L), # Max depth
                                    min.bucket = c(1, 5) #min bucket
      ),
      tuning_method = c("bayesian_opt"),
      n_iter = 16, #multiple of cores
      k_iter = 8, #multiple of cores
      ml_algorithm = "rf",
      init_points = 5,
      acq = "ucb",
      verbose = FALSE,
      quantile_tau = 0.25,
      huber_delta = 1.3,
      show_plots = FALSE,
      parallel = TRUE
    )}))

  target_fwd_name <- "fwd_premium_3m"
  chosen_loss <- "rmse"
  #Define initial objects
  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]

  #Full data traing
  full_data_first_train <- cbind(targets_first_train$fwd_premium_3m, features_first_train[,-c(1:3)])
  colnames(full_data_first_train)[1] <- target_fwd_name

  eval_function <- function(mtry, num.trees, max.depth, min.bucket){

    #Fit RF model
    rf_fit <- ranger::ranger(paste(target_fwd_name,'~.'), data = janitor::clean_names(full_data_first_train), #Names need to be clean
                             mtry = mtry * (ncol(full_data_first_train) - 1), #Proportion of variables used to forecast
                             num.trees = num.trees, #Number of trees
                             max.depth = max.depth, #Max Depth of tree
                             min.bucket = min.bucket) #Min Size of Terminal Node

    #Predict
    pred <- stats::predict(rf_fit,#RF model
                           data = janitor::clean_names(features_first_val[,-c(1:3)]) #Features val
    )$predictions

    #Error
    error <- targets_first_val$fwd_premium_3m - pred

    #Calculate loss metrics
    validation_sample_rsquared <- 1 - sum(error^2)/sum(targets_first_val$fwd_premium_3m^2) #R2
    validation_sample_crossproduct <- mean(pred*targets_first_val$fwd_premium_3m) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_pseudo_huber <- mean(1.3^2*(sqrt(1+(error/1.3)^2)-1)) #Pseudo Huber
    validation_sample_pinball <- mean(ifelse(error >= 0, 0.25*error, (1-0.25)*-error)) #Pinball
    validation_sample_mape <- mean(abs(error/targets_first_val$fwd_premium_3m)) #mape
    validation_sample_hr <- mean(targets_first_val$fwd_premium_3m*pred >= 0) #hr
    validation_sample_mb <- mean(error) #Pinball



    return(list(Score = -validation_sample_pseudo_huber, #RMSE
                rss =validation_sample_rsquared,
                cp = validation_sample_crossproduct,
                rmse = validation_sample_rmse,
                mae = validation_sample_mae,
                mphe = validation_sample_pseudo_huber,
                mpe = validation_sample_pinball,
                mape = validation_sample_mape,
                hr = validation_sample_hr,
                mb = validation_sample_mb
    ))


  }

  set.seed(123)
  #cl <- parallel::makeCluster(2)
  #doParallel::registerDoParallel(cl)
  #parallel::clusterExport(cl, c("features_first_train",
  #                    "features_first_val",
  #                    "targets_first_train",
  #                    "targets_first_val"))
  #parallel::clusterEvalQ(cl, expr={
  #  library("glmnet")
  #})

  #doFuture::registerDoFuture()
  #plan(multisession)
  #parallel::clusterExport(c("features_first_train",
  #                              "features_first_val",
  #                              "targets_first_train",
  #                              "targets_first_val"))
  #parallel::clusterEvalQ(cl, expr={
  #  library("glmnet")
  #})


  #tictoc::tic()
  #Bayes opt

  bayes_opt1 <- doFuture::withDoRNG(
    ParBayesianOptimization::bayesOpt(
      FUN = eval_function, #FUN
      bounds = list(mtry = c(0,1),
                    num.trees = c(100L, 1000L),
                    max.depth = c(2L,8L),
                    min.bucket = c(1,5)
      ),
      initPoints = 5, #Number of randomly chosen points to sample the target function before B.O.
      iters.k = 8,
      acq = "ucb", #Acquisition function to be used
      iters.n = 16, #Number of times BO is to be repeated
      verbose = FALSE,
      parallel = TRUE
    )
  )
  #tictoc::toc()
  #parallel::stopCluster(cl)

  #Features val
  chosen_eval_metric_val[[1]] <- data.frame(mtry = bayes_opt1$scoreSummary$mtry,
                                              num.trees = bayes_opt1$scoreSummary$num.trees,
                                              max.depth = bayes_opt1$scoreSummary$max.depth,
                                              min.bucket = bayes_opt1$scoreSummary$min.bucket,
                                              chosen_eval_metric = rep(NA, length(bayes_opt1$scoreSummary$mtry)))


  chosen_eval_metric_val[[1]]$chosen_eval_metric = as.numeric(bayes_opt1$scoreSummary$mphe)



  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rmse[1] <- bayes_opt1$scoreSummary$rmse[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$rss[1] <- bayes_opt1$scoreSummary$rss[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$cp[1] <- bayes_opt1$scoreSummary$cp[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mae[1] <- bayes_opt1$scoreSummary$mae[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mphe[1] <- bayes_opt1$scoreSummary$mphe[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mpe[1] <- bayes_opt1$scoreSummary$mpe[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mape[1] <- bayes_opt1$scoreSummary$mape[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$hr[1] <- bayes_opt1$scoreSummary$hr[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mb[1] <- bayes_opt1$scoreSummary$mb[which.max(bayes_opt1$scoreSummary$Score)]



  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]

  full_data_first_training_and_validation <- cbind(target_first_training_and_validation$fwd_premium_3m,
                                                   features_first_training_and_validation[,-c(1:3)])

  colnames(full_data_first_training_and_validation)[1] <- target_fwd_name

  #Refitted model
  rf.mod.refit <- ranger::ranger(paste(target_fwd_name, '~.'),
                                 data = janitor::clean_names(full_data_first_training_and_validation),
                                 mtry = unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['mtry']*
                                   (ncol(full_data_first_training_and_validation)-1),
                                 num.trees = unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['num.trees'],
                                 max.depth = unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['max.depth'],
                                 min.bucket = unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['min.bucket']
  )

  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(
    predict(rf.mod.refit,
            data = janitor::clean_names(features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)]))$predictions)
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  prediction_list[[2]] <-
    as.numeric(
      predict(rf.mod.refit,
              data = janitor::clean_names(features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)]))$predictions)
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]
  #Features val

  #Full data training
  full_data_second_train <- cbind(targets_second_train$fwd_premium_3m, features_second_train[,-c(1:3)])
  colnames(full_data_second_train)[1] <- target_fwd_name

  eval_function <- function(mtry, num.trees, max.depth, min.bucket){

    #Fit RF model
    rf_fit <- ranger::ranger(paste(target_fwd_name,'~.'), data = janitor::clean_names(full_data_second_train), #Names need to be clean
                             mtry = mtry * (ncol(full_data_second_train) - 1), #Proportion of variables used to forecast
                             num.trees = num.trees, #Number of trees
                             max.depth = max.depth, #Max Depth of tree
                             min.bucket = min.bucket) #Min Size of Terminal Node

    #Predict
    pred <- stats::predict(rf_fit,#RF model
                           data = janitor::clean_names(features_second_val[,-c(1:3)]) #Features val
    )$predictions

    #Error
    error <- targets_second_val$fwd_premium_3m - pred

    #Calculate loss metrics
    validation_sample_rsquared <- 1 - sum(error^2)/sum(targets_second_val$fwd_premium_3m^2) #R2
    validation_sample_crossproduct <- mean(pred*targets_second_val$fwd_premium_3m) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_pseudo_huber <- mean(1.3^2*(sqrt(1+(error/1.3)^2)-1)) #Pseudo Huber
    validation_sample_pinball <- mean(ifelse(error >= 0, 0.25*error, (1-0.25)*-error)) #Pinball
    validation_sample_mape <- mean(abs(error/targets_second_val$fwd_premium_3m)) #mape
    validation_sample_hr <- mean(targets_second_val$fwd_premium_3m*pred >= 0) #hr
    validation_sample_mb <- mean(error) #Pinball

    return(list(Score = -validation_sample_pseudo_huber, #MPHE
                rss =validation_sample_rsquared,
                cp = validation_sample_crossproduct,
                rmse = validation_sample_rmse,
                mae = validation_sample_mae,
                mphe = validation_sample_pseudo_huber,
                mpe = validation_sample_pinball,
                mape = validation_sample_mape,
                hr = validation_sample_hr,
                mb = validation_sample_mb
    ))


  }


  #cl <- parallel::makeCluster(2)
  #doParallel::registerDoParallel(cl)
  #parallel::clusterExport(cl, c("features_first_train",
  #                    "features_first_val",
  #                    "targets_first_train",
  #                    "targets_first_val"))
  #parallel::clusterEvalQ(cl, expr={
  #  library("glmnet")
  #})

  #doFuture::registerDoFuture()
  #plan(multisession)
  #parallel::clusterExport(c("features_first_train",
  #                              "features_first_val",
  #                              "targets_first_train",
  #                              "targets_first_val"))
  #parallel::clusterEvalQ(cl, expr={
  #  library("glmnet")
  #})


  #tictoc::tic()
  #Bayes opt
  bayes_opt2 <- doFuture::withDoRNG(
    ParBayesianOptimization::bayesOpt(
      FUN = eval_function, #FUN
      bounds = list(mtry = c(0,1),
                    num.trees = c(100L, 1000L),
                    max.depth = c(2L,8L),
                    min.bucket = c(1,5)
      ),
      initPoints = 5, #Number of randomly chosen points to sample the target function before B.O.
      iters.k = 8,
      acq = "ucb", #Acquisition function to be used
      iters.n = 16, #Number of times BO is to be repeated
      verbose = FALSE,
      parallel = TRUE
    )
  )

  #Features val
  chosen_eval_metric_val[[2]] <- data.frame(mtry = bayes_opt2$scoreSummary$mtry,
                                              num.trees = bayes_opt2$scoreSummary$num.trees,
                                              max.depth = bayes_opt2$scoreSummary$max.depth,
                                              min.bucket = bayes_opt2$scoreSummary$min.bucket,
                                              chosen_eval_metric = rep(NA, length(bayes_opt2$scoreSummary$mtry)))


  chosen_eval_metric_val[[2]]$chosen_eval_metric = as.numeric(bayes_opt2$scoreSummary$mphe)


  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rmse[2] <- bayes_opt2$scoreSummary$rmse[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$rss[2] <- bayes_opt2$scoreSummary$rss[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$cp[2] <- bayes_opt2$scoreSummary$cp[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mae[2] <- bayes_opt2$scoreSummary$mae[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mphe[2] <- bayes_opt2$scoreSummary$mphe[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mpe[2] <- bayes_opt2$scoreSummary$mpe[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mape[2] <- bayes_opt2$scoreSummary$mape[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$hr[2] <- bayes_opt2$scoreSummary$hr[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mb[2] <- bayes_opt2$scoreSummary$mb[which.max(bayes_opt2$scoreSummary$Score)]

  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]


  full_data_second_training_and_validation <- cbind(target_second_training_and_validation$fwd_premium_3m,
                                                    features_second_training_and_validation[,-c(1:3)])

  colnames(full_data_second_training_and_validation)[1] <- target_fwd_name


  #Refitted model
  rf.mod.refit <- ranger::ranger(paste(target_fwd_name, '~.'),
                                 data = janitor::clean_names(full_data_second_training_and_validation),
                                 mtry = unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['mtry']*
                                   (ncol(full_data_second_training_and_validation)-1),
                                 num.trees = unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['num.trees'],
                                 max.depth = unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['max.depth'],
                                 min.bucket = unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['min.bucket']
  )

  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]



  #Predict!
  prediction_list[[3]] <- as.numeric(
    predict(rf.mod.refit,
            data = janitor::clean_names(features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)]))$predictions)
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  prediction_list[[4]] <-
    as.numeric(
      predict(rf.mod.refit,
              data = janitor::clean_names(features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)]))$predictions)
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss =c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA),
                                         mphe = c(NA,NA,NA,NA),
                                         mpe = c(NA,NA,NA,NA),
                                         row.names =   c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(1.3^2*(sqrt(1+((y_list[[l]] - prediction_list[[l]])/1.3)^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0,
                                                       0.25*(y_list[[l]] - prediction_list[[l]]),
                                                       (1-0.25)*(-1)*(y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))
    oos_testing_eval_metrics$hr[l] <- mean(((y_list[[l]] * prediction_list[[l]]) >= 0))
    oos_testing_eval_metrics$mb[l] <- mean(y_list[[l]] - prediction_list[[l]])



  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(rf.mod.refit) - coef(ml_backtest_results@final_model@model)) < 0.0001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }

  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     mtry = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['mtry'],
                                              unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['mtry']),

                                     num.trees = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['num.trees'],
                                                   unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['num.trees']),

                                     max.depth = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['max.depth'],
                                                   unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['max.depth']),

                                     min.bucket = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['min.bucket'],
                                                    unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['min.bucket'])


  )

  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-5
  )

  future::plan("sequential")
  foreach::registerDoSEQ()

})

#Define your test
test_that("XGB (Parallel) - run_ml_backtest works with rebalancing, 3m target, bayesian_opt as tuning method, mphe as chosen eval metric and custom_objective pseudo_huber  -toy_preprocessed_features_and_targets",{

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  doFuture::registerDoFuture()
  future::plan("multisession")

  set.seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      ml_algorithm = "xgb",
      custom_objective = "pseudo_huber_error",
      target_fwd_name = c("fwd_premium_3m"),
      hyper_grid_domain = list(min_child_weight = c(1, 6), max_depth = c(2L, 8L),
                                    subsample = c(0.25, 1), colsample_bytree = c(0.25, 1),
                                    eta = c(0.02, 0.2), alpha = c(1, 5), gamma = c(0, 5), nrounds = c(200, 1000)),
      tuning_method = c("bayesian_opt"),
      n_iter = 16,
      k_iter = 8,
      init_points = 10,
      acq = "ucb",
      quantile_tau = 0.25,
      huber_delta = 1.25,
      verbose = TRUE,
      show_plots = FALSE,
      early_stop = 10
    )}))



  #Define initial objects
  target_fwd_name <- "fwd_premium_3m"
  chosen_loss <- "mphe"
  early_stop <- 10
  custom_obj <- "reg:pseudohubererror"

  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]
  #Full data
  full_data_first_train <- cbind(targets_first_train$fwd_premium_3m, features_first_train[,-c(1:3)])
  colnames(full_data_first_train)[1] <- c("fwd_premium_3m")

  wrapper_eval_function <- function(...){
    args <- list(...)
    custom_obj_xgb <- args$custom_obj
    quantile_tau <- args$quantile_tau
    features_1st_train <- args$features_first_train
    targets_1st_train <- args$targets_first_train
    features_1st_val <- args$features_first_val
    targets_1st_val <- args$targets_first_val
    early_stop <- args$early_stop

    eval_function <- function(min_child_weight, max_depth, subsample, colsample_bytree, eta, alpha, gamma, nrounds){



      #Create xgb.DMatrix object
      full_data_xgb_first_train <- xgboost::xgb.DMatrix(data = as.matrix(features_1st_train[,-c(1:3)]),
                                                        label = targets_1st_train$fwd_premium_3m)

      full_data_xgb_first_val <- xgboost::xgb.DMatrix(data = as.matrix(features_1st_val[,-c(1:3)]),
                                                      label = targets_1st_val$fwd_premium_3m)


      #Train Model
      xgb.mod <- xgboost::xgb.train(data = full_data_xgb_first_train,
                                     eta = eta,
                                     min_child_weight = min_child_weight,
                                     max_depth = max_depth,
                                     nrounds = nrounds,
                                     subsample = subsample,
                                     colsample_bytree = colsample_bytree,
                                     alpha = alpha,
                                     gamma = gamma,
                                     eval_metric = "mphe",
                                     objective = custom_obj_xgb,
                                     huber_slope = 1.25,
                                     verbose = 1,
                                     early_stopping_rounds = early_stop,
                                     watchlist = (list(train = full_data_xgb_first_train,
                                                       validation = full_data_xgb_first_val))



      )


      #Predict validation data
      pred <- stats::predict(xgb.mod, newdata = as.matrix(features_1st_val[,-c(1:3)]))

      #Error
      error <- targets_1st_val$fwd_premium_3m - pred

      #Calculate loss metrics
      validation_sample_rsquared <- 1 - sum(error^2)/sum(targets_1st_val$fwd_premium_3m^2) #R2
      validation_sample_crossproduct <- mean(pred*targets_1st_val$fwd_premium_3m) #Cross-Product
      validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
      validation_sample_mae <- mean(abs(error)) #mae
      validation_sample_pseudo_huber <- mean(1.25^2*(sqrt(1+(error/1.25)^2)-1)) #Pseudo Huber
      validation_sample_pinball <- mean(ifelse(error >= 0, 0.25*error, (1-0.25)*-error)) #Pinball
      validation_sample_mape <- mean(abs(error/targets_1st_val$fwd_premium_3m)) #mape
      validation_sample_hr <- mean(pred*targets_1st_val$fwd_premium_3m >= 0) #mae
      validation_sample_mb <- mean(error) #mae

        return(list(Score = -validation_sample_pseudo_huber, #Pinball
                  rss =validation_sample_rsquared,
                  cp = validation_sample_crossproduct,
                  rmse = validation_sample_rmse,
                  mae = validation_sample_mae,
                  mphe = validation_sample_pseudo_huber,
                  mpe = validation_sample_pinball,
                  mape = validation_sample_mape,
                  hr = validation_sample_hr,
                  mb = validation_sample_mb,
                  best_iteration = xgb.mod$best_iteration
      ))

    }

  }




  #Bayes opt
  set.seed(123)
  bayes_opt1 <- doFuture::withDoRNG(
    ParBayesianOptimization::bayesOpt(
      FUN = wrapper_eval_function(custom_obj = custom_obj, quantile_tau = 0.25, features_first_train = features_first_train,
                          targets_first_train = targets_first_train, features_first_val = features_first_val,
                          targets_first_val = targets_first_val, early_stop = early_stop), #FUN

      bounds = list(min_child_weight = c(1, 6), max_depth = c(2L, 8L),
                    subsample = c(0.25, 1), colsample_bytree = c(0.25, 1),
                    eta = c(0.02, 0.2), alpha = c(1, 5), gamma = c(0, 5), nrounds = c(200, 1000))
      ,
      initPoints = 10, #Number of randomly chosen points to sample the target function before B.O.
      iters.k = 8,
      acq = "ucb", #Acquisition function to be used
      iters.n = 16, #Number of times BO is to be repeated
      verbose = TRUE,
      parallel = TRUE,
      plotProgress = TRUE
    )
  )



  #Features val
  chosen_eval_metric_val[[1]] <- data.frame(min_child_weight = bayes_opt1$scoreSummary$min_child_weight,
                                              max_depth = bayes_opt1$scoreSummary$max_depth,
                                              subsample = bayes_opt1$scoreSummary$subsample,
                                              colsample_bytree = bayes_opt1$scoreSummary$colsample_bytree,
                                              eta = bayes_opt1$scoreSummary$eta,
                                              alpha = bayes_opt1$scoreSummary$alpha,
                                              gamma = bayes_opt1$scoreSummary$gamma,
                                              nrounds = bayes_opt1$scoreSummary$nrounds,
                                              best_iteration = bayes_opt1$scoreSummary$best_iteration,
                                              chosen_eval_metric = rep(NA, length(bayes_opt1$scoreSummary$min_child_weight)))

  chosen_eval_metric_val[[1]]$chosen_eval_metric = as.numeric(bayes_opt1$scoreSummary$mphe)


  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rmse[1] <- bayes_opt1$scoreSummary$rmse[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$rss[1] <- bayes_opt1$scoreSummary$rss[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$cp[1] <- bayes_opt1$scoreSummary$cp[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mae[1] <- bayes_opt1$scoreSummary$mae[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mphe[1] <- bayes_opt1$scoreSummary$mphe[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mpe[1] <- bayes_opt1$scoreSummary$mpe[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mape[1] <- bayes_opt1$scoreSummary$mape[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$hr[1] <- bayes_opt1$scoreSummary$hr[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mb[1] <- bayes_opt1$scoreSummary$mb[which.max(bayes_opt1$scoreSummary$Score)]



  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]

  #Full data
  full_data_first_training_and_validation <- cbind(target_first_training_and_validation$fwd_premium_3m, features_first_training_and_validation[,-c(1:3)])
  colnames(full_data_first_training_and_validation)[1] <- c("fwd_premium_3m")


  #Refitted model
  xgb.mod.refit1 <- xgboost::xgb.train(data = xgboost::xgb.DMatrix(data = as.matrix(features_first_training_and_validation[,-c(1:3)]),
                                                                  label = target_first_training_and_validation$fwd_premium_3m),
                                      objective = "reg:pseudohubererror",
                                      huber_slope = 1.25,
                                      min_child_weight = unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['min_child_weight'],
                                      max_depth = unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['max_depth'],
                                      subsample = unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['subsample'],
                                      colsample_bytree = unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['colsample_bytree'],
                                      eta = unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['eta'],
                                      alpha = unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['alpha'],
                                      gamma = unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['gamma'],
                                      nrounds = bayes_opt1$scoreSummary$best_iteration[which.max(bayes_opt1$scoreSummary$Score)]
                                      )




  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(predict(xgb.mod.refit1, newdata = as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)])))
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  prediction_list[[2]] <- as.numeric(predict(xgb.mod.refit1, newdata = as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)])))
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]

  #Full data
  full_data_second_train <- cbind(targets_second_train$fwd_premium_3m, features_second_train[,-c(1:3)])
  colnames(full_data_second_train)[1] <- c("fwd_premium_3m")


  wrapper_eval_function <- function(...){
    args <- list(...)
    custom_obj_xgb <- args$custom_obj
    quantile_tau <- args$quantile_tau
    early_stop <- args$early_stop

    eval_function <- function(min_child_weight, max_depth, subsample, colsample_bytree, eta, alpha, gamma, nrounds){



      #Create xgb.DMatrix object
      full_data_xgb_second_train <- xgboost::xgb.DMatrix(data = as.matrix(features_second_train[,-c(1:3)]),
                                                        label = targets_second_train$fwd_premium_3m)

      full_data_xgb_second_val <- xgboost::xgb.DMatrix(data = as.matrix(features_second_val[,-c(1:3)]),
                                                      label = targets_second_val$fwd_premium_3m)


      #Train Model
      xgb.mod <- xgboost::xgb.train(data = full_data_xgb_second_train,
                                    eta = eta,
                                    min_child_weight = min_child_weight,
                                    max_depth = max_depth,
                                    nrounds = nrounds,
                                    subsample = subsample,
                                    colsample_bytree = colsample_bytree,
                                    alpha = alpha,
                                    gamma = gamma,
                                    eval_metric = "mphe",
                                    objective = custom_obj_xgb,
                                    huber_slope = 1.25,
                                    verbose = 1,
                                    early_stopping_rounds = early_stop,
                                    watchlist = (list(train = full_data_xgb_second_train,
                                                      validation = full_data_xgb_second_val))



      )


      #Predict vlidation data
      pred <-
        stats::predict(xgb.mod, newdata = as.matrix(features_second_val[,-c(1:3)]))

      #Error
      error <- targets_second_val$fwd_premium_3m - pred

      #Calculate loss metrics
      validation_sample_rsquared <- 1 - sum(error^2)/sum(targets_second_val$fwd_premium_3m^2) #R2
      validation_sample_crossproduct <- mean(pred*targets_second_val$fwd_premium_3m) #Cross-Product
      validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
      validation_sample_mae <- mean(abs(error)) #mae
      validation_sample_pseudo_huber <- mean(1.25^2*(sqrt(1+(error/1.25)^2)-1)) #Pseudo Huber
      validation_sample_pinball <- mean(ifelse(error >= 0, 0.25*error, (1-0.25)*-error)) #Pinball
      validation_sample_mape <- mean(abs(error/targets_second_val$fwd_premium_3m)) #mape
      validation_sample_hr <- mean(pred*targets_second_val$fwd_premium_3m >= 0) #mae
      validation_sample_mb <- mean(error) #mae

      return(list(Score = -validation_sample_pseudo_huber, #MPHE
                  rss =validation_sample_rsquared,
                  cp = validation_sample_crossproduct,
                  rmse = validation_sample_rmse,
                  mae = validation_sample_mae,
                  mphe = validation_sample_pseudo_huber,
                  mpe = validation_sample_pinball,
                  mape = validation_sample_mape,
                  hr = validation_sample_hr,
                  mb = validation_sample_mb,
                  best_iteration = xgb.mod$best_iteration
      ))

    }

  }

  #Bayes opt
  bayes_opt2 <- doFuture::withDoRNG(
    ParBayesianOptimization::bayesOpt(

      FUN = wrapper_eval_function(custom_obj = custom_obj, quantile_tau = 0.25, early_stop = early_stop), #FUN

      bounds = list(min_child_weight = c(1, 6), max_depth = c(2L, 8L),
                    subsample = c(0.25, 1), colsample_bytree = c(0.25, 1),
                    eta = c(0.02, 0.2), alpha = c(1, 5), gamma = c(0, 5), nrounds = c(200, 1000))
      ,
      initPoints = 10, #Number of randomly chosen points to sample the target function before B.O.
      iters.k = 8,
      acq = "ucb", #Acquisition function to be used
      iters.n = 16, #Number of times BO is to be repeated
      verbose = TRUE,
      parallel = TRUE
    )
  )

  #Features val
  chosen_eval_metric_val[[2]] <- data.frame(min_child_weight = bayes_opt2$scoreSummary$min_child_weight,
                                              max_depth = bayes_opt2$scoreSummary$max_depth,
                                              subsample = bayes_opt2$scoreSummary$subsample,
                                              colsample_bytree = bayes_opt2$scoreSummary$colsample_bytree,
                                              eta = bayes_opt2$scoreSummary$eta,
                                              alpha = bayes_opt2$scoreSummary$alpha,
                                              gamma = bayes_opt2$scoreSummary$gamma,
                                              nrounds = bayes_opt2$scoreSummary$nrounds,
                                              best_iteration = bayes_opt2$scoreSummary$best_iteration,
                                              chosen_eval_metric = rep(NA, length(bayes_opt2$scoreSummary$min_child_weight)))


  chosen_eval_metric_val[[2]]$chosen_eval_metric = as.numeric(bayes_opt2$scoreSummary$mphe)


  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rmse[2] <- bayes_opt2$scoreSummary$rmse[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$rss[2] <- bayes_opt2$scoreSummary$rss[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$cp[2] <- bayes_opt2$scoreSummary$cp[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mae[2] <- bayes_opt2$scoreSummary$mae[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mphe[2] <- bayes_opt2$scoreSummary$mphe[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mpe[2] <- bayes_opt2$scoreSummary$mpe[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mape[2] <- bayes_opt2$scoreSummary$mape[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$hr[2] <- bayes_opt2$scoreSummary$hr[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mb[2] <- bayes_opt2$scoreSummary$mb[which.max(bayes_opt2$scoreSummary$Score)]



  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]

  #Full data
  full_data_second_training_and_validation <- cbind(target_second_training_and_validation$fwd_premium_3m, features_second_training_and_validation[,-c(1:3)])
  colnames(full_data_second_training_and_validation)[2] <- c("fwd_premium_3m")

  #Refitted model
  #Refitted model
  xgb.mod.refit2 <- xgboost::xgb.train(data = xgboost::xgb.DMatrix(data = as.matrix(features_second_training_and_validation[,-c(1:3)]),
                                                                  label = target_second_training_and_validation$fwd_premium_3m),
                                      objective = "reg:pseudohubererror",
                                      min_child_weight = unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['min_child_weight'],
                                      max_depth = unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['max_depth'],
                                      subsample = unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['subsample'],
                                      colsample_bytree = unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['colsample_bytree'],
                                      eta = unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['eta'],
                                      alpha = unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['alpha'],
                                      gamma = unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['gamma'],
                                      nrounds = bayes_opt2$scoreSummary$best_iteration[which.max(bayes_opt2$scoreSummary$Score)],
                                      huber_slope = 1.25)



  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]





  #Predict!
  prediction_list[[3]] <- as.numeric(predict(xgb.mod.refit2, newdata = as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)])))
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  prediction_list[[4]] <- as.numeric(predict(xgb.mod.refit2, newdata = as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)])))
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss = c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA), row.names =   c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(1.25^2*(sqrt(1+((y_list[[l]] - prediction_list[[l]])/(1.25))^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0,
                                                       0.25*(y_list[[l]] - prediction_list[[l]]),
                                                       (1-0.25)*(-1)*(y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))
    oos_testing_eval_metrics$hr[l] <- mean(((y_list[[l]] * prediction_list[[l]]) > 0))
    oos_testing_eval_metrics$mb[l] <- mean((y_list[[l]] - prediction_list[[l]]))



  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  if(all(abs(coef(xgb.mod.refit2) - coef(ml_backtest_results@final_model@model)) < 0.0001)){
    results$outputs[[5]] <- ml_backtest_results@final_model
  }

  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     min_child_weight = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['min_child_weight'],
                                              unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['min_child_weight']),

                                     max_depth = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['max_depth'],
                                                   unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['max_depth']),

                                     subsample = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['subsample'],
                                                   unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['subsample']),

                                     colsample_bytree = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['colsample_bytree'],
                                                    unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['colsample_bytree']),

                                     eta = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['eta'],
                                                          unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['eta']),

                                     alpha = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['alpha'],
                                                          unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['alpha']),

                                     gamma = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['gamma'],
                                               unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['gamma']),

                                     nrounds = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['nrounds'],
                                               unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['nrounds']),

                                     best_iteration = c(bayes_opt1$scoreSummary$best_iteration[which.max(bayes_opt1$scoreSummary$Score)],
                                                        bayes_opt2$scoreSummary$best_iteration[which.max(bayes_opt2$scoreSummary$Score)])
  )




  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-5
  )

  future::plan("sequential")

})

#Define your test
test_that("NN (Parallel = FALSE) - run_ml_backtest works with rebalancing, 3m target, bayesian_opt as tuning method, mphe as chosen eval metric and custom_objective pseudo_huber  -toy_preprocessed_features_and_targets",{

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  future::plan("sequential")

  set.seed(123)
  tensorflow::set_random_seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      ml_algorithm = "nn",
      custom_objective = "pseudo_huber_error",
      target_fwd_name = c("fwd_premium_3m"),
      hyper_grid_domain = list(regularizer_l1 = c(1, 6), regularizer_l2 = c(2, 8),
                                    droprate = c(0.25, 0.9), lr = c(0.25, 1),
                                    size_of_batch = c(256L,512L), number_of_epochs = c(50L, 100L)),
      keras_architecture_parameters = list(units = c(32,16,8), n_layers = 3, activation = c("relu", "relu", "relu"), nn_optimizer = "Adam", batch_norm_option = c(TRUE,TRUE,TRUE)),
      tuning_method = c("bayesian_opt"),
      n_iter = 3,
      k_iter = 1,
      init_points = 8,
      acq = "ucb",
      quantile_tau = 0.25,
      huber_delta = 1.25,
      verbose = TRUE,
      parallel = FALSE,
      show_plots = FALSE,
      early_stop = 10
    )}))



  #Define initial objects
  target_fwd_name <- "fwd_premium_3m"
  chosen_loss <- "mphe"
  early_stop <- 10

  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]
  #Full data
  full_data_first_train <- cbind(targets_first_train$fwd_premium_3m, features_first_train[,-c(1:3)])
  colnames(full_data_first_train)[1] <- c("fwd_premium_3m")


  eval_function <- function(regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs){

      model_nn <- keras::keras_model_sequential()
      model_nn %>%
        keras::layer_dense(units = 32, activation = "relu", input_shape = ncol(features_first_train[,-c(1:3)]),
                           kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
        keras::layer_batch_normalization() %>%
        keras::layer_dropout(rate = droprate) %>%

        keras::layer_dense(units = 16, activation = "relu",
                           kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
        keras::layer_batch_normalization() %>%
        keras::layer_dropout(rate = droprate) %>%

        keras::layer_dense(units = 8, activation = "relu",
                           kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
        keras::layer_batch_normalization() %>%
        keras::layer_dropout(rate = droprate) %>%

        keras::layer_dense(units = 1)

        #Compile
        model_nn %>% keras::compile(
          loss = keras::loss_huber(delta = 1.25),
          optimizer = keras::optimizer_adam(learning_rate = lr),
          metrics = keras::loss_huber(delta = 1.25)
        )

        fit_nn <- model_nn %>%
          keras::fit(x = as.matrix(features_first_train[,-c(1:3)]),
                     y = targets_first_train$fwd_premium_3m,
                     epochs = number_of_epochs,
                     batch_size = size_of_batch,
                     verbose = TRUE,
                     callbacks = list(keras::callback_early_stopping(monitor = "val_huber_loss",
                                                                     patience = 10,
                                                                     restore_best_weights = TRUE,
                                                                     mode = "min")),
                     validation_data = list(as.matrix(features_first_val[,-c(1:3)]), targets_first_val$fwd_premium_3m))


      #Predict validation data
      pred <- stats::predict(model_nn, as.matrix(features_first_val[,-c(1:3)]))

      #Error
      error <- targets_first_val$fwd_premium_3m - pred

      #Calculate loss metrics
      validation_sample_rsquared <- 1 - sum(error^2)/sum(targets_first_val$fwd_premium_3m^2) #R2
      validation_sample_crossproduct <- mean(pred*targets_first_val$fwd_premium_3m) #Cross-Product
      validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
      validation_sample_mae <- mean(abs(error)) #mae
      validation_sample_pseudo_huber <- mean(1.25^2*(sqrt(1+(error/1.25)^2)-1)) #Pseudo Huber
      validation_sample_pinball <- mean(ifelse(error >= 0, 0.25*error, (1-0.25)*-error)) #Pinball
      validation_sample_mape <- mean(abs(error/targets_first_val$fwd_premium_3m)) #mape
      validation_sample_hr <- mean(pred*targets_first_val$fwd_premium_3m >= 0) #mae
      validation_sample_mb <- mean(error) #mae

      return(list(Score = -validation_sample_pseudo_huber, #Pinball
                  rss =validation_sample_rsquared,
                  cp = validation_sample_crossproduct,
                  rmse = validation_sample_rmse,
                  mae = validation_sample_mae,
                  mphe = validation_sample_pseudo_huber,
                  mpe = validation_sample_pinball,
                  mape = validation_sample_mape,
                  hr = validation_sample_hr,
                  mb = validation_sample_mb,
                  best_iteration = which.min(fit_nn$metrics[["val_huber_loss"]])
      ))

    }




  #Bayes opt
  set.seed(123)
  tensorflow::set_random_seed(123)
  bayes_opt1 <- #doFuture::withDoRNG(
    ParBayesianOptimization::bayesOpt(
      FUN = eval_function, #FUN
      bounds = list(regularizer_l1 = c(1, 6), regularizer_l2 = c(2, 8),
                    droprate = c(0.25, 0.9), lr = c(0.25, 1),
                    size_of_batch = c(256L, 512L), number_of_epochs = c(50L, 100L))
      ,
      initPoints = 8, #Number of randomly chosen points to sample the target function before B.O.
      iters.k = 1,
      acq = "ucb", #Acquisition function to be used
      iters.n = 3, #Number of times BO is to be repeated
      parallel = FALSE
    )
  #)



  #Features val
  chosen_eval_metric_val[[1]] <- data.frame(regularizer_l1 = bayes_opt1$scoreSummary$regularizer_l1,
                                            regularizer_l2 = bayes_opt1$scoreSummary$regularizer_l2,
                                            droprate = bayes_opt1$scoreSummary$droprate,
                                            lr = bayes_opt1$scoreSummary$lr,
                                            size_of_batch = bayes_opt1$scoreSummary$size_of_batch,
                                            number_of_epochs = bayes_opt1$scoreSummary$number_of_epochs,
                                            best_iteration = bayes_opt1$scoreSummary$best_iteration,
                                            chosen_eval_metric = rep(NA, length(bayes_opt1$scoreSummary$regularizer_l1)))

  chosen_eval_metric_val[[1]]$chosen_eval_metric = as.numeric(bayes_opt1$scoreSummary$mphe)


  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rmse[1] <- bayes_opt1$scoreSummary$rmse[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$rss[1] <- bayes_opt1$scoreSummary$rss[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$cp[1] <- bayes_opt1$scoreSummary$cp[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mae[1] <- bayes_opt1$scoreSummary$mae[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mphe[1] <- bayes_opt1$scoreSummary$mphe[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mpe[1] <- bayes_opt1$scoreSummary$mpe[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mape[1] <- bayes_opt1$scoreSummary$mape[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$hr[1] <- bayes_opt1$scoreSummary$hr[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mb[1] <- bayes_opt1$scoreSummary$mb[which.max(bayes_opt1$scoreSummary$Score)]




  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]

  #Full data
  full_data_first_training_and_validation <- cbind(target_first_training_and_validation$fwd_premium_3m, features_first_training_and_validation[,-c(1:3)])
  colnames(full_data_first_training_and_validation)[1] <- c("fwd_premium_3m")


  #Refitted model
  nn.mod.refit1 <-  keras::keras_model_sequential()

  nn.mod.refit1 %>%
    keras::layer_dense(units = 32, activation = "relu", input_shape = ncol(features_first_training_and_validation[,-c(1:3)]),
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = ParBayesianOptimization::getBestPars(bayes_opt1)$regularizer_l1,
                                                                     l2 = ParBayesianOptimization::getBestPars(bayes_opt1)$regularizer_l2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = ParBayesianOptimization::getBestPars(bayes_opt1)$droprate) %>%

    keras::layer_dense(units = 16, activation = "relu",
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = ParBayesianOptimization::getBestPars(bayes_opt1)$regularizer_l1,
                                                                     l2 = ParBayesianOptimization::getBestPars(bayes_opt1)$regularizer_l2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = ParBayesianOptimization::getBestPars(bayes_opt1)$droprate) %>%

    keras::layer_dense(units = 8, activation = "relu",
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = ParBayesianOptimization::getBestPars(bayes_opt1)$regularizer_l1,
                                                                     l2 = ParBayesianOptimization::getBestPars(bayes_opt1)$regularizer_l2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = ParBayesianOptimization::getBestPars(bayes_opt1)$droprate) %>%

    keras::layer_dense(units = 1)

  #Compile
  nn.mod.refit1 %>% keras::compile(
    loss = keras::loss_huber(delta = 1.25),
    optimizer = keras::optimizer_adam(learning_rate = ParBayesianOptimization::getBestPars(bayes_opt1)$lr),
    metrics = keras::loss_huber(delta = 1.25)
  )

  nn.fit.refit1 <- nn.mod.refit1 %>%
    keras::fit(x = as.matrix(features_first_training_and_validation[,-c(1:3)]),
               y = target_first_training_and_validation$fwd_premium_3m,
               epochs = bayes_opt1$scoreSummary$best_iteration[which.max(bayes_opt1$scoreSummary$Score)],
               batch_size = ParBayesianOptimization::getBestPars(bayes_opt1)$size_of_batch,
               verbose = TRUE)




  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(predict(nn.mod.refit1, as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)])))
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  prediction_list[[2]] <- as.numeric(predict(nn.mod.refit1, as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)])))
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]

  #Full data
  full_data_second_train <- cbind(targets_second_train$fwd_premium_3m, features_second_train[,-c(1:3)])
  colnames(full_data_second_train)[1] <- c("fwd_premium_3m")


  eval_function <- function(regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs){

    model_nn <- keras::keras_model_sequential()
    model_nn %>%
      keras::layer_dense(units = 32, activation = "relu", input_shape = ncol(features_second_train[,-c(1:3)]),
                         kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_dropout(rate = droprate) %>%

      keras::layer_dense(units = 16, activation = "relu",
                         kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_dropout(rate = droprate) %>%

      keras::layer_dense(units = 8, activation = "relu",
                         kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_dropout(rate = droprate) %>%

      keras::layer_dense(units = 1)

    #Compile
    model_nn %>% keras::compile(
      loss = keras::loss_huber(delta = 1.25),
      optimizer = keras::optimizer_adam(learning_rate = lr),
      metrics = keras::loss_huber(delta = 1.25)
    )

    fit_nn <- model_nn %>%
      keras::fit(x = as.matrix(features_second_train[,-c(1:3)]),
                 y = targets_second_train$fwd_premium_3m,
                 epochs = number_of_epochs,
                 batch_size = size_of_batch,
                 verbose = TRUE,
                 callbacks = list(keras::callback_early_stopping(monitor = "val_huber_loss",
                                                                 patience = 10,
                                                                 restore_best_weights = TRUE,
                                                                 mode = "min")),
                 validation_data = list(as.matrix(features_second_val[,-c(1:3)]), targets_second_val$fwd_premium_3m))


    #Predict validation data
    pred <- stats::predict(model_nn, as.matrix(features_second_val[,-c(1:3)]))

    #Error
    error <- targets_second_val$fwd_premium_3m - pred

    #Calculate loss metrics
    validation_sample_rsquared <- 1 - sum(error^2)/sum(targets_second_val$fwd_premium_3m^2) #R2
    validation_sample_crossproduct <- mean(pred*targets_second_val$fwd_premium_3m) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_pseudo_huber <- mean(1.25^2*(sqrt(1+(error/1.25)^2)-1)) #Pseudo Huber
    validation_sample_pinball <- mean(ifelse(error >= 0, 0.25*error, (1-0.25)*-error)) #Pinball
    validation_sample_mape <- mean(abs(error/targets_second_val$fwd_premium_3m)) #mape
    validation_sample_hr <- mean(pred*targets_second_val$fwd_premium_3m >= 0) #mae
    validation_sample_mb <- mean(error) #mae

    return(list(Score = -validation_sample_pseudo_huber, #Pinball
                rss =validation_sample_rsquared,
                cp = validation_sample_crossproduct,
                rmse = validation_sample_rmse,
                mae = validation_sample_mae,
                mphe = validation_sample_pseudo_huber,
                mpe = validation_sample_pinball,
                mape = validation_sample_mape,
                hr = validation_sample_hr,
                mb = validation_sample_mb,
                best_iteration = which.min(fit_nn$metrics[["val_huber_loss"]])
    ))

  }


  #Bayes opt
  bayes_opt2 <- #doFuture::withDoRNG(
    ParBayesianOptimization::bayesOpt(
      FUN = eval_function, #FUN
      bounds = list(regularizer_l1 = c(1, 6), regularizer_l2 = c(2, 8),
                    droprate = c(0.25, 0.9), lr = c(0.25, 1),
                    size_of_batch = c(256L, 512L), number_of_epochs = c(50L, 100L))
      ,
      initPoints = 8, #Number of randomly chosen points to sample the target function before B.O.
      iters.k = 1,
      acq = "ucb", #Acquisition function to be used
      iters.n = 3, #Number of times BO is to be repeated
      parallel = FALSE
    )
  #)



  #Features val
  chosen_eval_metric_val[[2]] <- data.frame(regularizer_l1 = bayes_opt2$scoreSummary$regularizer_l1,
                                            regularizer_l2 = bayes_opt2$scoreSummary$regularizer_l2,
                                            droprate = bayes_opt2$scoreSummary$droprate,
                                            lr = bayes_opt2$scoreSummary$lr,
                                            size_of_batch = bayes_opt2$scoreSummary$size_of_batch,
                                            number_of_epochs = bayes_opt2$scoreSummary$number_of_epochs,
                                            best_iteration = bayes_opt2$scoreSummary$best_iteration,
                                            chosen_eval_metric = rep(NA, length(bayes_opt2$scoreSummary$regularizer_l1)))

  chosen_eval_metric_val[[2]]$chosen_eval_metric = as.numeric(bayes_opt2$scoreSummary$mphe)


  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rmse[2] <- bayes_opt2$scoreSummary$rmse[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$rss[2] <- bayes_opt2$scoreSummary$rss[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$cp[2] <- bayes_opt2$scoreSummary$cp[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mae[2] <- bayes_opt2$scoreSummary$mae[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mphe[2] <- bayes_opt2$scoreSummary$mphe[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mpe[2] <- bayes_opt2$scoreSummary$mpe[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mape[2] <- bayes_opt2$scoreSummary$mape[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$hr[2] <- bayes_opt2$scoreSummary$hr[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mb[2] <- bayes_opt2$scoreSummary$mb[which.max(bayes_opt2$scoreSummary$Score)]


  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]

  #Full data
  full_data_second_training_and_validation <- cbind(target_second_training_and_validation$fwd_premium_3m, features_second_training_and_validation[,-c(1:3)])
  colnames(full_data_second_training_and_validation)[2] <- c("fwd_premium_3m")

  #Refitted model
  nn.mod.refit2 <-  keras::keras_model_sequential()

  nn.mod.refit2 %>%
    keras::layer_dense(units = 32, activation = "relu", input_shape = ncol(features_second_training_and_validation[,-c(1:3)]),
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = ParBayesianOptimization::getBestPars(bayes_opt2)$regularizer_l1,
                                                                     l2 = ParBayesianOptimization::getBestPars(bayes_opt2)$regularizer_l2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = ParBayesianOptimization::getBestPars(bayes_opt2)$droprate) %>%

    keras::layer_dense(units = 16, activation = "relu",
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = ParBayesianOptimization::getBestPars(bayes_opt2)$regularizer_l1,
                                                                     l2 = ParBayesianOptimization::getBestPars(bayes_opt2)$regularizer_l2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = ParBayesianOptimization::getBestPars(bayes_opt2)$droprate) %>%

    keras::layer_dense(units = 8, activation = "relu",
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = ParBayesianOptimization::getBestPars(bayes_opt2)$regularizer_l1,
                                                                     l2 = ParBayesianOptimization::getBestPars(bayes_opt2)$regularizer_l2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = ParBayesianOptimization::getBestPars(bayes_opt2)$droprate) %>%

    keras::layer_dense(units = 1)

  #Compile
  nn.mod.refit2 %>% keras::compile(
    loss = keras::loss_huber(delta = 1.25),
    optimizer = keras::optimizer_adam(learning_rate = ParBayesianOptimization::getBestPars(bayes_opt2)$lr),
    metrics = keras::loss_huber(delta = 1.25)
  )

  nn.fit.refit2 <- nn.mod.refit2 %>%
    keras::fit(x = as.matrix(features_second_training_and_validation[,-c(1:3)]),
               y = target_second_training_and_validation$fwd_premium_3m,
               epochs = bayes_opt2$scoreSummary$best_iteration[which.max(bayes_opt2$scoreSummary$Score)],
               batch_size = ParBayesianOptimization::getBestPars(bayes_opt2)$size_of_batch,
               verbose = TRUE)


  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]





  #Predict!
  prediction_list[[3]] <- as.numeric(predict(nn.mod.refit2, as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)])))
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  prediction_list[[4]] <- as.numeric(predict(nn.mod.refit2, as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)])))
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss = c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA), row.names =   c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(1.25^2*(sqrt(1+((y_list[[l]] - prediction_list[[l]])/(1.25))^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0,
                                                   0.25*(y_list[[l]] - prediction_list[[l]]),
                                                   (1-0.25)*(-1)*(y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))
    oos_testing_eval_metrics$hr[l] <- mean(((y_list[[l]] * prediction_list[[l]]) > 0))
    oos_testing_eval_metrics$mb[l] <- mean((y_list[[l]] - prediction_list[[l]]))



  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  results$outputs[[5]] <- ml_backtest_results@final_model


  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     regularizer_l1 = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['regularizer_l1'],
                                                        unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['regularizer_l1']),

                                     regularizer_l2 = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['regularizer_l2'],
                                                        unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['regularizer_l2']),

                                     droprate = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['droprate'],
                                                   unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['droprate']),

                                     lr = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['lr'],
                                            unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['lr']),

                                     size_of_batch = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['size_of_batch'],
                                                       unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['size_of_batch']),

                                     number_of_epochs = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['number_of_epochs'],
                                                          unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['number_of_epochs']),

                                     best_iteration = c(bayes_opt1$scoreSummary$best_iteration[which.max(bayes_opt1$scoreSummary$Score)],
                                                        bayes_opt2$scoreSummary$best_iteration[which.max(bayes_opt2$scoreSummary$Score)])
  )




  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-5
  )

  future::plan("sequential")

})

#Define your test
test_that("Skipped: NN (Parallel = TRUE) - run_ml_backtest works with rebalancing, 3m target, bayesian_opt as tuning method, mphe as chosen eval metric and custom_objective pseudo_huber  -toy_preprocessed_features_and_targets",{
skip()
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  doFuture::registerDoFuture()
  future::plan("multisession")

  set.seed(123)
  tensorflow::set_random_seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      ml_algorithm = "nn",
      custom_objective = "pseudo_huber_error",
      target_fwd_name = c("fwd_premium_3m"),
      hyper_grid_domain = list(regularizer_l1 = c(1, 6), regularizer_l2 = c(2, 8),
                                    droprate = c(0.25, 0.9), lr = c(0.25, 1),
                                    size_of_batch = c(256L,512L), number_of_epochs = c(50L, 100L)),
      keras_architecture_parameters = list(units = c(32,16,8), n_layers = 3, activation = c("relu", "relu", "relu"), nn_optimizer = "Adam", batch_norm_option = c(TRUE,TRUE,TRUE)),
      tuning_method = c("bayesian_opt"),
      n_iter = 3,
      k_iter = 1,
      init_points = 8,
      acq = "ucb",
      quantile_tau = 0.25,
      huber_delta = 1.25,
      verbose = TRUE,
      parallel = TRUE,
      show_plots = FALSE,
      early_stop = 10
    )}))



  #Define initial objects
  target_fwd_name <- "fwd_premium_3m"
  chosen_loss <- "mphe"
  early_stop <- 10

  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             mphe = c(NA,NA),
                                             mpe = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]
  #Full data
  full_data_first_train <- cbind(targets_first_train$fwd_premium_3m, features_first_train[,-c(1:3)])
  colnames(full_data_first_train)[1] <- c("fwd_premium_3m")


  eval_function <- function(regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs){

    model_nn <- keras::keras_model_sequential()
    model_nn %>%
      keras::layer_dense(units = 32, activation = "relu", input_shape = ncol(features_first_train[,-c(1:3)]),
                         kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_dropout(rate = droprate) %>%

      keras::layer_dense(units = 16, activation = "relu",
                         kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_dropout(rate = droprate) %>%

      keras::layer_dense(units = 8, activation = "relu",
                         kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_dropout(rate = droprate) %>%

      keras::layer_dense(units = 1)

    #Compile
    model_nn %>% keras::compile(
      loss = keras::loss_huber(delta = 1.25),
      optimizer = keras::optimizer_adam(learning_rate = lr),
      metrics = keras::loss_huber(delta = 1.25)
    )

    fit_nn <- model_nn %>%
      keras::fit(x = as.matrix(features_first_train[,-c(1:3)]),
                 y = targets_first_train$fwd_premium_3m,
                 epochs = number_of_epochs,
                 batch_size = size_of_batch,
                 verbose = TRUE,
                 callbacks = list(keras::callback_early_stopping(monitor = "val_huber_loss",
                                                                 patience = 10,
                                                                 restore_best_weights = TRUE,
                                                                 mode = "min")),
                 validation_data = list(as.matrix(features_first_val[,-c(1:3)]), targets_first_val$fwd_premium_3m))


    #Predict validation data
    pred <- stats::predict(model_nn, as.matrix(features_first_val[,-c(1:3)]))

    #Error
    error <- targets_first_val$fwd_premium_3m - pred

    #Calculate loss metrics
    validation_sample_rsquared <- 1 - sum(error^2)/sum(targets_first_val$fwd_premium_3m^2) #R2
    validation_sample_crossproduct <- mean(pred*targets_first_val$fwd_premium_3m) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_pseudo_huber <- mean(1.25^2*(sqrt(1+(error/1.25)^2)-1)) #Pseudo Huber
    validation_sample_pinball <- mean(ifelse(error >= 0, 0.25*error, (1-0.25)*-error)) #Pinball
    validation_sample_mape <- mean(abs(error/targets_first_val$fwd_premium_3m)) #mape
    validation_sample_hr <- mean(pred*targets_first_val$fwd_premium_3m >= 0) #mae
    validation_sample_mb <- mean(error) #mae

    return(list(Score = -validation_sample_pseudo_huber, #Pinball
                rss =validation_sample_rsquared,
                cp = validation_sample_crossproduct,
                rmse = validation_sample_rmse,
                mae = validation_sample_mae,
                mphe = validation_sample_pseudo_huber,
                mpe = validation_sample_pinball,
                mape = validation_sample_mape,
                hr = validation_sample_hr,
                mb = validation_sample_mb,
                best_iteration = which.min(fit_nn$metrics[["val_huber_loss"]])
    ))

  }




  #Bayes opt
  set.seed(123)
  tensorflow::set_random_seed(123)
  bayes_opt1 <- doFuture::withDoRNG(
    ParBayesianOptimization::bayesOpt(
      FUN = eval_function, #FUN
      bounds = list(regularizer_l1 = c(1, 6), regularizer_l2 = c(2, 8),
                    droprate = c(0.25, 0.9), lr = c(0.25, 1),
                    size_of_batch = c(256L, 512L), number_of_epochs = c(50L, 100L))
      ,
      initPoints = 8, #Number of randomly chosen points to sample the target function before B.O.
      iters.k = 1,
      acq = "ucb", #Acquisition function to be used
      iters.n = 3, #Number of times BO is to be repeated
      parallel = TRUE
    )
  )



  #Features val
  chosen_eval_metric_val[[1]] <- data.frame(regularizer_l1 = bayes_opt1$scoreSummary$regularizer_l1,
                                            regularizer_l2 = bayes_opt1$scoreSummary$regularizer_l2,
                                            droprate = bayes_opt1$scoreSummary$droprate,
                                            lr = bayes_opt1$scoreSummary$lr,
                                            size_of_batch = bayes_opt1$scoreSummary$size_of_batch,
                                            number_of_epochs = bayes_opt1$scoreSummary$number_of_epochs,
                                            best_iteration = bayes_opt1$scoreSummary$best_iteration,
                                            chosen_eval_metric = rep(NA, length(bayes_opt1$scoreSummary$regularizer_l1)))

  chosen_eval_metric_val[[1]]$chosen_eval_metric = as.numeric(bayes_opt1$scoreSummary$mphe)


  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rmse[1] <- bayes_opt1$scoreSummary$rmse[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$rss[1] <- bayes_opt1$scoreSummary$rss[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$cp[1] <- bayes_opt1$scoreSummary$cp[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mae[1] <- bayes_opt1$scoreSummary$mae[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mphe[1] <- bayes_opt1$scoreSummary$mphe[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mpe[1] <- bayes_opt1$scoreSummary$mpe[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mape[1] <- bayes_opt1$scoreSummary$mape[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$hr[1] <- bayes_opt1$scoreSummary$hr[which.max(bayes_opt1$scoreSummary$Score)]
  validation_eval_hyper_choice$mb[1] <- bayes_opt1$scoreSummary$mb[which.max(bayes_opt1$scoreSummary$Score)]




  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]

  #Full data
  full_data_first_training_and_validation <- cbind(target_first_training_and_validation$fwd_premium_3m, features_first_training_and_validation[,-c(1:3)])
  colnames(full_data_first_training_and_validation)[1] <- c("fwd_premium_3m")


  #Refitted model
  nn.mod.refit1 <-  keras::keras_model_sequential()

  nn.mod.refit1 %>%
    keras::layer_dense(units = 32, activation = "relu", input_shape = ncol(features_first_training_and_validation[,-c(1:3)]),
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = ParBayesianOptimization::getBestPars(bayes_opt1)$regularizer_l1,
                                                                     l2 = ParBayesianOptimization::getBestPars(bayes_opt1)$regularizer_l2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = ParBayesianOptimization::getBestPars(bayes_opt1)$droprate) %>%

    keras::layer_dense(units = 16, activation = "relu",
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = ParBayesianOptimization::getBestPars(bayes_opt1)$regularizer_l1,
                                                                     l2 = ParBayesianOptimization::getBestPars(bayes_opt1)$regularizer_l2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = ParBayesianOptimization::getBestPars(bayes_opt1)$droprate) %>%

    keras::layer_dense(units = 8, activation = "relu",
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = ParBayesianOptimization::getBestPars(bayes_opt1)$regularizer_l1,
                                                                     l2 = ParBayesianOptimization::getBestPars(bayes_opt1)$regularizer_l2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = ParBayesianOptimization::getBestPars(bayes_opt1)$droprate) %>%

    keras::layer_dense(units = 1)

  #Compile
  nn.mod.refit1 %>% keras::compile(
    loss = keras::loss_huber(delta = 1.25),
    optimizer = keras::optimizer_adam(learning_rate = ParBayesianOptimization::getBestPars(bayes_opt1)$lr),
    metrics = keras::loss_huber(delta = 1.25)
  )

  nn.fit.refit1 <- nn.mod.refit1 %>%
    keras::fit(x = as.matrix(features_first_training_and_validation[,-c(1:3)]),
               y = target_first_training_and_validation$fwd_premium_3m,
               epochs = bayes_opt1$scoreSummary$best_iteration[which.max(bayes_opt1$scoreSummary$Score)],
               batch_size = ParBayesianOptimization::getBestPars(bayes_opt1)$size_of_batch,
               verbose = TRUE)




  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(predict(nn.mod.refit1, as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)])))
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  prediction_list[[2]] <- as.numeric(predict(nn.mod.refit1, as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)])))
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]

  #Full data
  full_data_second_train <- cbind(targets_second_train$fwd_premium_3m, features_second_train[,-c(1:3)])
  colnames(full_data_second_train)[1] <- c("fwd_premium_3m")


  eval_function <- function(regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs){

    model_nn <- keras::keras_model_sequential()
    model_nn %>%
      keras::layer_dense(units = 32, activation = "relu", input_shape = ncol(features_second_train[,-c(1:3)]),
                         kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_dropout(rate = droprate) %>%

      keras::layer_dense(units = 16, activation = "relu",
                         kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_dropout(rate = droprate) %>%

      keras::layer_dense(units = 8, activation = "relu",
                         kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_dropout(rate = droprate) %>%

      keras::layer_dense(units = 1)

    #Compile
    model_nn %>% keras::compile(
      loss = keras::loss_huber(delta = 1.25),
      optimizer = keras::optimizer_adam(learning_rate = lr),
      metrics = keras::loss_huber(delta = 1.25)
    )

    fit_nn <- model_nn %>%
      keras::fit(x = as.matrix(features_second_train[,-c(1:3)]),
                 y = targets_second_train$fwd_premium_3m,
                 epochs = number_of_epochs,
                 batch_size = size_of_batch,
                 verbose = TRUE,
                 callbacks = list(keras::callback_early_stopping(monitor = "val_huber_loss",
                                                                 patience = 10,
                                                                 restore_best_weights = TRUE,
                                                                 mode = "min")),
                 validation_data = list(as.matrix(features_second_val[,-c(1:3)]), targets_second_val$fwd_premium_3m))


    #Predict validation data
    pred <- stats::predict(model_nn, as.matrix(features_second_val[,-c(1:3)]))

    #Error
    error <- targets_second_val$fwd_premium_3m - pred

    #Calculate loss metrics
    validation_sample_rsquared <- 1 - sum(error^2)/sum(targets_second_val$fwd_premium_3m^2) #R2
    validation_sample_crossproduct <- mean(pred*targets_second_val$fwd_premium_3m) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_pseudo_huber <- mean(1.25^2*(sqrt(1+(error/1.25)^2)-1)) #Pseudo Huber
    validation_sample_pinball <- mean(ifelse(error >= 0, 0.25*error, (1-0.25)*-error)) #Pinball
    validation_sample_mape <- mean(abs(error/targets_second_val$fwd_premium_3m)) #mape
    validation_sample_hr <- mean(pred*targets_second_val$fwd_premium_3m >= 0) #mae
    validation_sample_mb <- mean(error) #mae

    return(list(Score = -validation_sample_pseudo_huber, #Pinball
                rss =validation_sample_rsquared,
                cp = validation_sample_crossproduct,
                rmse = validation_sample_rmse,
                mae = validation_sample_mae,
                mphe = validation_sample_pseudo_huber,
                mpe = validation_sample_pinball,
                mape = validation_sample_mape,
                hr = validation_sample_hr,
                mb = validation_sample_mb,
                best_iteration = which.min(fit_nn$metrics[["val_huber_loss"]])
    ))

  }


  #Bayes opt
  bayes_opt2 <- doFuture::withDoRNG(
    ParBayesianOptimization::bayesOpt(
      FUN = eval_function, #FUN
      bounds = list(regularizer_l1 = c(1, 6), regularizer_l2 = c(2, 8),
                    droprate = c(0.25, 0.9), lr = c(0.25, 1),
                    size_of_batch = c(256L, 512L), number_of_epochs = c(50L, 100L))
      ,
      initPoints = 8, #Number of randomly chosen points to sample the target function before B.O.
      iters.k = 1,
      acq = "ucb", #Acquisition function to be used
      iters.n = 3, #Number of times BO is to be repeated
      parallel = TRUE
    )
  )



  #Features val
  chosen_eval_metric_val[[2]] <- data.frame(regularizer_l1 = bayes_opt2$scoreSummary$regularizer_l1,
                                            regularizer_l2 = bayes_opt2$scoreSummary$regularizer_l2,
                                            droprate = bayes_opt2$scoreSummary$droprate,
                                            lr = bayes_opt2$scoreSummary$lr,
                                            size_of_batch = bayes_opt2$scoreSummary$size_of_batch,
                                            number_of_epochs = bayes_opt2$scoreSummary$number_of_epochs,
                                            best_iteration = bayes_opt2$scoreSummary$best_iteration,
                                            chosen_eval_metric = rep(NA, length(bayes_opt2$scoreSummary$regularizer_l1)))

  chosen_eval_metric_val[[2]]$chosen_eval_metric = as.numeric(bayes_opt2$scoreSummary$mphe)


  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rmse[2] <- bayes_opt2$scoreSummary$rmse[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$rss[2] <- bayes_opt2$scoreSummary$rss[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$cp[2] <- bayes_opt2$scoreSummary$cp[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mae[2] <- bayes_opt2$scoreSummary$mae[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mphe[2] <- bayes_opt2$scoreSummary$mphe[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mpe[2] <- bayes_opt2$scoreSummary$mpe[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mape[2] <- bayes_opt2$scoreSummary$mape[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$hr[2] <- bayes_opt2$scoreSummary$hr[which.max(bayes_opt2$scoreSummary$Score)]
  validation_eval_hyper_choice$mb[2] <- bayes_opt2$scoreSummary$mb[which.max(bayes_opt2$scoreSummary$Score)]


  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]

  #Full data
  full_data_second_training_and_validation <- cbind(target_second_training_and_validation$fwd_premium_3m, features_second_training_and_validation[,-c(1:3)])
  colnames(full_data_second_training_and_validation)[2] <- c("fwd_premium_3m")

  #Refitted model
  nn.mod.refit2 <-  keras::keras_model_sequential()

  nn.mod.refit2 %>%
    keras::layer_dense(units = 32, activation = "relu", input_shape = ncol(features_second_training_and_validation[,-c(1:3)]),
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = ParBayesianOptimization::getBestPars(bayes_opt2)$regularizer_l1,
                                                                     l2 = ParBayesianOptimization::getBestPars(bayes_opt2)$regularizer_l2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = ParBayesianOptimization::getBestPars(bayes_opt2)$droprate) %>%

    keras::layer_dense(units = 16, activation = "relu",
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = ParBayesianOptimization::getBestPars(bayes_opt2)$regularizer_l1,
                                                                     l2 = ParBayesianOptimization::getBestPars(bayes_opt2)$regularizer_l2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = ParBayesianOptimization::getBestPars(bayes_opt2)$droprate) %>%

    keras::layer_dense(units = 8, activation = "relu",
                       kernel_regularizer = keras::regularizer_l1_l2(l1 = ParBayesianOptimization::getBestPars(bayes_opt2)$regularizer_l1,
                                                                     l2 = ParBayesianOptimization::getBestPars(bayes_opt2)$regularizer_l2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(rate = ParBayesianOptimization::getBestPars(bayes_opt2)$droprate) %>%

    keras::layer_dense(units = 1)

  #Compile
  nn.mod.refit2 %>% keras::compile(
    loss = keras::loss_huber(delta = 1.25),
    optimizer = keras::optimizer_adam(learning_rate = ParBayesianOptimization::getBestPars(bayes_opt2)$lr),
    metrics = keras::loss_huber(delta = 1.25)
  )

  nn.fit.refit2 <- nn.mod.refit2 %>%
    keras::fit(x = as.matrix(features_second_training_and_validation[,-c(1:3)]),
               y = target_second_training_and_validation$fwd_premium_3m,
               epochs = bayes_opt2$scoreSummary$best_iteration[which.max(bayes_opt2$scoreSummary$Score)],
               batch_size = ParBayesianOptimization::getBestPars(bayes_opt2)$size_of_batch,
               verbose = TRUE)


  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]





  #Predict!
  prediction_list[[3]] <- as.numeric(predict(nn.mod.refit2, as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)])))
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  prediction_list[[4]] <- as.numeric(predict(nn.mod.refit2, as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)])))
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss = c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA), row.names =   c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(1.25^2*(sqrt(1+((y_list[[l]] - prediction_list[[l]])/(1.25))^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0,
                                                   0.25*(y_list[[l]] - prediction_list[[l]]),
                                                   (1-0.25)*(-1)*(y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))
    oos_testing_eval_metrics$hr[l] <- mean(((y_list[[l]] * prediction_list[[l]]) > 0))
    oos_testing_eval_metrics$mb[l] <- mean((y_list[[l]] - prediction_list[[l]]))



  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  results$outputs[[5]] <- ml_backtest_results@final_model


  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     regularizer_l1 = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['regularizer_l1'],
                                                        unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['regularizer_l1']),

                                     regularizer_l2 = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['regularizer_l2'],
                                                        unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['regularizer_l2']),

                                     droprate = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['droprate'],
                                                  unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['droprate']),

                                     lr = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['lr'],
                                            unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['lr']),

                                     size_of_batch = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['size_of_batch'],
                                                       unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['size_of_batch']),

                                     number_of_epochs = c(unlist(ParBayesianOptimization::getBestPars(bayes_opt1))['number_of_epochs'],
                                                          unlist(ParBayesianOptimization::getBestPars(bayes_opt2))['number_of_epochs']),

                                     best_iteration = c(bayes_opt1$scoreSummary$best_iteration[which.max(bayes_opt1$scoreSummary$Score)],
                                                        bayes_opt2$scoreSummary$best_iteration[which.max(bayes_opt2$scoreSummary$Score)])
  )




  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-5
  )

  future::plan("sequential")

})


###################
#END ML TESTS


#BEGIN OTHER TESTS
#####################################
test_that("run_ml_backtest correctly classifies data as training, validation and testing", {

   load(paste(test_path(),"/testdata/","toy_fulldates_features_and_targets.RData", sep =""))


  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_features_full_dates,
      target_m_df = toy_target_full_date,
      training_sample_size = 60,
      validation_sample_size = 36,
      rebalancing_months = 6,
      ml_algorithm = "xgb",
      custom_objective = "pseudo_huber_error",
      target_fwd_name = c("fwd_premium_3m"),
      hyper_grid_domain = list(min_child_weight = c(1), max_depth = c(6),
                                    subsample = c(0.75), colsample_bytree = c(1),
                                    eta = c(0.1), alpha = c(2), gamma = c(0), nrounds = c(50)),
      tuning_method = c("grid_search"),
      verbose = FALSE,
      show_plots = FALSE
    )}))

  rebalance_dates <- as.Date(rownames(ml_backtest_results@validation_eval_metrics_hyper_choice), format = "%Y-%m-%d")


  #Check if rebalance dates match expected months
  expect_equal(unique(lubridate::month(rebalance_dates[-1])), 6)
  expect_equal(as.Date(rebalance_dates[-1], format = "%Y-%m-%d"),
               seq.Date(from = as.Date(toy_dates_full_dates[order(toy_dates_full_dates)], format = "%Y-%m-%d")[96+4], to = toy_dates_full_dates[length(toy_dates_full_dates)], by = "year"))
  expect_equal(unique(lubridate::month(rebalance_dates[1])),
               lubridate::month(as.Date(toy_dates_full_dates[order(toy_dates_full_dates)], format = "%Y-%m-%d")[96]))


})

#Define your test
test_that("run_ml_backtest works with NAs in last target_fwd periods of target_m_df",{

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-07-15", "2023-06-15", "2023-05-15")), "fwd_return_3m"] <- NA
  toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-07-15", "2023-06-15", "2023-05-15")), "fwd_premium_3m"] <- NA
  toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-07-15")), "fwd_return_1m"] <- NA
  toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-07-15")), "fwd_premium_1m"] <- NA


  set.seed(123)
  #Apply function
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      ml_algorithm = "glmnet",
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "rss",
      hyper_grid_domain = list(alpha = list(distribution_choice = "uniform", pars = c(min = 0,max = 1)),
                                    lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.9))), #Random Search
      tuning_method = c("random_search"),
      n_iter = 5,
      parallel = FALSE,
      verbose = TRUE,
      show_plots = FALSE
    )}))


  #Define initial objects
  set.seed(123)
  hyper_expanded_grid1 <- list(alpha = runif(n = 5, min = 0, max = 1), lambda.min.ratio = runif(n = 5, min = 0.1, max = 0.9))
  hyper_expanded_grid1$alpha <- unique(hyper_expanded_grid1$alpha)
  hyper_expanded_grid1$lambda.min.ratio <- unique(hyper_expanded_grid1$lambda.min.ratio)
  hyper_expanded_grid1 <- expand.grid(hyper_expanded_grid1)

  hyper_expanded_grid2 <- list(alpha = runif(n = 5, min = 0, max = 1), lambda.min.ratio = runif(n = 5, min = 0.1, max = 0.9))
  hyper_expanded_grid2$alpha <- unique(hyper_expanded_grid2$alpha)
  hyper_expanded_grid2$lambda.min.ratio <- unique(hyper_expanded_grid2$lambda.min.ratio)
  hyper_expanded_grid2 <- expand.grid(hyper_expanded_grid2)


  validation_eval_hyper_choice <- data.frame(rss =c(NA,NA),  #Validation loss df
                                             cp = c(NA,NA),
                                             rmse = c(NA,NA),
                                             mae = c(NA,NA),
                                             row.names = c("2023-04-15", "2023-06-15"))
  rebalance_dates <- c("2023-04-15", "2023-06-15")
  n_rebalance_dates <- 2

  chosen_eval_metric_val <- list()

  #1st rebalancing
  #Features obj
  features_first_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15", "2022-09-15", "2022-10-15")),]
  features_first_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-01-15")),]
  #Targets
  targets_first_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15")),]
  targets_first_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-01-15")),]
  #Features val
  #Start first rebalancing
  chosen_eval_metric_val[[1]] <- data.frame(alpha = hyper_expanded_grid1$alpha,
                                            lambda.min.ratio = hyper_expanded_grid1$lambda.min.ratio,
                                            best_lam = rep(NA,25), chosen_eval_metric = rep(NA, 25))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(targets_first_val$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid1)))

  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[1]])
  best_lam1 <- vector(length =  nrow(hyper_expanded_grid1))

  for(s in 1:length(hyper_expanded_grid1$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_first_train[,-c(1:3)],
      y = targets_first_train$fwd_premium_3m,
      alpha = hyper_expanded_grid1$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid1$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam1[s] <- glm.mod1$lambda[
      which.max(1 - (colSums((targets_first_val$fwd_premium_3m -
                                predict(glm.mod1, newx = as.matrix(features_first_val[,-c(1:3)])))^2)/sum(targets_first_val$fwd_premium_3m^2)))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_first_val[,-c(1:3)]), s = best_lam1[s])

    #RSQUARED CHOSEN
    chosen_eval_metric_val[[1]]$chosen_eval_metric[s] <-
      (1 - (sum((targets_first_val$fwd_premium_3m -
                   shrinkage.pred_df[,s])^2)/sum(targets_first_val$fwd_premium_3m^2)))



  }
  chosen_eval_metric_val[[1]]$best_lam <- best_lam1

  #rsquared IS MAX: PAY ATTENTION
  hyper_choice1 <- which.max(chosen_eval_metric_val[[1]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[1] <- (1 - (sum((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2)/sum(targets_first_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[1] <- sqrt(mean((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])^2))

  validation_eval_hyper_choice$cp[1] <- mean(targets_first_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice1])

  validation_eval_hyper_choice$mae[1] <- mean(abs(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]))

  validation_eval_hyper_choice$mphe[1] <- mean((1)^2*(sqrt(1+((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/
                                                                (1))^2)-1))

  validation_eval_hyper_choice$mpe[1] <- mean(ifelse((targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]) >= 0,
                                                     0.5*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1]),
                                                     (1-0.5)*(-1)*(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])))

  validation_eval_hyper_choice$mape[1] <- mean(abs(
    (targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])/targets_first_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[1] <- length(which(sign(targets_first_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice1])))/
    length(targets_first_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[1] <- mean(targets_first_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice1])




  #Refit
  features_first_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                   "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  target_first_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                               "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15")),]


  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_first_training_and_validation[,-c(1:3)],
                                  y = target_first_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid1$alpha[hyper_choice1],
                                  lambda.min.ratio = hyper_expanded_grid1$lambda.min.ratio[hyper_choice1])
  coef(glm.mod.refit)


  #First test set
  features_first_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-04-15","2023-05-15")),]
  target_first_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-04-15","2023-05-15")),]



  #Predict!
  prediction_list <- list()
  prediction_list[[1]] <- as.numeric(predict(glm.mod.refit, newx = as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-04-15")),-c(1:3)]),
                                             s = best_lam1[hyper_choice1]))
  names(prediction_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  prediction_list[[2]] <- as.numeric(predict(glm.mod.refit, newx = as.matrix(features_first_test[which(features_first_test$dates %in% c("2023-05-15")),-c(1:3)]),
                                             s = best_lam1[hyper_choice1]))
  names(prediction_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Calc error
  error_list <- list()
  error_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] - as.numeric(prediction_list[[1]])
  names(error_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  error_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] - as.numeric(prediction_list[[2]])
  names(error_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #Y
  y_list <- list()
  y_list[[1]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-04-15"))] %>% as.numeric()
  names(y_list[[1]]) <- features_first_test[which(features_first_test$dates %in% c("2023-04-15")),2]
  y_list[[2]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-05-15"))] %>% as.numeric()
  names(y_list[[2]]) <- features_first_test[which(features_first_test$dates %in% c("2023-05-15")),2]

  #2nd rebal!
  #Features obj
  features_second_train <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                                  "2022-11-15", "2022-12-15")),]
  features_second_val <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-03-15")),]
  #Targets
  targets_second_train <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15","2022-08-15","2022-09-15","2022-10-15",
                                                                                               "2022-11-15", "2022-12-15")),]
  targets_second_val <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-03-15")),]



  chosen_eval_metric_val[[2]] <- data.frame(alpha = hyper_expanded_grid2$alpha,
                                            lambda.min.ratio = hyper_expanded_grid2$lambda.min.ratio,
                                            best_lam = rep(NA,25), chosen_eval_metric = rep(NA, 25))

  shrinkage.pred_df <- data.frame(matrix(NA, nrow = length(targets_second_val$fwd_premium_3m),
                                         ncol = nrow(hyper_expanded_grid2)))

  colnames(shrinkage.pred_df) <- rownames(chosen_eval_metric_val[[2]])
  best_lam2 <- vector(length =  nrow(hyper_expanded_grid2))

  for(s in 1:length(hyper_expanded_grid2$alpha)){
    #Train Model
    glm.mod1 <- glmnet::glmnet(
      x = features_second_train[,-c(1:3)],
      y = targets_second_train$fwd_premium_3m,
      alpha = hyper_expanded_grid2$alpha[s], #Alpha
      lambda.min.ratio = hyper_expanded_grid2$lambda.min.ratio[s] #Lambda
    )

    #Get best lam
    best_lam2[s] <- glm.mod1$lambda[
      which.max(1 - (colSums((targets_second_val$fwd_premium_3m -
                                predict(glm.mod1, newx = as.matrix(features_second_val[,-c(1:3)])))^2)/sum(targets_second_val$fwd_premium_3m^2)))
    ]


    #Predict to validation data
    shrinkage.pred_df[,s] <-
      predict(glm.mod1, newx = as.matrix(features_second_val[,-c(1:3)]), s = best_lam2[s])

    #RSQUARED CHOSEN
    chosen_eval_metric_val[[2]]$chosen_eval_metric[s] <-
      (1 - (sum((targets_second_val$fwd_premium_3m -
                   shrinkage.pred_df[,s])^2)/sum(targets_second_val$fwd_premium_3m^2)))



  }
  chosen_eval_metric_val[[2]]$best_lam <- best_lam2

  #r2 IS MAX: PAY ATTENTION
  hyper_choice2 <- which.max(chosen_eval_metric_val[[2]]$chosen_eval_metric)

  #Calculate val losses for best hyper choice
  validation_eval_hyper_choice$rss[2] <- (1 - (sum((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2)/sum(targets_second_val$fwd_premium_3m^2)))

  validation_eval_hyper_choice$rmse[2] <- sqrt(mean((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])^2))

  validation_eval_hyper_choice$cp[2] <- mean(targets_second_val$fwd_premium_3m*shrinkage.pred_df[,hyper_choice2])

  validation_eval_hyper_choice$mae[2] <- mean(abs(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]))

  validation_eval_hyper_choice$mphe[2] <- mean((1)^2*(sqrt(1+((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/
                                                                (1))^2)-1))

  validation_eval_hyper_choice$mpe[2] <- mean(ifelse((targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]) >= 0,
                                                     0.5*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2]),
                                                     (1-0.5)*(-1)*(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])))

  validation_eval_hyper_choice$mape[2] <- mean(abs(
    (targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])/targets_second_val$fwd_premium_3m))

  validation_eval_hyper_choice$hr[2] <- length(which(sign(targets_second_val$fwd_premium_3m) == sign(shrinkage.pred_df[,hyper_choice2])))/
    length(targets_second_val$fwd_premium_3m)

  validation_eval_hyper_choice$mb[2] <- mean(targets_second_val$fwd_premium_3m - shrinkage.pred_df[,hyper_choice2])



  #Refit
  features_second_training_and_validation <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                    "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                    "2023-02-15", "2023-03-15")),]


  target_second_training_and_validation <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2022-07-15", "2022-08-15", "2022-09-15",
                                                                                                                "2022-10-15", "2022-11-15", "2022-12-15", "2023-01-15",
                                                                                                                "2023-02-15", "2023-03-15")),]


  #Refitted model
  glm.mod.refit <- glmnet::glmnet(x = features_second_training_and_validation[,-c(1:3)],
                                  y = target_second_training_and_validation$fwd_premium_3m,
                                  alpha = hyper_expanded_grid2$alpha[hyper_choice2],
                                  lambda.min.ratio = hyper_expanded_grid2$lambda.min.ratio[hyper_choice2])
  coef(glm.mod.refit)



  #second test set
  features_second_test <- toy_preprocessed_features[which(toy_preprocessed_features$dates %in% c("2023-06-15","2023-07-15")),]
  target_second_test <- toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-06-15","2023-07-15")),]



  #Predict!
  prediction_list[[3]] <- as.numeric(predict(glm.mod.refit, newx = as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-06-15")),-c(1:3)]),
                                             s = best_lam2[hyper_choice2]))
  names(prediction_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  prediction_list[[4]] <- as.numeric(predict(glm.mod.refit, newx = as.matrix(features_second_test[which(features_second_test$dates %in% c("2023-07-15")),-c(1:3)]),
                                             s = best_lam2[hyper_choice2]))
  names(prediction_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Calc error
  error_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] - as.numeric(prediction_list[[3]])
  names(error_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  error_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] - as.numeric(prediction_list[[4]])
  names(error_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]

  #Y
  y_list[[3]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-06-15"))] %>% as.numeric()
  names(y_list[[3]]) <- features_second_test[which(features_second_test$dates %in% c("2023-06-15")),2]
  y_list[[4]] <- toy_preprocessed_targets$fwd_premium_3m[which(toy_preprocessed_targets$dates %in% c("2023-07-15"))] %>% as.numeric()
  names(y_list[[4]]) <- features_second_test[which(features_second_test$dates %in% c("2023-07-15")),2]


  #Create results object
  results <- list()
  results[[1]] <- list()
  names(results) <- c("outputs")

  #Create results object
  #Pred list
  names(prediction_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[1]] <- prediction_list
  #Error list
  names(error_list) <- c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[2]] <- error_list
  #Y-list
  names(y_list) <-  c("2023-04-15","2023-05-15", "2023-06-15", "2023-07-15")
  results$outputs[[3]] <- y_list

  #Eval metrics
  oos_testing_eval_metrics <- data.frame(rss =c(NA,NA,NA,NA),
                                         cp = c(NA,NA,NA,NA),
                                         rmse = c(NA,NA,NA,NA),
                                         mae = c(NA,NA,NA,NA), row.names = c("2023-04-15","2023-05-15", "2023-06-15","2023-07-15"))

  for(l in 1:length(prediction_list)){
    oos_testing_eval_metrics$rss[l] <- 1 - ((sum((y_list[[l]] - prediction_list[[l]])^2))/sum(y_list[[l]]^2))
    oos_testing_eval_metrics$rmse[l] <- sqrt(mean((y_list[[l]] - prediction_list[[l]])^2))
    oos_testing_eval_metrics$cp[l] <- mean(y_list[[l]]*prediction_list[[l]])
    oos_testing_eval_metrics$mae[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mphe[l] <- mean(1^2*(sqrt(1+(y_list[[l]] - prediction_list[[l]])^2)-1))
    oos_testing_eval_metrics$mpe[l] <- mean(ifelse((y_list[[l]] - prediction_list[[l]]) >= 0,
                                                   0.5*(y_list[[l]] - prediction_list[[l]]),
                                                   (1-0.5)*(-1)*(y_list[[l]] - prediction_list[[l]])))
    oos_testing_eval_metrics$mape[l] <- mean(abs((y_list[[l]] - prediction_list[[l]])/y_list[[l]]))
    oos_testing_eval_metrics$hr[l] <- mean((y_list[[l]] * prediction_list[[l]])>0)
    oos_testing_eval_metrics$mb[l] <- mean(y_list[[l]] - prediction_list[[l]])

  }

  results$outputs[[4]] <- oos_testing_eval_metrics

  #Final Model
  results$outputs[[5]] <- ml_backtest_results@final_model


  #Validation lossess for chosen metric
  names(chosen_eval_metric_val) <- rebalance_dates
  results$outputs[[6]] <- chosen_eval_metric_val

  #Best Hyoer
  results$outputs[[7]] <- data.frame(row.names = rebalance_dates,
                                     alpha = c(hyper_expanded_grid1$alpha[hyper_choice1], hyper_expanded_grid2$alpha[hyper_choice2]),
                                     lambda.min.ratio = c(hyper_expanded_grid1$lambda.min.ratio[hyper_choice1], hyper_expanded_grid2$lambda.min.ratio[hyper_choice2]),
                                     best_lam = c(best_lam1[hyper_choice1], best_lam2[hyper_choice2]))

  #Validation loss metrics for hyper choice
  results$outputs[[8]] <- validation_eval_hyper_choice
  #Rename
  names(results$outputs) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics",
                              "final_model", "chosen_eval_metric_validation",
                              "best_hyperparameters", "validation_eval_metrics_hyper_choice")

  ml_backtest_results <- as.list(ml_backtest_results)
  ml_backtest_results$metadata <- NULL


  expect_equal(
    ml_backtest_results,
    results$outputs,
    tolerance = 1e-5
  )

})

#Define your test
test_that("run_ml_backtest does not works with NAs in last target_fwd+ 1 periods of target_m_df",{

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-07-15", "2023-06-15", "2023-05-15", "2023-04-15")), "fwd_return_3m"] <- NA
  toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-07-15", "2023-06-15", "2023-05-15", "2023-04-15")), "fwd_premium_3m"] <- NA
  toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-07-15")), "fwd_return_1m"] <- NA
  toy_preprocessed_targets[which(toy_preprocessed_targets$dates %in% c("2023-07-15")), "fwd_premium_1m"] <- NA

  #Apply function
  expect_error(
  suppressMessages(suppressWarnings({
    ml_backtest_results <- run_ml_backtest(
      features_m_df = toy_preprocessed_features,
      target_m_df = toy_preprocessed_targets,
      training_sample_size = 7,
      validation_sample_size = 3,
      rebalancing_months = 6,
      ml_algorithm = "glmnet",
      target_fwd_name = c("fwd_premium_3m"),
      chosen_eval_metric  = "rss",
      hyper_grid_domain = list(alpha = list(distribution_choice = "uniform", pars = c(min = 0,max = 1)),
                                    lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.9))), #Random Search
      tuning_method = c("random_search"),
      n_iter = 5,
      parallel = FALSE,
      verbose = TRUE,
      show_plots = FALSE
    )})),
  "target_m_df can't have NAs until the last target_fwd periods")




})


#####################################
#END OTHER TESTS


