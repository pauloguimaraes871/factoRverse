test_that("convert_oos_predictions_lists_to_m_df returns a meta_dataframe with expected format", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  ols_config <- create_sb_backtest_config(sb_algorithm = "ols", custom_objective = "squared_error", target_fwd_name = "fwd_premium_3m",
                                          training_sample_size = 9, rebalancing_months = 6)

  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", training_sample_size = 6, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m") %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))

  rf_config <- create_sb_backtest_config(sb_algorithm = "rf", training_sample_size = 6, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m") %>%
    add_tuning_strategy(tuning_method = "random_search", validation_sample_size = 3, n_iter = 2) %>%
    add_hyperparameter(hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
                       distribution_choice = c("uniform", "uniform", "lognormal", "uniform"),
                       pars = list(c(min=0.1, max = 0.9), c(min = 100L, max = 500L), c(meanlog = 1L, sdlog = 1L),
                                   c(min = 1L, max = 10L))
    )


  set.seed(123)
  ols_results <- run_sb_backtest(
    features_m_df = create_meta_dataframe(toy_preprocessed_features),
    target_m_df = create_meta_dataframe(toy_preprocessed_targets),
    target_fwd_name = "fwd_premium_3m",
    config = ols_config,
    verbose = TRUE,
    parallel = FALSE
  )

  glmnet_results <- run_sb_backtest(
    features_m_df = create_meta_dataframe(toy_preprocessed_features),
    target_m_df = create_meta_dataframe(toy_preprocessed_targets),
    target_fwd_name = "fwd_premium_3m",
    config = glmnet_config,
    verbose = TRUE,
    parallel = FALSE
  )

  rf_results <- run_sb_backtest(
    features_m_df = create_meta_dataframe(toy_preprocessed_features),
    target_m_df = create_meta_dataframe(toy_preprocessed_targets),
    target_fwd_name = "fwd_premium_3m",
    config = rf_config,
    verbose = TRUE,
    parallel = FALSE
  )

  #Get ml_backtest_results
  sb_backtest_results_list <- list(ols_config = ols_results, glmnet_config = glmnet_results, rf_config = rf_results)

  predictions_m_df <- convert_oos_predictions_lists_to_m_df(sb_backtest_results_list, winsorize_predictions = FALSE, normalize_predictions = FALSE)

  #Expect class to be meta_dataframe
  expect_equal(as.character(class(predictions_m_df)), "meta_dataframe")
  #Expect number of stocks to be qual
  expect_equal(predictions_m_df@unique_tickers, length(unique(unlist(sapply(ols_results@oos_prediction_list, function(x) names(x))))))
  #Expect number of months to be equal
  expect_equal(predictions_m_df@unique_dates, length(names(ols_results@oos_prediction_list)))
  #Expect columns to be configs names
  expect_equal(names(predictions_m_df@data), c("id", "tickers", "dates", names(sb_backtest_results_list)))
  #Expect predictions to be correctly set
  ols_predictions_from_predictions_m_df <- predictions_m_df@data[which(predictions_m_df@data$dates == "2023-06-15"), c("ols_config")]
  names(ols_predictions_from_predictions_m_df) <- predictions_m_df@data[which(predictions_m_df@data$dates == "2023-06-15"), c("tickers")]
  expect_equal(ols_predictions_from_predictions_m_df, ols_results@oos_prediction_list[["2023-06-15"]])
  rf_predictions_from_predictions_m_df <- predictions_m_df@data[which(predictions_m_df@data$dates == "2023-03-15"), c("rf_config")]
  names(rf_predictions_from_predictions_m_df) <- predictions_m_df@data[which(predictions_m_df@data$dates == "2023-03-15"), c("tickers")]
  expect_equal(rf_predictions_from_predictions_m_df, rf_results@oos_prediction_list[["2023-03-15"]])
  glmnet_predictions_from_predictions_m_df <- predictions_m_df@data[which(predictions_m_df@data$dates == "2023-04-15"), c("glmnet_config")]
  names(glmnet_predictions_from_predictions_m_df) <- predictions_m_df@data[which(predictions_m_df@data$dates == "2023-04-15"), c("tickers")]
  expect_equal(glmnet_predictions_from_predictions_m_df, glmnet_results@oos_prediction_list[["2023-04-15"]])

  ols_results_incomplete <- ols_results
  ols_results_incomplete@oos_prediction_list <- ols_results_incomplete@oos_prediction_list[-2]
  sb_backtest_results_list_incomplete <- list(ols_config = ols_results_incomplete, glmnet_config = glmnet_results, rf_config = rf_results)

  #Error checking
  expect_error(
    convert_oos_predictions_list_to_m_df(sb_backtest_results_list_incomplete, winsorize_predictions = FALSE, normalize_predictions = FALSE),
    "Length of oos_prediction_list in each ml_backtest_results object must be the same."
  )

  ols_results_incomplete <- ols_results
  ols_results_incomplete@oos_prediction_list[[2]] <- ols_results_incomplete@oos_prediction_list[[2]][-2]
  sb_backtest_results_list_incomplete <- list(ols_config = ols_results_incomplete, glmnet_config = glmnet_results, rf_config = rf_results)

  expect_error(
    convert_oos_predictions_list_to_m_df(sb_backtest_results_list_incomplete, winsorize_predictions = FALSE, normalize_predictions = FALSE),
    "Elements of lists in oos_prediction_list in each ml_backtest_results object must be the same."
  )

  #Check for correct winsorization
  expect_equal(convert_oos_predictions_list_to_m_df(sb_backtest_results_list, winsorize_predictions = TRUE, winsorization_probs = c(0.01, 0.99), normalize_predictions = TRUE)@workflow,
               list(c("winsorization with 0.01 0.99 quantiles and following variables with Infs preserved: "), c("normalization")))


})
