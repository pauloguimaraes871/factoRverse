test_that("create_heuristic_ensembles works for EW, rmse, huber_delta = 1", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  ols_config <- create_ml_backtest_config(ml_algorithm = "ols", custom_objective = "squared_error",
                                          training_sample_size = 9, rebalancing_months = 6)

  glmnet_config <- create_ml_backtest_config(ml_algorithm = "glmnet", training_sample_size = 6, rebalancing_months = 6) %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))

  rf_config <- create_ml_backtest_config(ml_algorithm = "rf", training_sample_size = 6, rebalancing_months = 6) %>%
    add_tuning_strategy(tuning_method = "random_search", validation_sample_size = 3, n_iter = 2) %>%
    add_hyperparameter(hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
                       distribution_choice = c("uniform", "uniform", "lognormal", "uniform"),
                       pars = list(c(min=0.1, max = 0.9), c(min = 100L, max = 500L), c(meanlog = 1L, sdlog = 1L),
                                   c(min = 1L, max = 10L))
    )


  set.seed(123)
  ols_results <- run_ml_backtest(
    features_m_df = create_meta_dataframe(toy_preprocessed_features),
    target_m_df = create_meta_dataframe(toy_preprocessed_targets),
    target_fwd_name = "fwd_premium_3m",
    config = ols_config,
    verbose = TRUE,
    parallel = FALSE
  )

  glmnet_results <- run_ml_backtest(
    features_m_df = create_meta_dataframe(toy_preprocessed_features),
    target_m_df = create_meta_dataframe(toy_preprocessed_targets),
    target_fwd_name = "fwd_premium_3m",
    config = glmnet_config,
    verbose = TRUE,
    parallel = FALSE
  )

  future::plan("multisession")
  rf_results <- run_ml_backtest(
    features_m_df = create_meta_dataframe(toy_preprocessed_features),
    target_m_df = create_meta_dataframe(toy_preprocessed_targets),
    target_fwd_name = "fwd_premium_3m",
    config = rf_config,
    verbose = TRUE,
    parallel = TRUE
  )


  #Get preds
  ols_predictions <- ols_results@oos_prediction_list
  glmnet_predictions <- glmnet_results@oos_prediction_list
  rf_predictions <- rf_results@oos_prediction_list
  all_predictions <- list(ols_predictions, glmnet_predictions, rf_predictions)
  names(all_predictions) <- c("ols_config", "glmnet_config", "rf_config")
  dates <- names(ols_predictions)

  #EW
  ew_weights <- rep(1/3, 3)
  names(ew_weights) <- c("ols_config", "glmnet_config", "rf_config")

  # Multiply each prediction list by its corresponding weight
  weighted_predictions  <- purrr::map2(all_predictions, ew_weights, function(pred_list, weight){
    purrr::map(pred_list, function(pred_vector){
      pred_vector * weight
    })
  })
  # Sum the weighted predictions across all models for each date
  ew_ensemble_preds <- purrr::reduce(weighted_predictions, function(acc, current) {
    purrr::map2(acc, current, function(acc_pred, current_pred) {
      acc_pred + current_pred
    })
  })

  #Errors and eval metrics
  ew_ensemble_errors <- list()
  ew_ensemble_eval_metrics <- list()
  for(l in 1:length(ew_ensemble_preds)){
    ew_ensemble_errors[[l]] <-
      calculate_eval_metrics(ew_ensemble_preds[[l]], ols_results@oos_y_list[[l]], "rmse", huber_delta = 1, quantile_tau = 0.5, return_error = TRUE)$error

    ew_ensemble_eval_metrics[[l]] <-
      calculate_eval_metrics(ew_ensemble_preds[[l]], ols_results@oos_y_list[[l]], "rmse", huber_delta = 1, quantile_tau = 0.5, return_error = TRUE)$df_eval_metrics[-1]
  }
  names(ew_ensemble_errors) <- names(ew_ensemble_preds)
  names(ew_ensemble_eval_metrics) <- names(ew_ensemble_preds)

  #Create eval_df
  ew_ensemble_eval_df <- purrr::map_dfr(ew_ensemble_eval_metrics, ~ .x, .id = "date")
  ew_ensemble_eval_df <- tibble::column_to_rownames(ew_ensemble_eval_df, var = "date")
  consolidade_row_ew_ensemble_eval_df <- calculate_eval_metrics(pred = unlist(ew_ensemble_preds), target = unlist(ols_results@oos_y_list),
                                                                "rmse", huber_delta = 1, quantile_tau = 0.5, return_error = FALSE)[-1]

  ew_ensemble_eval_df <- rbind(ew_ensemble_eval_df, consolidade_row_ew_ensemble_eval_df)
  rownames(ew_ensemble_eval_df)[6] <- "consolidated"

  actual_result <- create_heuristic_ensembles(list(ols_config = ols_results, glmnet_config = glmnet_results, rf_config = rf_results), "rmse")

  expect_equal(actual_result$ew_ensemble_config@oos_prediction_list, ew_ensemble_preds)
  expect_equal(actual_result$ew_ensemble_config@oos_error_list, ew_ensemble_errors)
  expect_equal(actual_result$ew_ensemble_config@oos_y_list, ols_results@oos_y_list)
  expect_equal(actual_result$ew_ensemble_config@oos_testing_eval_metrics, ew_ensemble_eval_df)

  expected_may <- rowMeans(cbind(ols_results@oos_prediction_list$`2023-05-15`,
                                     glmnet_results@oos_prediction_list$`2023-05-15`,
                                     rf_results@oos_prediction_list$`2023-05-15`))


  actual_may <- actual_result$ew_ensemble_config@oos_prediction_list$`2023-05-15`

  expect_equal(actual_may,expected_may)


  future::plan("sequential")

})





test_that("create_heuristic_ensembles works for Optimal, rss, huber_delta = 1.25, quantile_tau = 0.25", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  ols_config <- create_ml_backtest_config(ml_algorithm = "ols", custom_objective = "squared_error",
                                          training_sample_size = 9, rebalancing_months = 6)

  glmnet_config <- create_ml_backtest_config(ml_algorithm = "glmnet", training_sample_size = 6, rebalancing_months = 6) %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))

  rf_config <- create_ml_backtest_config(ml_algorithm = "rf", training_sample_size = 6, rebalancing_months = 6) %>%
    add_tuning_strategy(tuning_method = "random_search", validation_sample_size = 3, n_iter = 2) %>%
    add_hyperparameter(hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
                       distribution_choice = c("uniform", "uniform", "lognormal", "uniform"),
                       pars = list(c(min=0.1, max = 0.9), c(min = 100L, max = 500L), c(meanlog = 1L, sdlog = 1L),
                                   c(min = 1L, max = 10L))
    )


  set.seed(123)
  ols_results <- run_ml_backtest(
    features_m_df = create_meta_dataframe(toy_preprocessed_features),
    target_m_df = create_meta_dataframe(toy_preprocessed_targets),
    target_fwd_name = "fwd_premium_3m",
    config = ols_config,
    verbose = TRUE,
    parallel = FALSE
  )

  glmnet_results <- run_ml_backtest(
    features_m_df = create_meta_dataframe(toy_preprocessed_features),
    target_m_df = create_meta_dataframe(toy_preprocessed_targets),
    target_fwd_name = "fwd_premium_3m",
    config = glmnet_config,
    verbose = TRUE,
    parallel = FALSE
  )

  future::plan("multisession")
  rf_results <- run_ml_backtest(
    features_m_df = create_meta_dataframe(toy_preprocessed_features),
    target_m_df = create_meta_dataframe(toy_preprocessed_targets),
    target_fwd_name = "fwd_premium_3m",
    config = rf_config,
    verbose = TRUE,
    parallel = TRUE
  )


  #Get preds
  ols_predictions <- ols_results@oos_prediction_list
  glmnet_predictions <- glmnet_results@oos_prediction_list
  rf_predictions <- rf_results@oos_prediction_list
  all_predictions <- list(list(ols_predictions$`2023-06-15`, ols_predictions$`2023-07-15`),
                          list(glmnet_predictions$`2023-06-15`, glmnet_predictions$`2023-07-15`),
                          list(rf_predictions$`2023-06-15`, rf_predictions$`2023-07-15`))

  opt_oos_y_list <- ols_results@oos_y_list[c("2023-06-15", "2023-07-15")]


  names(all_predictions) <- c("ols_config", "glmnet_config", "rf_config")

  names(all_predictions$ols_config) <- c("2023-06-15", "2023-07-15")
  names(all_predictions$glmnet_config) <- c("2023-06-15", "2023-07-15")
  names(all_predictions$rf_config) <- c("2023-06-15", "2023-07-15")


  #Optimal
  optimal_weights_1 <- c(ols_results@oos_testing_eval_metrics$rss[1],
                         glmnet_results@oos_testing_eval_metrics$rss[1],
                         rf_results@oos_testing_eval_metrics$rss[1])

  optimal_weights_1 <- optimal_weights_1/sum(optimal_weights_1)
  optimal_weights_2 <- optimal_weights_1

  optimal_weights <- list(optimal_weights_1, optimal_weights_2)
  names(optimal_weights) <- as.Date(c("2023-06-15", "2023-07-15"))


  # Multiply each prediction list by its corresponding weight
  opt_ensemble_preds  <- list(
    apply(cbind(all_predictions$ols_config$`2023-06-15`, all_predictions$glmnet_config$`2023-06-15`, all_predictions$rf_config$`2023-06-15`), 1,
          function(x) sum(x*optimal_weights_1)),
    apply(cbind(all_predictions$ols_config$`2023-07-15`, all_predictions$glmnet_config$`2023-07-15`, all_predictions$rf_config$`2023-07-15`), 1,
          function(x) sum(x*optimal_weights_2)
    ))


  names(opt_ensemble_preds) <- as.Date(c("2023-06-15", "2023-07-15"))

  #Errors and eval metrics
  opt_ensemble_errors <- list()
  opt_ensemble_eval_metrics <- list()
  for(l in 1:length(opt_ensemble_preds)){
    opt_ensemble_errors[[l]] <-
      calculate_eval_metrics(opt_ensemble_preds[[l]], opt_oos_y_list[[l]], "rss", huber_delta = 1.25, quantile_tau = 0.25, return_error = TRUE)$error

    opt_ensemble_eval_metrics[[l]] <-
      calculate_eval_metrics(opt_ensemble_preds[[l]], opt_oos_y_list[[l]], "rss", huber_delta = 1.25, quantile_tau = 0.25, return_error = TRUE)$df_eval_metrics[-1]
  }
  names(opt_ensemble_errors) <- names(opt_ensemble_preds)
  names(opt_ensemble_eval_metrics) <- names(opt_ensemble_preds)

  #Create eval_df
  opt_ensemble_eval_df <- purrr::map_dfr(opt_ensemble_eval_metrics, ~ .x, .id = "date")
  opt_ensemble_eval_df <- tibble::column_to_rownames(opt_ensemble_eval_df, var = "date")
  consolidade_row_opt_ensemble_eval_df <- calculate_eval_metrics(pred = unlist(opt_ensemble_preds), target = unlist(opt_oos_y_list),
                                                                "rss", huber_delta = 1.25, quantile_tau = 0.25, return_error = FALSE)[-1]

  opt_ensemble_eval_df <- rbind(opt_ensemble_eval_df, consolidade_row_opt_ensemble_eval_df)
  rownames(opt_ensemble_eval_df)[3] <- "consolidated"

  actual_result <- create_heuristic_ensembles(list(ols_config = ols_results, glmnet_config = glmnet_results, rf_config = rf_results), "rss",
                                              ensemble_huber_delta = 1.25, ensemble_quantile_tau = 0.25)

  names(optimal_weights_2) <- c("ols_config", "glmnet_config", "rf_config")
  expect_equal(actual_result$optimal_ensemble_config@final_model@model, optimal_weights_2)
  expect_equal(actual_result$optimal_ensemble_config@oos_prediction_list, opt_ensemble_preds)
  expect_equal(actual_result$optimal_ensemble_config@oos_error_list, opt_ensemble_errors)
  expect_equal(actual_result$optimal_ensemble_config@oos_y_list, ols_results@oos_y_list[c("2023-06-15", "2023-07-15")])
  expect_equal(actual_result$optimal_ensemble_config@oos_testing_eval_metrics, opt_ensemble_eval_df)
  expect_lt(actual_result$optimal_ensemble_config@final_model@model[1],
            actual_result$optimal_ensemble_config@final_model@model[2])
  expect_lt(actual_result$optimal_ensemble_config@final_model@model[1],
            actual_result$optimal_ensemble_config@final_model@model[3])

  future::plan("sequential")

})
