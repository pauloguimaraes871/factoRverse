test_that("convert_oos_predictions_lists_to_m_df returns a meta_dataframe with expected format for features_pass = none", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, type = "signals", meta_dataframe_name = "feat123")
  target_m_df <- create_meta_dataframe(toy_preprocessed_targets, type = "target", meta_dataframe_name = "target123")

  ols_config <- create_sb_backtest_config(sb_algorithm = "ols", custom_objective = "squared_error", target_fwd_name = "fwd_premium_3m",
                                          training_sample_size = 9, rebalancing_months = 6, config_name = "ols1")

  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", training_sample_size = 6, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m", config_name = "glm1") %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))

  rf_config <- create_sb_backtest_config(sb_algorithm = "rf", training_sample_size = 6, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m", config_name = "rf1") %>%
    add_tuning_strategy(tuning_method = "random_search", validation_sample_size = 3, n_iter = 2) %>%
    add_hyperparameter(hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
                       distribution_choice = c("uniform", "uniform", "lognormal", "uniform"),
                       pars = list(c(min=0.1, max = 0.9), c(min = 100L, max = 500L), c(meanlog = 1L, sdlog = 1L),
                                   c(min = 1L, max = 10L))
    )


  set.seed(123)
  ols_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = ols_config,
    verbose = TRUE,
    parallel = FALSE
  )

  glmnet_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = glmnet_config,
    verbose = TRUE,
    parallel = FALSE
  )

  rf_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = rf_config,
    verbose = TRUE,
    parallel = FALSE
  )

  #Get sb_backtest_results
  sb_backtest_results_list <- list(ols_config = ols_results, glmnet_config = glmnet_results, rf_config = rf_results)

  predictions_m_df <- consolidate_oos_sb_outputs_m_df(sb_backtest_results_list, winsorize_predictions = FALSE, normalize_predictions = FALSE)

  #Expect class to be meta_dataframe
  expect_equal(as.character(class(predictions_m_df)), "meta_dataframe")
  #Expect ncol to be right
  expect_equal(ncol(predictions_m_df@data), 3 + length(sb_backtest_results_list)) #id, tickers, dates = 3
  #Expect number of stocks to be equal
  expect_equal(predictions_m_df@unique_tickers, ols_results@oos_sb_outputs_m_df@unique_tickers)
  #Expect number of months to be equal
  expect_equal(predictions_m_df@unique_dates, ols_results@oos_sb_outputs_m_df@unique_dates)
  #Expect columns to be backtest names
  expect_equal(names(predictions_m_df@data), c("id", "tickers", "dates", unname(sapply(sb_backtest_results_list, function(x) x@backtest_identifier))))
  #Expect predictions to be correctly set
  expect_equal(predictions_m_df@data %>% dplyr::pull(`c:ols1_f:feat123_t:target123-fwd_premium_3m`),
               ols_results@oos_sb_outputs_m_df@data %>% dplyr::pull(pred))

  expect_equal(predictions_m_df@data %>% dplyr::pull(`c:glm1_f:feat123_t:target123-fwd_premium_3m`),
               glmnet_results@oos_sb_outputs_m_df@data %>% dplyr::pull(pred))

  expect_equal(predictions_m_df@data %>% dplyr::pull(`c:rf1_f:feat123_t:target123-fwd_premium_3m`),
               rf_results@oos_sb_outputs_m_df@data %>% dplyr::pull(pred))

  #Expect ids to be the same
  expect_equal(predictions_m_df@data$id, ols_results@oos_sb_outputs_m_df@data$id)
  expect_equal(predictions_m_df@data$id, glmnet_results@oos_sb_outputs_m_df@data$id)
  expect_equal(predictions_m_df@data$id, rf_results@oos_sb_outputs_m_df@data$id)


})


test_that("convert_oos_predictions_lists_to_m_df returns a meta_dataframe with expected format for features_pass = 'all'", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, type = "signals", meta_dataframe_name = "feat123")
  target_m_df <- create_meta_dataframe(toy_preprocessed_targets, type = "target", meta_dataframe_name = "target123")

  ols_config <- create_sb_backtest_config(sb_algorithm = "ols", custom_objective = "squared_error", target_fwd_name = "fwd_premium_3m",
                                          training_sample_size = 9, rebalancing_months = 6, config_name = "ols1")

  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", training_sample_size = 6, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m", config_name = "glm1") %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))

  rf_config <- create_sb_backtest_config(sb_algorithm = "rf", training_sample_size = 6, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m", config_name = "rf1") %>%
    add_tuning_strategy(tuning_method = "random_search", validation_sample_size = 3, n_iter = 2) %>%
    add_hyperparameter(hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
                       distribution_choice = c("uniform", "uniform", "lognormal", "uniform"),
                       pars = list(c(min=0.1, max = 0.9), c(min = 100L, max = 500L), c(meanlog = 1L, sdlog = 1L),
                                   c(min = 1L, max = 10L))
    )

  set.seed(123)
  ols_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = ols_config,
    verbose = TRUE,
    parallel = FALSE
  )

  glmnet_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = glmnet_config,
    verbose = TRUE,
    parallel = FALSE
  )

  rf_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = rf_config,
    verbose = TRUE,
    parallel = FALSE
  )

  #Get sb_backtest_results
  sb_backtest_results_list <- list(ols_config = ols_results, glmnet_config = glmnet_results, rf_config = rf_results)

  #features_passthrough_and_positions
  features_passthrough <- c("asset_turnover_12m", "book_yield", "idio_vol_mrkt_ewma", "sharpe_6m", "sectors_c1Agro")
  features_passthrough_and_positions <-



  predictions_m_df <- consolidate_oos_sb_outputs_m_df(sb_backtest_results_list, winsorize_predictions = FALSE, normalize_predictions = FALSE,
                                                      features_passthrough_and_positions = features_passthrough_and_positions, features_m_df = features_m_df)

  #Expect class to be meta_dataframe
  expect_equal(as.character(class(predictions_m_df)), "meta_dataframe")
  #Expect ncol to be right
  expect_equal(ncol(predictions_m_df@data), 3 + length(sb_backtest_results_list)) #id, tickers, dates = 3
  #Expect number of stocks to be equal
  expect_equal(predictions_m_df@unique_tickers, ols_results@oos_sb_outputs_m_df@unique_tickers)
  #Expect number of months to be equal
  expect_equal(predictions_m_df@unique_dates, ols_results@oos_sb_outputs_m_df@unique_dates)
  #Expect columns to be backtest names
  expect_equal(names(predictions_m_df@data), c("id", "tickers", "dates", unname(sapply(sb_backtest_results_list, function(x) x@backtest_identifier))))
  #Expect predictions to be correctly set
  expect_equal(predictions_m_df@data %>% dplyr::pull(`c:ols1_f:feat123_t:target123-fwd_premium_3m`),
               ols_results@oos_sb_outputs_m_df@data %>% dplyr::pull(pred))

  expect_equal(predictions_m_df@data %>% dplyr::pull(`c:glm1_f:feat123_t:target123-fwd_premium_3m`),
               glmnet_results@oos_sb_outputs_m_df@data %>% dplyr::pull(pred))

  expect_equal(predictions_m_df@data %>% dplyr::pull(`c:rf1_f:feat123_t:target123-fwd_premium_3m`),
               rf_results@oos_sb_outputs_m_df@data %>% dplyr::pull(pred))

  #Expect ids to be the same
  expect_equal(predictions_m_df@data$id, ols_results@oos_sb_outputs_m_df@data$id)
  expect_equal(predictions_m_df@data$id, glmnet_results@oos_sb_outputs_m_df@data$id)
  expect_equal(predictions_m_df@data$id, rf_results@oos_sb_outputs_m_df@data$id)


})























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
