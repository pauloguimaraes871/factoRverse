test_that("explain_prediction works for a sb_backtest_results object", {

  #First run a SB backtest
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, "feats_123")
  target_m_df <- create_meta_dataframe(toy_preprocessed_targets, "tg_123")

  chosen_signals_and_positions <- c(asset_turnover_12m = "long", book_yield = "long", dps_yield = "long", eps_yield = "long",
                                    idio_vol_mrkt_ewma = "short", roe_3m = "long", sectors_c1Agro = "long", sectors_c1Indústria = "long",
                                    sharpe_6m = "long")

  rf_config <- create_sb_backtest_config(sb_algorithm = "rf", training_sample_size = 4, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m",
                                         config_name = "rf_101", chosen_signals_and_positions = chosen_signals_and_positions) %>%
    add_tuning_strategy(tuning_method = "random_search", validation_sample_size = 3, n_iter = 2) %>%
    add_hyperparameter(hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
                       distribution_choice = c("uniform", "uniform", "lognormal", "uniform"),
                       pars = list(c(min=0.1, max = 0.9), c(min = 100L, max = 500L), c(meanlog = 1L, sdlog = 1L),
                                   c(min = 1L, max = 10L))
    )

  set.seed(123)
  suppressWarnings(
    rf_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      config = rf_config,
      parallel = FALSE,
      verbose = TRUE
    )
  )

  #Explain prediction
    ##Get pred
    selected_ticker <- "BBDC4"
    selected_date <- as.Date("2023-06-15")
    selected_id <- paste0(selected_ticker,"-",selected_date)
    pred <- rf_results@oos_sb_outputs_m_df@data %>% dplyr::filter(id == selected_id) %>% dplyr::pull(pred)

    ##Get features
    features_df <- features_m_df@data %>% dplyr::filter(id == selected_id) %>% dplyr::select(names(chosen_signals_and_positions))
    ##Transform short position
    features_df$low_idio_vol_mrkt_ewma <- -features_df$idio_vol_mrkt_ewma
    features_df$idio_vol_mrkt_ewma <- NULL
    features_df <- data.frame(features = colnames(features_df), values = as.numeric(features_df))

    ##Get Importance
    importance_df <- rf_results@feature_importance_m_df@data %>% dplyr::filter(dates <= selected_date) %>% dplyr::filter(dates == max(dates)) %>%
      dplyr::select(-id, -dates, -normalized_importance, -is_eligible)

    ##Join
    features_df <- features_df %>% dplyr::left_join(importance_df, by = c("features" = "tickers")) %>%
      dplyr::mutate(imp_times_value = values * importance)

    intercept <- importance_df %>% dplyr::filter(tickers == "(Intercept)") %>% dplyr::pull(importance)

    ##Most and less important positive and neg contributions
    most_imp_pos <- features_df %>% dplyr::filter(features %in% c("book_yield", "low_idio_vol_mrkt_ewma", "roe_3m", "asset_turnover_12m", "sharpe_6m"))
    less_imp_pos <- features_df %>% dplyr::filter(features %in% c("eps_yield"))

    most_imp_neg <- features_df %>% dplyr::filter(features %in% c("dps_yield"))
    less_imp_neg <- features_df %>% dplyr::filter(features %in% c("ababababa"))

    ##Prediction of simple model
    gsm_pred <- features_df$imp_times_value %>% sum() + intercept

    ##Non-linearity
    complexity <- pred - gsm_pred

    ##Get results
    results <- explain_prediction(sb_backtest_results = rf_results, features_m_df = features_m_df, selected_ticker = "BBDC4", selected_date = as.Date("2023-06-15"))

    expect_equal(results$TotalContribution[1], intercept)
    expect_equal(results %>% dplyr::filter(ContributionType == "Most Important Positive") %>% dplyr::pull(TotalContribution) %>% sum(),
                 most_imp_pos$imp_times_value %>% sum())
    expect_equal(results %>% dplyr::filter(ContributionType == "Most Important Positive") %>% dplyr::select(tickers, TotalContribution),
                 most_imp_pos %>% dplyr::select(features, imp_times_value) %>% dplyr::rename(tickers = features, TotalContribution = imp_times_value) %>%
                   dplyr::arrange(desc(TotalContribution))
                 )
    expect_equal(results %>% dplyr::filter(ContributionType == "Less Important Positive") %>% dplyr::select(tickers, TotalContribution) %>% dplyr::pull(TotalContribution),
                 less_imp_pos$imp_times_value
    )
    expect_equal(results %>% dplyr::filter(ContributionType == "Less Important Negative") %>% dplyr::pull(TotalContribution),
                 0
    )
    expect_equal(results %>% dplyr::filter(tickers == "complexity") %>% dplyr::pull(TotalContribution), complexity)

    expect_equal(results$TotalContribution[1:5] %>% sum(), results$Cumulative[5])

})

test_that("explain_prediction works for a meta_sb_backtest_results object", {

  #First run a SB backtest
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, "feats_123")
  target_m_df <- create_meta_dataframe(toy_preprocessed_targets, "tg_123")

  chosen_signals_and_positions <- c(asset_turnover_12m = "long", book_yield = "long", dps_yield = "long", eps_yield = "long",
                                    idio_vol_mrkt_ewma = "short", roe_3m = "long", sectors_c1Agro = "long", sectors_c1Indústria = "long",
                                    sharpe_6m = "long")

  rf_config <- create_sb_backtest_config(sb_algorithm = "rf", training_sample_size = 4, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m",
                                         config_name = "rf_101", chosen_signals_and_positions = chosen_signals_and_positions) %>%
    add_tuning_strategy(tuning_method = "random_search", validation_sample_size = 3, n_iter = 2) %>%
    add_hyperparameter(hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
                       distribution_choice = c("uniform", "uniform", "lognormal", "uniform"),
                       pars = list(c(min=0.1, max = 0.9), c(min = 100L, max = 500L), c(meanlog = 1L, sdlog = 1L),
                                   c(min = 1L, max = 10L))
    )


  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", training_sample_size = 4, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m",
                                             config_name = "glmnet_123", chosen_signals_and_positions = chosen_signals_and_positions) %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))




  set.seed(123)
  suppressWarnings(
    rf_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      config = rf_config,
      parallel = FALSE,
      verbose = TRUE
    )
  )

  suppressWarnings(
    glmnet_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      config = glmnet_config,
      parallel = FALSE,
      verbose = TRUE
    )
  )

  meta_learner_config <- create_sb_backtest_config(sb_algorithm = "glmnet", training_sample_size = 4, target_fwd_name = "fwd_premium_3m",
                                                   rebalancing_months = 6, config_name = "meta") %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))


  meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_configs = list(rf_config, glmnet_config),
                                  features_passthrough = c("asset_turnover_12m", "book_yield", "dps_yield"),
                                  config_name = "meta_rf_glmnet")

  set.seed(123)
  suppressWarnings(
    sb_metabacktest_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      config = meta_config,
      parallel = FALSE,
      verbose = TRUE
    )
  )


  #Explain prediction
  ##Get pred
  selected_ticker <- "BBDC4"
  selected_date <- as.Date("2023-07-15")
  selected_id <- paste0(selected_ticker,"-",selected_date)
  pred <- sb_metabacktest_results@meta_sb_backtest_results@oos_sb_outputs_m_df@data %>% dplyr::filter(id == selected_id) %>% dplyr::pull(pred)

  ##Get features
  base_features_df <- features_m_df@data %>% dplyr::filter(id == selected_id) %>% dplyr::select(names(chosen_signals_and_positions))
  meta_features_df <- sb_metabacktest_results@base_learners_oos_predictions_m_df@data %>% dplyr::filter(id == selected_id) %>% dplyr::select(-id, -tickers, -dates)

  ##Transform short position
  base_features_df$low_idio_vol_mrkt_ewma <- -base_features_df$idio_vol_mrkt_ewma
  base_features_df$idio_vol_mrkt_ewma <- NULL
  base_features_df <- data.frame(features = colnames(base_features_df), values = as.numeric(base_features_df))
  meta_features_df <- data.frame(features = colnames(meta_features_df), values = as.numeric(meta_features_df))

  #Get importance
  glmnet_importance_df <- glmnet_results@feature_importance_m_df@data %>% dplyr::filter(dates <= selected_date) %>% dplyr::filter(dates == max(dates)) %>%
    dplyr::select(-id, -dates, -normalized_importance, -is_eligible)
  rf_importance_df <- rf_results@feature_importance_m_df@data %>% dplyr::filter(dates <= selected_date) %>% dplyr::filter(dates == max(dates)) %>%
    dplyr::select(-id, -dates, -normalized_importance, -is_eligible)
  meta_importance_df <- sb_metabacktest_results@meta_sb_backtest_results@feature_importance_m_df@data %>% dplyr::filter(dates <= selected_date) %>% dplyr::filter(dates == max(dates)) %>%
    dplyr::select(-id, -dates, -normalized_importance, -is_eligible)
  glm_imp <-  meta_importance_df$importance[4]
  rf_imp <- meta_importance_df$importance[5]

  #Get rel Importance
  glmnet_importance_df <- glmnet_importance_df %>% dplyr::mutate(rel_importance = importance/sum(importance), importance = rel_importance * glm_imp)
  rf_importance_df <- rf_importance_df %>% dplyr::mutate(rel_importance = importance/sum(importance), importance = rel_importance * rf_imp)

  #Bind to meta_importance
  meta_importance_df <- meta_importance_df %>% dplyr::bind_rows(glmnet_importance_df) %>% dplyr::bind_rows(rf_importance_df)
  #Exclude base learners
  meta_importance_df <- meta_importance_df %>% dplyr::filter(!tickers %in%
                                                               c("c:glmnet_123_f:feats_123_t:tg_123-fwd_premium_3m","c:rf_101_f:feats_123_t:tg_123-fwd_premium_3m")) %>%
    dplyr::select(-rel_importance)

  meta_importance_df <- meta_importance_df %>% dplyr::group_by(tickers) %>% dplyr::summarize(importance = sum(importance))

  ##Join
  features_df <- base_features_df %>% dplyr::left_join(meta_importance_df, by = c("features" = "tickers")) %>%
    dplyr::mutate(imp_times_value = values * importance) %>% dplyr::arrange(desc(imp_times_value))

  intercept <- meta_importance_df %>% dplyr::filter(tickers == "(Intercept)") %>% dplyr::pull(importance)

  ##Most and less important positive and neg contributions
  most_imp_pos <- features_df %>% dplyr::filter(features %in% c("book_yield", "low_idio_vol_mrkt_ewma", "dps_yield", "asset_turnover_12m", "roe_3m"))
  less_imp_pos <- features_df %>% dplyr::filter(features %in% c("sharpe_6m", "eps_yield"))

  most_imp_neg <- features_df %>% dplyr::filter(features %in% c("abababab"))
  less_imp_neg <- features_df %>% dplyr::filter(features %in% c("ababababa"))

  ##Prediction of simple model
  gsm_pred <- features_df$imp_times_value %>% sum() + intercept

  ##Non-linearity
  complexity <- pred - gsm_pred

  ##Get results
  results <- explain_prediction(sb_backtest_results = sb_metabacktest_results,
                                features_m_df = features_m_df, selected_ticker = "BBDC4", selected_date = as.Date("2023-07-15"))

  expect_equal(results$TotalContribution[1], intercept)
  expect_equal(results %>% dplyr::filter(ContributionType == "Most Important Positive") %>% dplyr::pull(TotalContribution) %>% sum(),
               most_imp_pos$imp_times_value %>% sum())
  expect_equal(results %>% dplyr::filter(ContributionType == "Most Important Positive") %>% dplyr::select(tickers, TotalContribution),
               most_imp_pos %>% dplyr::select(features, imp_times_value) %>% dplyr::rename(tickers = features, TotalContribution = imp_times_value) %>%
                 dplyr::arrange(desc(TotalContribution))
  )
  expect_equal(results %>% dplyr::filter(ContributionType == "Less Important Positive") %>% dplyr::select(tickers, TotalContribution) %>% dplyr::pull(TotalContribution),
               less_imp_pos$imp_times_value %>% sum()
  )
  expect_equal(results %>% dplyr::filter(ContributionType == "Less Important Negative") %>% dplyr::pull(TotalContribution),
               0
  )
  expect_equal(results %>% dplyr::filter(tickers == "complexity") %>% dplyr::pull(TotalContribution), complexity)

  expect_equal(results$TotalContribution[1:5] %>% sum(), results$Cumulative[5])

})

test_that("decomposition works for a meta_sb_backtest_results object - toy_data", {

  #Create data
  meta_m_df <- data.frame(
    id = c("(Intercept)-2023-07-15", "glmnet_m_df-2023-07-15", "rf_m_df"),
    tickers = c("(Intercept)", "glmnet_m_df", "rf_m_df"),
    dates = as.Date("2023-07-15"),
    importance = c(-0.279, 1.050, 0.15)
    )

  rf_m_df <- data.frame(
    id = c("(Intercept)-2023-07-15", "asset_turnover_12m-2023-07-15", "book_yield_12m-2023-07-15", "dps_yield_12m-2023-07-15", "eps_yield_12m-2023-07-15",
           "fcf_yield-2023-07-15", "low_idio_vol_mrkt_ewma-2023-07-15", "mom_res_12m-2023-07-15", "roe_3m-2023-07-15"),
    tickers = c("(Intercept)", "asset_turnover_12m", "book_yield_12m", "dps_yield_12m", "eps_yield_12m",
                "fcf_yield", "low_idio_vol_mrkt_ewma", "mom_res_12m", "roe_3m"),
    dates = as.Date("2023-07-15"),
    importance = c(-0.16, 0.10, 0.20, 0.25, 0.05, 0.12, 0.02, 0.20, 0.01)
  )

  glmnet_m_df <- data.frame(
    id = c("(Intercept)-2023-07-15", "asset_turnover_12m-2023-07-15", "book_yield_12m-2023-07-15", "dps_yield_12m-2023-07-15", "eps_yield_12m-2023-07-15",
           "fcf_yield-2023-07-15", "low_idio_vol_mrkt_ewma-2023-07-15", "mom_res_12m-2023-07-15", "roe_3m-2023-07-15"),
    tickers = c("(Intercept)", "asset_turnover_12m", "book_yield_12m", "dps_yield_12m", "eps_yield_12m",
                "fcf_yield", "low_idio_vol_mrkt_ewma", "mom_res_12m", "roe_3m"),
    dates = as.Date("2023-07-15"),
    importance = c(-0.25, 0.05, 0.50, 0.01, 0.00, 0.20, 0.02, 0.05, 0.01)
  )

  #Run
  results <- decompose_feature_importance(meta_feature_importance = meta_m_df,
                                          base_identifiers = c("rf_m_df", "glmnet_m_df"),
                                          base_feature_importance_filtered = list(rf_m_df, glmnet_m_df),
                                          most_recent_meta_date = as.Date("2023-07-15")
                                          )

  #Expectation
  expect_equal(results$importance[1], -0.751, tolerance = 0.1)
  expect_equal(results$importance[2], 0.107, tolerance = 0.1)
  expect_equal(results$importance[3], 0.919, tolerance = 0.1)
  expect_equal(results$importance[4], 0.067, tolerance = 0.1)
  expect_equal(results$importance[5], 0.012, tolerance = 0.1)
  expect_equal(results$importance[6], 0.375, tolerance = 0.1)
  expect_equal(results$importance[7], 0.040, tolerance = 0.1)
  expect_equal(results$importance[8], 0.126, tolerance = 0.1)
  expect_equal(results$importance[9], 0.024, tolerance = 0.1)

})
