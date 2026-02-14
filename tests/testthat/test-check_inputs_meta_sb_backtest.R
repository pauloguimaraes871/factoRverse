test_that("checks related to features_passthrough", {

  #Load objects
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #First apply a signal selection backtest
  set.seed(123)
  #Backtest Returns
  mocked_backtest_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    asset_turnover_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 5, sd = 3.5),
    book_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1, sd = 5),
    dps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 15, sd = 0.4),
    eps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.0005, sd = 0.3),
    mom_res_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 3.15, sd = 3.5),
    roe_3m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.1, sd = 2),
    sharpe_6m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 2.5, sd = 5),
    low_idio_vol_mrkt_ewma = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.05, sd = 7.5)
  ), order.by = unique(toy_preprocessed_features$dates)), meta_xts_name = "backtest")

  #Benchmark Returns xts
  suppressWarnings(
    mocked_benchmark_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
      IBOV = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.01, sd = 0.035),
      SMLL = rnorm(length(unique(toy_preprocessed_features$dates)), mean = -0.01, sd = 0.025)
    ),  order.by = unique(toy_preprocessed_features$dates)), meta_xts_name = "benchmark")
  )

  #Chosen Signals and Positions
  chosen_signals_and_positions <- c(asset_turnover_12m = "long", book_yield = "long", dps_yield = "long", eps_yield = "long",
                                    idio_vol_mrkt_ewma = "short", sharpe_6m = "long")

  #Mocked Signal Themes
  mocked_signal_themes_m_df <- expand.grid(
    tickers = names(mocked_backtest_returns_m_xts@data),
    dates = unique(toy_preprocessed_features$dates),
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(id = paste0(tickers,"-",dates),
                      theme = dplyr::case_when(
                        tickers %in% c("mom_res_12m", "sharpe_6m") ~ "momentum",
                        tickers %in% c("dy_med_36m", "eps_yield", "book_yield", "asset_turnover_12m", "dps_yield") ~ "value",
                        tickers %in% c("roe_3m", "low_idio_vol_mrkt_ewma") ~ "defensive"
                      )
  ) %>%  dplyr::arrange(id) %>% dplyr::select(id, tickers, dates, theme)

  signal_themes_m_df <- create_meta_dataframe(mocked_signal_themes_m_df, "st_11", type = "groups")

  ##SS Config 1
  frequentist_ss_config <- create_ss_backtest_config(initial_sample_size = 3, rebalancing_months = 6,
                                                     split_method = "expanding", config_name = "frequentist_ss", active_returns = TRUE,
                                                     chosen_signals_and_positions = chosen_signals_and_positions
  ) %>%
    add_alpha_test_strategy(model_structure = "no_pooled",
                            signal_significance_threshold = 0.15, p_correction_method = "none",
                            market_factor_proxy = "IBOV", enable_theme_representativeness = TRUE)

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, "feats_123")
  target_m_df <- create_meta_dataframe(toy_preprocessed_targets, "tg_123")

  #SB Configs
  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", training_sample_size = 4, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m",
                                             config_name = "glmnet_123") %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))

  rf_config <- create_sb_backtest_config(sb_algorithm = "rf", training_sample_size = 4, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m",
                                         config_name = "rf_101") %>%
    add_tuning_strategy(tuning_method = "random_search", validation_sample_size = 3, n_iter = 2) %>%
    add_hyperparameter(hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
                       distribution_choice = c("uniform", "uniform", "lognormal", "uniform"),
                       pars = list(c(min=0.1, max = 0.9), c(min = 100L, max = 500L), c(meanlog = 1L, sdlog = 1L),
                                   c(min = 1L, max = 10L))
    )

  suppressWarnings(
  frequentist_ss_results <- run_ss_backtest(
    signals_m_df = features_m_df,
    config = frequentist_ss_config,
    backtest_returns_m_xts = mocked_backtest_returns_m_xts,
    benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
    signal_themes_m_df = signal_themes_m_df
    )
  )

  suppressWarnings(
    glmnet_results <- run_sb_backtest(
      config = glmnet_config,
      features_m_df = features_m_df,
      ss_backtest_results = frequentist_ss_results,
      target_m_df = target_m_df
    )
  )

  set.seed(123)
  suppressWarnings(
    rf_results <- run_sb_backtest(
      config = rf_config,
      features_m_df = features_m_df,
      ss_backtest_results = frequentist_ss_results,
      target_m_df = target_m_df
    )
  )


  #Meta configs
  meta_learner_config <- create_sb_backtest_config(sb_algorithm = "glmnet", training_sample_size = 4, target_fwd_name = "fwd_premium_3m",
                                                   chosen_signals_and_positions = "all",
                                                   rebalancing_months = 6, config_name = "meta") %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))


  meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  features_passthrough = c("roe_3m", "sharpe_6m", "not_contained"),
                                  config_name = "meta_rf_glmnet")


  #Features passthrough contained
  ##################################
  expect_error(
    check_inputs_meta_sb_backtest(config = meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(rf_results, glmnet_results),
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
  ), "features_passthrough should be contained in features_m_df."
  )

  #Features passthrough is 'none' when using custom_obj
  ##################################
  expect_message(
  meta_learner_config <- create_sb_backtest_config(sb_algorithm = "sw", custom_objective = "max_hr", training_sample_size = 4, target_fwd_name = "fwd_premium_3m",
                                                   chosen_signals_and_positions = "all",
                                                   rebalancing_months = 6, config_name = "meta"),
  "This custom_objective is valid only for meta sb backtests. Please be sure that the current configuration is for such a backtest.")

  meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  features_passthrough = c("roe_3m", "sharpe_6m"),
                                  config_name = "meta_rf_glmnet")

  expect_error(
    check_inputs_meta_sb_backtest(config = meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(rf_results, glmnet_results),
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "features_passthrough should be 'none' when using custom_objective from oos_testing_eval_metrics."
  )

})

test_that("checks related to base_backtest_results work", {

  #Load objects
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #First apply a signal selection backtest
  set.seed(123)
  #Backtest Returns
  mocked_backtest_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    asset_turnover_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 5, sd = 3.5),
    book_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1, sd = 5),
    dps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 15, sd = 0.4),
    eps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.0005, sd = 0.3),
    mom_res_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 3.15, sd = 3.5),
    roe_3m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.1, sd = 2),
    sharpe_6m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 2.5, sd = 5),
    low_idio_vol_mrkt_ewma = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.05, sd = 7.5)
  ), order.by = unique(toy_preprocessed_features$dates)), meta_xts_name = "backtest")

  #Benchmark Returns xts
  suppressWarnings(
    mocked_benchmark_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
      IBOV = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.01, sd = 0.035),
      SMLL = rnorm(length(unique(toy_preprocessed_features$dates)), mean = -0.01, sd = 0.025)
    ),  order.by = unique(toy_preprocessed_features$dates)), meta_xts_name = "benchmark")
  )

  #Chosen Signals and Positions
  chosen_signals_and_positions <- c(asset_turnover_12m = "long", book_yield = "long", dps_yield = "long", eps_yield = "long",
                                    idio_vol_mrkt_ewma = "short", sharpe_6m = "long")

  #Mocked Signal Themes
  mocked_signal_themes_m_df <- expand.grid(
    tickers = names(mocked_backtest_returns_m_xts@data),
    dates = unique(toy_preprocessed_features$dates),
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(id = paste0(tickers,"-",dates),
                      theme = dplyr::case_when(
                        tickers %in% c("mom_res_12m", "sharpe_6m") ~ "momentum",
                        tickers %in% c("dy_med_36m", "eps_yield", "book_yield", "asset_turnover_12m", "dps_yield") ~ "value",
                        tickers %in% c("roe_3m", "low_idio_vol_mrkt_ewma") ~ "defensive"
                      )
  ) %>%  dplyr::arrange(id) %>% dplyr::select(id, tickers, dates, theme)

  signal_themes_m_df <- create_meta_dataframe(mocked_signal_themes_m_df, "st_11", type = "groups")

  ##SS Config 1
  frequentist_ss_config <- create_ss_backtest_config(initial_sample_size = 3, rebalancing_months = 6,
                                                     split_method = "expanding", config_name = "frequentist_ss", active_returns = TRUE,
                                                     chosen_signals_and_positions = chosen_signals_and_positions
  ) %>%
    add_alpha_test_strategy(model_structure = "no_pooled",
                            signal_significance_threshold = 0.15, p_correction_method = "none",
                            market_factor_proxy = "IBOV", enable_theme_representativeness = TRUE)

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, "feats_123")
  target_m_df <- create_meta_dataframe(toy_preprocessed_targets, "tg_123")

  ss_results <- suppressWarnings( #This is for NA warning of NAs at the end of run_ss_backtest
    run_ss_backtest(frequentist_ss_config,
                    signals_m_df = features_m_df, backtest_returns_m_xts = mocked_backtest_returns_m_xts, benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                    signal_themes_m_df = signal_themes_m_df,
                    verbose = TRUE
    )
  )

  #SB Configs
  ew_config <- create_sb_backtest_config(sb_algorithm = "ew", training_sample_size = 4, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m",
                                         config_name = "ew_123")

  rp_config <- create_sb_backtest_config(sb_algorithm = "rp", training_sample_size = 4, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m",
                                         config_name = "rp_101") %>%
    add_cov_est_method(cov_matrix_sample_size = 3, cov_matrix_benchmark = "IBOV")

  mvo_config <- create_sb_backtest_config(sb_algorithm = "mvo", custom_objective = "max_info_ratio", training_sample_size = 4, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m",
                                          config_name = "mvo_101")%>%
    add_cov_est_method(cov_matrix_sample_size = 3, cov_matrix_benchmark = "IBOV")


  #SB Results
  set.seed(123)
  suppressWarnings(
    rp_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      ss_backtest_results = ss_results,
      config = rp_config,
      backtest_returns_m_xts = mocked_backtest_returns_m_xts,
      signal_themes_m_df = signal_themes_m_df,
      benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
      parallel = FALSE,
      verbose = TRUE
    )
  )

  suppressWarnings(
    ew_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      ss_backtest_results = ss_results,
      config = ew_config,
      parallel = FALSE,
      verbose = TRUE
    )
  )

  set.seed(123)
  suppressWarnings(
    mvo_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      ss_backtest_results = ss_results,
      config = mvo_config,
      backtest_returns_m_xts = mocked_backtest_returns_m_xts,
      signal_themes_m_df = signal_themes_m_df,
      benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
      parallel = FALSE,
      verbose = TRUE
    )
  )

  #Meta configs
  meta_learner_config <- create_sb_backtest_config(sb_algorithm = "ew", training_sample_size = 4, target_fwd_name = "fwd_premium_3m",
                                                   chosen_signals_and_positions = "all",
                                                   rebalancing_months = 6, config_name = "meta")



  #Repeated objects
  #####################
  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  features_passthrough = c("roe_3m", "sharpe_6m"),
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(rp_results, rp_results, mvo_results),
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "Base sb backtest identifiers must have unique names."
  )


  #Wrong objs at SB level
  #####################

  #Different features_m_df
  #####################
  wrong_ew_results <- ew_results
  wrong_ew_results@sb_backtest_workflow$`2023-07-15`$features_object_name <- "wrong_name"

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  features_passthrough = "all",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(rp_results, wrong_ew_results, mvo_results),
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "features_m_df object is not the same in every base SB base backtest results and/or with the features_m_df being currently supplied."
  )

  #Different target_m_df
  #####################
  wrong_ew_results <- ew_results
  wrong_ew_results@sb_backtest_workflow$`2023-07-15`$target_object_name <- "wrong_name"

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  features_passthrough = "all",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(rp_results, wrong_ew_results, mvo_results),
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "target_m_df object is not the same in every base SB base backtest results and/or with the target_m_df being currently supplied."
  )


  #Different backtest_returns
  ############################
  wrong_rp_results <- rp_results
  wrong_rp_results@sb_backtest_workflow$`2023-07-15`$backtest_returns_object_name <- "wrong_name"

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  features_passthrough = "all",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(wrong_rp_results, ew_results, mvo_results),
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "backtest_returns_object_name objects are not the same among the filtered backtest results."
  )


  ##Expect no error for EW, which does not require a backtest_returns object
  right_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  features_passthrough = c("all"),
                                  config_name = "meta_rf_ew")

  expect_no_error(
    check_inputs_meta_sb_backtest(config = right_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(ew_results, mvo_results),
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    )
  )

  #Different signal_themes
  ############################
  wrong_rp_results <- rp_results
  wrong_rp_results@sb_backtest_workflow[[length(wrong_rp_results@sb_backtest_workflow)]]$signal_themes_object_name <- "wrong_name"

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  features_passthrough = "all",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(wrong_rp_results, ew_results, mvo_results),
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "signal_themes_object_name objects are not the same among the filtered backtest results."
  )

  #Different training_sample_size
  ############################
  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", training_sample_size = 4, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m",
                                             config_name = "glmnet_123") %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))

  suppressWarnings(
    wrong_glmnet_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      ss_backtest_results = ss_results,
      config = glmnet_config,
      parallel = FALSE,
      verbose = TRUE
    )
  )

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  features_passthrough = "all",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(rp_results, wrong_glmnet_results, mvo_results),
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "training_sample_size plus validation_sample_size is not the same in every base SB backtest results."
  )


  #Different rebalancing_month
  ############################
  #ew_config <- create_sb_backtest_config(sb_algorithm = "ew", training_sample_size = 4, rebalancing_months = 8, target_fwd_name = "fwd_premium_3m",
  #                                       config_name = "ew_123")

  #suppressWarnings(
  #  wrong_ew_results <- run_sb_backtest(
  #    target_m_df = target_m_df,
  #    features_m_df = features_m_df,
  #    ss_backtest_results = ss_results,
  #    config = ew_config,
  #    parallel = FALSE,
  #    verbose = TRUE
  #  )
  #)

  #wrong_meta_config <-
  #  create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
  #                                features_passthrough = "all",
  #                                config_name = "meta_rf_ew")

  #expect_error(
  #  check_inputs_meta_sb_backtest(config = wrong_meta_config,
  #                                features_m_df = features_m_df, target_m_df = target_m_df,
  #                                base_sb_backtest_results_list = list(rp_results, wrong_ew_results, mvo_results),
  #                                base_signal_themes_m_df = signal_themes_m_df,
  #                                base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
  #                                meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
  #                                base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
  #                                meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
  #                                verbose = TRUE
  #  ), "rebalancing_months is not the same in every base SB backtest results."
  #)

  #Different target_fwd_name
  ############################
  ew_config <- create_sb_backtest_config(sb_algorithm = "ew", training_sample_size = 4, rebalancing_months = 6, target_fwd_name = "fwd_premium_1m",
                                         config_name = "ew_123")

  suppressWarnings(
    wrong_ew_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      ss_backtest_results = ss_results,
      config = ew_config,
      parallel = FALSE,
      verbose = TRUE
    )
  )

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  features_passthrough = "all",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  base_sb_backtest_results_list = list(rp_results, wrong_ew_results, mvo_results),
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "target_fwd_name is not the same in every base SB backtest results."
  )

  #meta_custom_signal_weights_m_df is not NULL when custom obj is max_hr
  ############################
  wrong_meta_learner_config <- create_sb_backtest_config(sb_algorithm = "sw", custom_objective = "max_hr", training_sample_size = 4, target_fwd_name = "fwd_premium_3m",
                                                   chosen_signals_and_positions = "all",
                                                   rebalancing_months = 6, config_name = "meta")

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = wrong_meta_learner_config,
                                  features_passthrough = "none",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  base_sb_backtest_results_list = list(rp_results, ew_results, mvo_results),
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = features_m_df,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "base_custom_signal_universe_metrics_m_df and meta_custom_signal_universe_metrics_m_df should be NULL when using custom_objective from oos_testing_eval_metrics."
  )

  #meta_custom_signal_weights_m_df is not NULL when custom obj is max_hr
  ############################
  expect_warning(
  wrong_meta_learner_config <- create_sb_backtest_config(sb_algorithm = "sw", custom_objective = "max_tir", training_sample_size = 4, target_fwd_name = "fwd_premium_3m",
                                                         chosen_signals_and_positions = "all",
                                                         rebalancing_months = 6, config_name = "meta"),
  "custom_objective is not one of typical valid heuristic performance. Please be sure that the metric is present in signal_universe_m_df")


  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = wrong_meta_learner_config,
                                  features_passthrough = "none",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  base_sb_backtest_results_list = list(rp_results, ew_results, mvo_results),
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = features_m_df,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "custom_objective should be contained in meta_custom_signal_universe_metrics_m_df."
  )




  #only one sb_backtest_result provided
  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  base_sb_backtest_results_list = list(rp_results),
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "More than one base_sb_backtest_results_list must be supplied."
  )

  #base_sb results contains objs that are no sb results
  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  base_sb_backtest_results_list = list(rp_results, mvo_config),
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "All elements in 'base_sb_backtest_results_list' must be of class 'sb_backtest_results'."
  )





  #object provided is no mdf/mxts
  meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  features_passthrough = c("roe_3m", "sharpe_6m"),
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(rp_results, mvo_results),
                                  base_signal_themes_m_df = signal_themes_m_df@data,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "If provided, base_signal_themes_m_df must be a meta_dataframe object."
  )

  expect_error(
    check_inputs_meta_sb_backtest(config = meta_config,
                                  base_sb_backtest_results_list = list(rp_results, mvo_results),
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts@data, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "If provided, base_backtest_returns_m_xts must be a meta_xts object."
  )


})

test_that("check_inputs_meta_sb_backtest works with wrong combination of base and meta objs", {

  #Load objects
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #First apply a signal selection backtest
  set.seed(123)
  #Backtest Returns
  mocked_backtest_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    asset_turnover_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 5, sd = 3.5),
    book_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1, sd = 5),
    dps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 15, sd = 0.4),
    eps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.0005, sd = 0.3),
    mom_res_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 3.15, sd = 3.5),
    roe_3m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.1, sd = 2),
    sharpe_6m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 2.5, sd = 5),
    low_idio_vol_mrkt_ewma = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.05, sd = 7.5)
  ), order.by = unique(toy_preprocessed_features$dates)), meta_xts_name = "backtest")

  #Benchmark Returns xts
  suppressWarnings(
    mocked_benchmark_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
      IBOV = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.01, sd = 0.035),
      SMLL = rnorm(length(unique(toy_preprocessed_features$dates)), mean = -0.01, sd = 0.025)
    ),  order.by = unique(toy_preprocessed_features$dates)), meta_xts_name = "benchmark")
  )

  #Chosen Signals and Positions
  chosen_signals_and_positions <- c(asset_turnover_12m = "long", book_yield = "long", dps_yield = "long", eps_yield = "long",
                                    idio_vol_mrkt_ewma = "short", sharpe_6m = "long")

  #Mocked Signal Themes
  mocked_signal_themes_m_df <- expand.grid(
    tickers = names(mocked_backtest_returns_m_xts@data),
    dates = unique(toy_preprocessed_features$dates),
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(id = paste0(tickers,"-",dates),
                      theme = dplyr::case_when(
                        tickers %in% c("mom_res_12m", "sharpe_6m") ~ "momentum",
                        tickers %in% c("dy_med_36m", "eps_yield", "book_yield", "asset_turnover_12m", "dps_yield") ~ "value",
                        tickers %in% c("roe_3m", "low_idio_vol_mrkt_ewma") ~ "defensive"
                      )
  ) %>%  dplyr::arrange(id) %>% dplyr::select(id, tickers, dates, theme)

  signal_themes_m_df <- create_meta_dataframe(mocked_signal_themes_m_df, "st_11", type = "groups")

  #SB Configs
  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", training_sample_size = 4, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m",
                                             config_name = "glmnet_123") %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))

  rf_config <- create_sb_backtest_config(sb_algorithm = "rf", training_sample_size = 4, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m",
                                         config_name = "rf_101") %>%
    add_tuning_strategy(tuning_method = "random_search", validation_sample_size = 3, n_iter = 2) %>%
    add_hyperparameter(hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
                       distribution_choice = c("uniform", "uniform", "lognormal", "uniform"),
                       pars = list(c(min=0.1, max = 0.9), c(min = 100L, max = 500L), c(meanlog = 1L, sdlog = 1L),
                                   c(min = 1L, max = 10L))
    )

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, "feats_123")
  target_m_df <- create_meta_dataframe(toy_preprocessed_targets, "tg_123")


  suppressWarnings(
    glmnet_results <- run_sb_backtest(
      config = glmnet_config,
      features_m_df = features_m_df,
      target_m_df = target_m_df
    )
  )

  set.seed(123)
  suppressWarnings(
    rf_results <- run_sb_backtest(
      config = rf_config,
      features_m_df = features_m_df,
      target_m_df = target_m_df
    )
  )


  #Meta configs
  meta_learner_config <- create_sb_backtest_config(sb_algorithm = "glmnet", training_sample_size = 4, target_fwd_name = "fwd_premium_3m",
                                                   chosen_signals_and_positions = "all",
                                                   rebalancing_months = 6, config_name = "meta") %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))


  meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  features_passthrough = c("roe_3m", "sharpe_6m"),
                                  config_name = "meta_rf_glmnet")

  #Meta Signal Themes
  ################
  expect_error(
    check_inputs_meta_sb_backtest(config = meta_config,
                                   features_m_df = features_m_df,
                                   target_m_df = target_m_df,
                                   base_sb_backtest_results_list = list(rf_results, glmnet_results),
                                   base_custom_signal_weights_m_df = NULL,
                                   meta_custom_signal_weights_m_df = NULL,
                                   base_custom_signal_universe_metrics_m_df = NULL,
                                   meta_custom_signal_universe_metrics_m_df = NULL,
                                   base_backtest_returns_m_xts = NULL,
                                   meta_backtest_returns_m_xts = NULL,
                                   base_benchmark_returns_m_xts = NULL,
                                   meta_benchmark_returns_m_xts = NULL,
                                   base_signal_themes_m_df = signal_themes_m_df,
                                   meta_signal_themes_m_df = signal_themes_m_df),
    "base_signal_themes_m_df and meta_signal_themes_m_df should be different objects.")

  meta_signal_themes_m_df <- expand.grid(tickers = c("c:rf_101_f:feats_123_t:tg_123-fwd_premium_3m", "c:glmnet_123_f:feats_123_t:tg_123-fwd_premium_3m"),
                                         dates = signal_themes_m_df@data$dates %>% unique()) %>%
    dplyr::mutate(id = paste0(tickers, "-", dates), .before = tickers) %>%
    dplyr::mutate(tickers = as.character(tickers), theme = "ml") %>%
    dplyr::arrange(id) %>%
    create_meta_dataframe()

  wrong_meta_signal_themes_m_df <- meta_signal_themes_m_df
  wrong_meta_signal_themes_m_df@data$id[1] <- signal_themes_m_df@data$id[1]

  expect_error(
    check_inputs_meta_sb_backtest(config = meta_config,
                                  features_m_df = features_m_df,
                                  target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(rf_results, glmnet_results),
                                  base_custom_signal_weights_m_df = NULL,
                                  meta_custom_signal_weights_m_df = NULL,
                                  base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = NULL,
                                  meta_backtest_returns_m_xts = NULL,
                                  base_benchmark_returns_m_xts = NULL,
                                  meta_benchmark_returns_m_xts = NULL,
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  meta_signal_themes_m_df = wrong_meta_signal_themes_m_df),
    "base_signal_themes_m_df and meta_signal_themes_m_df should not share any ids.")


  wrong_meta_signal_themes_m_df <- meta_signal_themes_m_df
  wrong_meta_signal_themes_m_df@data$tickers[1] <- signal_themes_m_df@data$tickers[1]

  expect_error(
    check_inputs_meta_sb_backtest(config = meta_config,
                                  features_m_df = features_m_df,
                                  target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(rf_results, glmnet_results),
                                  base_custom_signal_weights_m_df = NULL,
                                  meta_custom_signal_weights_m_df = NULL,
                                  base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = NULL,
                                  meta_backtest_returns_m_xts = NULL,
                                  base_benchmark_returns_m_xts = NULL,
                                  meta_benchmark_returns_m_xts = NULL,
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  meta_signal_themes_m_df = wrong_meta_signal_themes_m_df),
    "base_signal_themes_m_df and meta_signal_themes_m_df should not share any tickers.")


  wrong_meta_signal_themes_m_df <- meta_signal_themes_m_df
  colnames(wrong_meta_signal_themes_m_df@data)[4] <- "group"

  expect_error(
    check_inputs_meta_sb_backtest(config = meta_config,
                                  features_m_df = features_m_df,
                                  target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(rf_results, glmnet_results),
                                  base_custom_signal_weights_m_df = NULL,
                                  meta_custom_signal_weights_m_df = NULL,
                                  base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = NULL,
                                  meta_backtest_returns_m_xts = NULL,
                                  base_benchmark_returns_m_xts = NULL,
                                  meta_benchmark_returns_m_xts = NULL,
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  meta_signal_themes_m_df = wrong_meta_signal_themes_m_df),
    "base_signal_themes_m_df and meta_signal_themes_m_df should have the same columns.")

  wrong_meta_signal_themes_m_df <- meta_signal_themes_m_df
  wrong_meta_signal_themes_m_df@data$dates[1] <- as.Date("2023-07-16")

  expect_error(
    check_inputs_meta_sb_backtest(config = meta_config,
                                  features_m_df = features_m_df,
                                  target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(rf_results, glmnet_results),
                                  base_custom_signal_weights_m_df = NULL,
                                  meta_custom_signal_weights_m_df = NULL,
                                  base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = NULL,
                                  meta_backtest_returns_m_xts = NULL,
                                  base_benchmark_returns_m_xts = NULL,
                                  meta_benchmark_returns_m_xts = NULL,
                                  base_signal_themes_m_df = signal_themes_m_df,
                                  meta_signal_themes_m_df = wrong_meta_signal_themes_m_df),
    "All dates in meta_signal_themes_m_df should be present in base_signal_themes_m_df.")

  expect_error(
    check_inputs_meta_sb_backtest(config = meta_config,
                                  features_m_df = features_m_df,
                                  target_m_df = target_m_df,
                                  base_sb_backtest_results_list = list(rf_results, glmnet_results),
                                  base_custom_signal_weights_m_df = NULL,
                                  meta_custom_signal_weights_m_df = NULL,
                                  base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = NULL,
                                  meta_backtest_returns_m_xts = NULL,
                                  base_benchmark_returns_m_xts = NULL,
                                  meta_benchmark_returns_m_xts = NULL,
                                  base_signal_themes_m_df = NULL,
                                  meta_signal_themes_m_df = meta_signal_themes_m_df),
    "base_signal_themes_m_df should be provided when features_passthrough is different from 'none'.")

  ##############


})


