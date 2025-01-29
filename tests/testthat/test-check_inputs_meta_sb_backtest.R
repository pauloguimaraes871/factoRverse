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
  #Meta configs
  meta_learner_config <- create_sb_backtest_config(sb_algorithm = "glmnet", training_sample_size = 4, target_fwd_name = "fwd_premium_3m",
                                                   chosen_signals_and_positions = "all",
                                                   rebalancing_months = 6, config_name = "meta") %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))

  meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_configs = list(rf_config, glmnet_config),
                                  features_passthrough = c("roe_3m", "sharpe_6m", "not_contained"),
                                  config_name = "meta_rf_glmnet")


  #Features passthrough contained
  ##################################
  expect_error(
    check_inputs_meta_sb_backtest(config = meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
  ), "features_passthrough should be contained in features_m_df."
  )

  #Features passthrough is 'none' when using custom_obj
  ##################################
  meta_learner_config <- create_sb_backtest_config(sb_algorithm = "sw", custom_objective = "max_hr", training_sample_size = 4, target_fwd_name = "fwd_premium_3m",
                                                   chosen_signals_and_positions = "all",
                                                   rebalancing_months = 6, config_name = "meta")

  meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_configs = list(rf_config, glmnet_config),
                                  features_passthrough = c("roe_3m", "sharpe_6m"),
                                  config_name = "meta_rf_glmnet")

  expect_error(
    check_inputs_meta_sb_backtest(config = meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
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
      config = rp_config %>% add_ss_backtest_obj(ss_results),
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
      config = ew_config %>% add_ss_backtest_obj(ss_results),
      parallel = FALSE,
      verbose = TRUE
    )
  )

  set.seed(123)
  suppressWarnings(
    mvo_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      config = mvo_config %>% add_ss_backtest_obj(ss_results),
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



  #Repeated objects at SS level
  #####################
  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_results = list(rp_results, rp_results, mvo_results),
                                  features_passthrough = c("roe_3m", "sharpe_6m"),
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "Base sb backtest identifiers must have unique names."
  )

  #Different signals_m_df
  #####################
  wrong_ew_results <- ew_results
  wrong_ew_results@ss_backtest_results@ss_backtest_workflow$signals_object_name <- "wrong_name"

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_results = list(rp_results, wrong_ew_results, mvo_results),
                                  features_passthrough = c("roe_3m", "sharpe_6m"),
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "signals_m_df object is not the same in every base SS backtest results and/or with the features_m_df being currently supplied."
  )

  #Different signal_themes_m_df
  #####################
  wrong_ew_results <- ew_results
  wrong_ew_results@ss_backtest_results@ss_backtest_workflow$signal_themes_object_name <- "wrong_name"

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_results = list(rp_results, wrong_ew_results, mvo_results),
                                  features_passthrough = c("roe_3m", "sharpe_6m"),
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "signal_themes_m_df object is not the same in every base SS backtest results."
  )


  #Different backtest_returns
  #####################
  wrong_ew_results <- ew_results
  wrong_ew_results@ss_backtest_results@ss_backtest_workflow$backtest_returns_object_name <- "wrong_name"

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_results = list(rp_results, wrong_ew_results, mvo_results),
                                  features_passthrough = c("roe_3m", "sharpe_6m"),
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "backtest_returns_m_xts object is not the same in every base SS backtest results."
  )

  #Different benchmark_returns
  #####################
  wrong_rp_results <- rp_results
  wrong_rp_results@ss_backtest_results@ss_backtest_workflow$benchmark_returns_object_name <- "wrong_name"

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_results = list(wrong_rp_results, ew_results, mvo_results),
                                  features_passthrough = c("all"),
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "benchmark_returns_m_xts object is not the same in every base SS backtest results."
  )



  #Repeated objects at SB level
  #####################

  #Different features_m_df
  #####################
  wrong_ew_results <- ew_results
  wrong_ew_results@sb_backtest_workflow$features_object_name <- "wrong_name"

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_results = list(rp_results, wrong_ew_results, mvo_results),
                                  features_passthrough = "all",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "features_m_df object is not the same in every base SB base backtest results and/or with the features_m_df being currently supplied."
  )

  #Different target_m_df
  #####################
  wrong_ew_results <- ew_results
  wrong_ew_results@sb_backtest_workflow$target_object_name <- "wrong_name"

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_results = list(rp_results, wrong_ew_results, mvo_results),
                                  features_passthrough = "all",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "target_m_df object is not the same in every base SB base backtest results and/or with the target_m_df being currently supplied."
  )


  #Different backtest_returns
  ############################
  wrong_rp_results <- rp_results
  wrong_rp_results@sb_backtest_workflow$backtest_returns_object_name <- "wrong_name"

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_results = list(wrong_rp_results, ew_results, mvo_results),
                                  features_passthrough = "all",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "backtest_returns_object_name objects are not the same among the filtered backtest results."
  )


  ##Expect no error for EW, which does not require a backtest_returns object
  right_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_results = list(ew_results, mvo_results),
                                  features_passthrough = c("all"),
                                  config_name = "meta_rf_ew")

  expect_no_error(
    check_inputs_meta_sb_backtest(config = right_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    )
  )

  #Different signal_themes
  ############################
  wrong_rp_results <- rp_results
  wrong_rp_results@sb_backtest_workflow$signal_themes_object_name <- "wrong_name"

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_results = list(wrong_rp_results, ew_results, mvo_results),
                                  features_passthrough = "all",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
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
      config = glmnet_config %>% add_ss_backtest_obj(ss_results),
      parallel = FALSE,
      verbose = TRUE
    )
  )

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_results = list(rp_results, wrong_glmnet_results, mvo_results),
                                  features_passthrough = "all",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "training_sample_size plus validation_sample_size is not the same in every base SB backtest results."
  )


  #Different rebalancing_month
  ############################
  ew_config <- create_sb_backtest_config(sb_algorithm = "ew", training_sample_size = 4, rebalancing_months = 8, target_fwd_name = "fwd_premium_3m",
                                         config_name = "ew_123")

  suppressWarnings(
    wrong_ew_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      config = ew_config %>% add_ss_backtest_obj(ss_results),
      parallel = FALSE,
      verbose = TRUE
    )
  )

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_results = list(rp_results, wrong_ew_results, mvo_results),
                                  features_passthrough = "all",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "rebalancing_months is not the same in every base SB backtest results."
  )

  #Different target_fwd_name
  ############################
  ew_config <- create_sb_backtest_config(sb_algorithm = "ew", training_sample_size = 4, rebalancing_months = 6, target_fwd_name = "fwd_premium_1m",
                                         config_name = "ew_123")

  suppressWarnings(
    wrong_ew_results <- run_sb_backtest(
      target_m_df = target_m_df,
      features_m_df = features_m_df,
      config = ew_config %>% add_ss_backtest_obj(ss_results),
      parallel = FALSE,
      verbose = TRUE
    )
  )

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_results = list(rp_results, wrong_ew_results, mvo_results),
                                  features_passthrough = "all",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL,
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
                                  base_sb_backtest_results = list(rp_results, ew_results, mvo_results),
                                  features_passthrough = "none",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = features_m_df,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "base_custom_signal_universe_metrics_m_df and meta_custom_signal_universe_metrics_m_df should be NULL when using custom_objective from oos_testing_eval_metrics."
  )

  #meta_custom_signal_weights_m_df is not NULL when custom obj is max_hr
  ############################
  suppressWarnings(wrong_meta_learner_config <- create_sb_backtest_config(sb_algorithm = "sw", custom_objective = "max_tir", training_sample_size = 4, target_fwd_name = "fwd_premium_3m",
                                                         chosen_signals_and_positions = "all",
                                                         rebalancing_months = 6, config_name = "meta")
  )

  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = wrong_meta_learner_config,
                                  base_sb_backtest_results = list(rp_results, ew_results, mvo_results),
                                  features_passthrough = "none",
                                  config_name = "meta_rf_ew")

  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = features_m_df,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "custom_objective should be contained in meta_custom_signal_universe_metrics_m_df."
  )


})

test_that("checks related to base_backtest_configs work", {

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

  #Meta configs
  meta_learner_config <- create_sb_backtest_config(sb_algorithm = "ew", training_sample_size = 4, target_fwd_name = "fwd_premium_3m",
                                                   chosen_signals_and_positions = "all",
                                                   rebalancing_months = 6, config_name = "meta")


  ##Training sample size
  #######################
  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_configs = list(ew_config, rp_config, mvo_config),
                                  features_passthrough = "none",
                                  config_name = "meta_rf_ew")

  wrong_meta_config@base_sb_backtest_configs[[1]]@training_sample_size <- 3


  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL,
                                  meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "training_sample_size plus validation_sample_size is not the same in every base SB backtest configs"
  )

  ##Rebal Months
  #######################
  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_configs = list(ew_config, rp_config, mvo_config),
                                  features_passthrough = "none",
                                  config_name = "meta_rf_ew")

  wrong_meta_config@base_sb_backtest_configs[[1]]@rebalancing_months <- 3


  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL,
                                  meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "rebalancing_months is not the same in every base SB backtest configs."
  )

  ##tgt fwd
  #######################
  wrong_meta_config <-
    create_sb_metabacktest_config(meta_sb_backtest_config = meta_learner_config,
                                  base_sb_backtest_configs = list(ew_config, rp_config, mvo_config),
                                  features_passthrough = "none",
                                  config_name = "meta_rf_ew")

  wrong_meta_config@base_sb_backtest_configs[[1]]@target_fwd_name <- "fwd_premium_1m"


  expect_error(
    check_inputs_meta_sb_backtest(config = wrong_meta_config,
                                  features_m_df = features_m_df, target_m_df = target_m_df,
                                  base_signal_themes_m_df = signal_themes_m_df, base_priors_m_df = NULL,
                                  base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL,
                                  meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, meta_custom_signal_weights_m_df = NULL,
                                  meta_custom_signal_universe_metrics_m_df = NULL,
                                  base_backtest_returns_m_xts = mocked_backtest_returns_m_xts, base_benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                  meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL,
                                  verbose = TRUE
    ), "target_fwd_name is not the same in every base SB backtest configs."
  )



})
