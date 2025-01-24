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
  suppressWarnings(
  ols_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = ols_config,
    verbose = TRUE,
    parallel = FALSE
  )
  )

  suppressWarnings(
  glmnet_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = glmnet_config,
    verbose = TRUE,
    parallel = FALSE
  )
  )

  suppressWarnings(
  rf_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = rf_config,
    verbose = TRUE,
    parallel = FALSE
  )
  )

  #Get sb_backtest_results
  sb_backtest_results_list <- list(ols_config = ols_results, glmnet_config = glmnet_results, rf_config = rf_results)
  names(sb_backtest_results_list) <-  sapply(sb_backtest_results_list, function(x) x@backtest_identifier)

  predictions_m_df <- consolidate_oos_sb_outputs_m_df(sb_backtest_results_list, winsorize_predictions = FALSE, normalize_predictions = FALSE)

  #Expect class to be meta_dataframe
  expect_equal(as.character(class(predictions_m_df)), "signals_m_df")
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

test_that("convert_oos_predictions_lists_to_m_df returns a meta_dataframe with expected format for features_pass = 'all', winsorization and normalization", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, type = "signals", meta_dataframe_name = "feat123")
  target_m_df <- create_meta_dataframe(toy_preprocessed_targets, type = "target", meta_dataframe_name = "target123")

  #First apply a signal selection backtest
  set.seed(123)
  #Backtest Returns
  mocked_backtest_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    asset_turnover_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 5, sd = 3.5),
    book_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1, sd = 5),
    dps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 15, sd = 0.4),
    eps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.5, sd = 1.3),
    mom_res_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 3.15, sd = 3.5),
    roe_3m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.1, sd = 2),
    sharpe_6m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 2.5, sd = 5),
    low_idio_vol_mrkt_ewma = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.05, sd = 7.5)
  ), order.by = unique(toy_preprocessed_features$dates)),
  type = "assets", meta_xts_name = "mocked_xts")

  #Benchmark Returns XTS
  suppressWarnings(mocked_benchmark_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    IBOV = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.01, sd = 0.035),
    SMLL = rnorm(length(unique(toy_preprocessed_features$dates)), mean = -0.01, sd = 0.025)
  ),  order.by = unique(toy_preprocessed_features$dates)),
  type = "assets", meta_xts_name = "mocked_benchmarks")
 )

  #Chosen Signals and Positions
  chosen_signals_and_positions <- c(asset_turnover_12m = "long", book_yield = "long", dps_yield = "long", eps_yield = "long",
                                    idio_vol_mrkt_ewma = "short", sharpe_6m = "long", sectors_c1Agro = "force")

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

  ##SS Config
  frequentist_ss_config <- create_ss_backtest_config(initial_sample_size = 3, rebalancing_months = 6,
                                                     split_method = "expanding", config_name = "frequentist_ss", active_returns = TRUE,
                                                     chosen_signals_and_positions = chosen_signals_and_positions
  ) %>%
    add_alpha_test_strategy(model_structure = "no_pooled",
                            signal_significance_threshold = 0.15, p_correction_method = "none",
                            market_factor_proxy = "IBOV", enable_theme_representativeness = TRUE)


  ss_results <- suppressWarnings( #This is for NA warning of NAs at the end of run_ss_backtest
    run_ss_backtest(frequentist_ss_config,
                    signals_m_df = features_m_df, backtest_returns_m_xts = mocked_backtest_returns_m_xts, benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                    signal_themes_m_df = signal_themes_m_df,
                    verbose = TRUE
    )
  )


  ##SB Config
  ols_config <- create_sb_backtest_config(sb_algorithm = "ols", custom_objective = "squared_error", target_fwd_name = "fwd_premium_3m",
                                          training_sample_size = 9, rebalancing_months = 6, config_name = "ols1") %>%
    add_ss_backtest_obj(ss_results)

  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", training_sample_size = 6, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m", config_name = "glm1") %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9))) %>%
    add_ss_backtest_obj(ss_results)

  rf_config <- create_sb_backtest_config(sb_algorithm = "rf", training_sample_size = 6, rebalancing_months = 6, target_fwd_name = "fwd_premium_3m", config_name = "rf1") %>%
    add_tuning_strategy(tuning_method = "random_search", validation_sample_size = 3, n_iter = 2) %>%
    add_hyperparameter(hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
                       distribution_choice = c("uniform", "uniform", "lognormal", "uniform"),
                       pars = list(c(min=0.1, max = 0.9), c(min = 100L, max = 500L), c(meanlog = 1L, sdlog = 1L),
                                   c(min = 1L, max = 10L))
    ) %>%
    add_ss_backtest_obj(ss_results)

  set.seed(123)
  suppressWarnings(
  ols_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = ols_config,
    verbose = TRUE,
    parallel = FALSE
  )
  )

  suppressWarnings(
  glmnet_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = glmnet_config,
    verbose = TRUE,
    parallel = FALSE
  )
  )

  suppressWarnings(
  rf_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = rf_config,
    verbose = TRUE,
    parallel = FALSE
  )
  )

  #Get sb_backtest_results
  sb_backtest_results_list <- list(ols_config = ols_results, glmnet_config = glmnet_results, rf_config = rf_results)
  names(sb_backtest_results_list) <-  sapply(sb_backtest_results_list, function(x) x@backtest_identifier)

  #features_passthrough_and_positions
  features_passthrough <- c("asset_turnover_12m", "book_yield", "idio_vol_mrkt_ewma", "sharpe_6m", "sectors_c1Agro")

  features_passthrough_and_positions <- get_features_positions(base_sb_backtest_results_list = sb_backtest_results_list,
                                                               features_passthrough = features_passthrough, features_m_df = features_m_df
                                                               )

  predictions_m_df <- consolidate_oos_sb_outputs_m_df(sb_backtest_results_list, winsorize_predictions = FALSE, normalize_predictions = FALSE,
                                                      features_passthrough_and_positions = features_passthrough_and_positions, features_m_df = features_m_df)

  #Expect class to be meta_dataframe
  expect_equal(as.character(class(predictions_m_df)), "signals_m_df")
  #Expect ncol to be right
  expect_equal(ncol(predictions_m_df@data), 3 + length(sb_backtest_results_list) + length(features_passthrough)) #id, tickers, dates = 3
  #Expect number of stocks to be equal
  expect_equal(predictions_m_df@unique_tickers, ols_results@oos_sb_outputs_m_df@unique_tickers)
  #Expect number of months to be equal
  expect_equal(predictions_m_df@unique_dates, ols_results@oos_sb_outputs_m_df@unique_dates)
  #Expect columns to be backtest names + features
  expect_equal(names(predictions_m_df@data),
               c("id", "tickers", "dates", unname(sapply(sb_backtest_results_list, function(x) x@backtest_identifier)),
                 "asset_turnover_12m", "book_yield", "idio_vol_mrkt_ewma", "sharpe_6m", "sectors_c1Agro"))
  #Expect columns to match features_m_df
  expect_equal(predictions_m_df@data %>% dplyr::pull(asset_turnover_12m), features_m_df@data %>% dplyr::filter(id %in% predictions_m_df@data$id) %>% dplyr::pull(asset_turnover_12m))
  expect_equal(predictions_m_df@data %>% dplyr::pull(book_yield), features_m_df@data %>% dplyr::filter(id %in% predictions_m_df@data$id) %>% dplyr::pull(book_yield))
  expect_equal(predictions_m_df@data %>% dplyr::pull(idio_vol_mrkt_ewma), features_m_df@data %>% dplyr::filter(id %in% predictions_m_df@data$id) %>% dplyr::pull(idio_vol_mrkt_ewma))
  expect_equal(predictions_m_df@data %>% dplyr::pull(sharpe_6m), features_m_df@data %>% dplyr::filter(id %in% predictions_m_df@data$id) %>% dplyr::pull(sharpe_6m))
  expect_equal(predictions_m_df@data %>% dplyr::pull(sectors_c1Agro), features_m_df@data %>% dplyr::filter(id %in% predictions_m_df@data$id) %>% dplyr::pull(sectors_c1Agro))


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


  #Now run with winsorize and normalize
  winsorized_predictions_m_df <- predictions_m_df@data %>% dplyr::select(-names(features_passthrough_and_positions)) %>% winsorize_panel_data(probs = c(0.025,0.975))
  normalized_predictions_m_df <- winsorized_predictions_m_df %>% normalize_panel_data()
  normalized_predictions_m_df <- dplyr::left_join(normalized_predictions_m_df@data,
                                                 features_m_df@data %>% dplyr::select(id, names(features_passthrough_and_positions)),
                                                 by = "id")

  predictions_m_df <- consolidate_oos_sb_outputs_m_df(sb_backtest_results_list, winsorize_predictions = TRUE, normalize_predictions = TRUE, winsorization_probs = c(0.025,0.975),
                                                      features_passthrough_and_positions = features_passthrough_and_positions, features_m_df = features_m_df)

  expect_equal(predictions_m_df@data, normalized_predictions_m_df)

  #Check for correct winsorization
  expect_equal(predictions_m_df@workflow,
               list(c("winsorization with 0.025 0.975 quantiles and following variables with Infs preserved: "), c("normalization")))


})

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
  suppressWarnings(
  ols_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = ols_config,
    verbose = TRUE,
    parallel = FALSE
  )
  )

  suppressWarnings(
  glmnet_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = glmnet_config,
    verbose = TRUE,
    parallel = FALSE
  )
  )

  suppressWarnings(
  rf_results <- run_sb_backtest(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    config = rf_config,
    verbose = TRUE,
    parallel = FALSE
  )
  )

  #Get sb_backtest_results
  ols_results_incomplete <- ols_results
  ols_results_incomplete@oos_sb_outputs_m_df@data <- ols_results_incomplete@oos_sb_outputs_m_df@data[-2,]
  sb_backtest_results_list_incomplete <- list(ols_config = ols_results_incomplete, glmnet_config = glmnet_results, rf_config = rf_results)

  expect_error(consolidate_oos_sb_outputs_m_df(sb_backtest_results_list_incomplete, winsorize_predictions = FALSE, normalize_predictions = FALSE),
               "Elements of oos_sb_outputs_m_df in each sb_backtest_results object must be the same.")


  sb_backtest_results_list <- list(ols_config = ols_results, glmnet_config = glmnet_results, rf_config = rf_results)
  names(sb_backtest_results_list) <-  sapply(sb_backtest_results_list, function(x) x@backtest_identifier)
  features_m_df@data <- features_m_df@data[-5,] #Taking one out at begging won't matter because there is no corresponding id in preds

  expect_silent(consolidate_oos_sb_outputs_m_df(sb_backtest_results_list, winsorize_predictions = FALSE, normalize_predictions = FALSE, features_m_df = features_m_df))

  features_m_df@data <- features_m_df@data %>% dplyr::filter(!id == ols_results@oos_sb_outputs_m_df@data$id[200]) #Taking one out at the end matters

  expect_error(consolidate_oos_sb_outputs_m_df(sb_backtest_results_list, winsorize_predictions = FALSE, normalize_predictions = FALSE, features_m_df = features_m_df),
               "Not all ids in base_sb_backtest_results are available in features_m_df")


  sb_backtest_results_list_repeated <- list(ols_config = ols_results, glmnet_config = ols_results, rf_config = rf_results)

  expect_error(consolidate_oos_sb_outputs_m_df(sb_backtest_results_list_repeated, winsorize_predictions = FALSE, normalize_predictions = FALSE),
               "Backtest identifiers must be unique.")

})

test_that("consolidate_backtest_returns_m_xts adequately combines base and meta backtests", {


  #Create objects
  set.seed(123)
  #Backtest Returns
  meta_backtest_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    rf_results = rnorm(10, mean = 5, sd = 3.5),
    ols_results = rnorm(10, mean = 1, sd = 5),
    ew_results = rnorm(10, mean = 15, sd = 0.4),
   order.by = seq.Date(from = as.Date("2000-01-01"), by = "month", length.out = 10))),
  type = "assets", meta_xts_name = "meta_xts")

  base_backtest_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    asset_turnover_12m = rnorm(5, mean = 5, sd = 3.5),
    book_yield = rnorm(5, mean = 1, sd = 5),
    dps_yield = rnorm(5, mean = 15, sd = 0.4),
    eps_yield = rnorm(5, mean = 0.5, sd = 1.3),
    mom_res_12m = rnorm(5, mean = 3.15, sd = 3.5),
    roe_3m = rnorm(5, mean = 1.1, sd = 2),
    sharpe_6m = rnorm(5, mean = 2.5, sd = 5),
    low_idio_vol_mrkt_ewma = rnorm(5, mean = 1.05, sd = 7.5)
  ), order.by = seq.Date(from = as.Date("2000-06-01"), by = "month", length.out = 5)),
  type = "assets", meta_xts_name = "base_xts")


  #Merge them according to meta_backtest_returns_xts
  expected_results <- merge(meta_backtest_returns_m_xts@data, base_backtest_returns_m_xts@data, join = "left") %>% na.omit() %>% create_meta_xts(
    meta_xts_name = "meta_xts_base_xts", type = "assets"
  )

  expect_equal(consolidate_backtest_returns_m_xts(meta_backtest_returns_m_xts, base_backtest_returns_m_xts),
               expected_results)

  #Only meta_backtest_returns_xts
  expect_equal(consolidate_backtest_returns_m_xts(meta_backtest_returns_m_xts, base_backtest_returns_m_xts = NULL),
               meta_backtest_returns_m_xts)


  #Only base_backtest_returns_xts
  expect_equal(consolidate_backtest_returns_m_xts(meta_backtest_returns_m_xts = NULL, base_backtest_returns_m_xts = base_backtest_returns_m_xts),
               NULL)


})

test_that("consolidate_benchmark_returns_m_xts adequately combines base and meta benchmarks", {


  #Create objects
  set.seed(123)
  #Backtest Returns
  meta_benchmark_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    theme_ss = rnorm(10, mean = 5, sd = 3.5),
    theme_sb = rnorm(10, mean = 1, sd = 5)
  ), order.by = seq.Date(from = as.Date("2000-01-01"), by = "month", length.out = 10)),
    type = "assets", meta_xts_name = "meta_xts")

  base_benchmark_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    IBOV = rnorm(5, mean = 5, sd = 3.5),
    IDIV = rnorm(5, mean = 1, sd = 5),
    SMLL = rnorm(5, mean = 15, sd = 0.4)
  ), order.by = seq.Date(from = as.Date("2000-06-01"), by = "month", length.out = 5)),
  type = "assets", meta_xts_name = "base_xts")


  #Merge them according to meta_backtest_returns_xts
  expected_results <- merge(meta_benchmark_returns_m_xts@data, base_benchmark_returns_m_xts@data, join = "left") %>% na.omit() %>% create_meta_xts(
    type = "assets", meta_xts_name = "meta_xts_base_xts"
  )

  expect_equal(consolidate_benchmark_returns_m_xts(meta_benchmark_returns_m_xts, base_benchmark_returns_m_xts),
               expected_results)

  #Only meta_benchmark_returns_xts
  expect_equal(consolidate_benchmark_returns_m_xts(meta_benchmark_returns_m_xts, base_benchmark_returns_m_xts = NULL),
               meta_benchmark_returns_m_xts)


  #Only base_benchmark_returns_xts
  expect_equal(consolidate_benchmark_returns_m_xts(meta_benchmark_returns_m_xts = NULL, base_benchmark_returns_m_xts = base_benchmark_returns_m_xts),
               base_benchmark_returns_m_xts)


})

test_that("consolidate_generic_meta_dataframes adequately combines base and meta mdfs", {

  #Base Signal Themes
  base_signal_themes_m_df <- expand.grid(
    tickers = c("mom_res_12m", "sharpe_6m", "dy_med_36m", "eps_yield", "book_yield", "asset_turnover_12m", "dps_yield", "roe_3m", "low_idio_vol_mrkt_ewma"),
    dates = seq.Date(from = as.Date("2000-01-01"), by = "month", length.out = 10),
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(id = paste0(tickers,"-",dates),
                      theme = dplyr::case_when(
                        tickers %in% c("mom_res_12m", "sharpe_6m") ~ "momentum",
                        tickers %in% c("dy_med_36m", "eps_yield", "book_yield", "asset_turnover_12m", "dps_yield") ~ "value",
                        tickers %in% c("roe_3m", "low_idio_vol_mrkt_ewma") ~ "defensive"
                      )
  ) %>%  dplyr::arrange(id) %>% dplyr::select(id, tickers, dates, theme)

  base_signal_themes_m_df <- create_meta_dataframe(base_signal_themes_m_df, "st_11", type = "groups")

  #Meta Signal Themes
  meta_signal_themes_m_df <- expand.grid(
    tickers = c("rf_results", "ols_results", "xgb_results", "glmnet_results", "nn1_results", "nn2_results", "nn3_results", "rp_results", "mvo_results"),
    dates = seq.Date(from = as.Date("2000-05-01"), by = "month", length.out = 5),
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(id = paste0(tickers,"-",dates),
                      theme = dplyr::case_when(
                        tickers %in% c("rf_results", "xgb_results") ~ "tree",
                        tickers %in% c("ols_results", "glmnet_results") ~ "linear",
                        tickers %in% c("nn1_results", "nn2_results", "nn3_results") ~ "neural",
                        tickers %in% c("rp_results", "mvo_results") ~ "heuristic"
                      )
  ) %>%  dplyr::arrange(id) %>% dplyr::select(id, tickers, dates, theme)

  meta_signal_themes_m_df <- create_meta_dataframe(meta_signal_themes_m_df, "meta_st", type = "groups")

  #Merge them according to meta_signal_themes_m_df
  expected_results <- dplyr::bind_rows(meta_signal_themes_m_df@data, base_signal_themes_m_df@data) %>% dplyr::arrange(id) %>% create_meta_dataframe(
    type = "groups", meta_dataframe_name = "meta_st_st_11"
  )

  expect_equal(consolidate_generic_meta_dataframes(meta_signal_themes_m_df, base_signal_themes_m_df),
               expected_results)

  expect_equal(consolidate_generic_meta_dataframes(NULL, base_signal_themes_m_df),
               NULL)

  expect_equal(consolidate_generic_meta_dataframes(meta_signal_themes_m_df, NULL),
               meta_signal_themes_m_df)



})

test_that("derive_adapted_custom_signal_universe_m_df adequately creates consolidated_oos_eval_metrics_m_df", {

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
  suppressWarnings(
    ols_results <- run_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      config = ols_config,
      verbose = TRUE,
      parallel = FALSE
    )
  )

  suppressWarnings(
    glmnet_results <- run_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      config = glmnet_config,
      verbose = TRUE,
      parallel = FALSE
    )
  )

  suppressWarnings(
    rf_results <- run_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      config = rf_config,
      verbose = TRUE,
      parallel = FALSE
    )
  )

  #Derive oos_eval_metrics_m_df
  ols_oos_eval_metrics_m_df <- ols_results@oos_testing_eval_metrics_m_xts@data %>% as.data.frame() %>% tibble::rownames_to_column(var = "dates") %>%
    dplyr::mutate(tickers = ols_results@backtest_identifier, .before = dates)
  ols_oos_eval_metrics_m_df$dates <- as.Date(ols_oos_eval_metrics_m_df$dates) + months(3)
  ols_oos_eval_metrics_m_df <- ols_oos_eval_metrics_m_df %>% dplyr::mutate(id = paste0(tickers,"-",dates), .before = tickers)

  glmnet_oos_eval_metrics_m_df <- glmnet_results@oos_testing_eval_metrics_m_xts@data %>% as.data.frame() %>% tibble::rownames_to_column(var = "dates") %>%
    dplyr::mutate(tickers = glmnet_results@backtest_identifier, .before = dates)
  glmnet_oos_eval_metrics_m_df$dates <- as.Date(glmnet_oos_eval_metrics_m_df$dates) + months(3)
  glmnet_oos_eval_metrics_m_df <- glmnet_oos_eval_metrics_m_df %>% dplyr::mutate(id = paste0(tickers,"-",dates), .before = tickers)


  rf_oos_eval_metrics_m_df <- rf_results@oos_testing_eval_metrics_m_xts@data %>% as.data.frame() %>% tibble::rownames_to_column(var = "dates") %>%
    dplyr::mutate(tickers = rf_results@backtest_identifier, .before = dates)
  rf_oos_eval_metrics_m_df$dates <- as.Date(rf_oos_eval_metrics_m_df$dates) + months(3)
  rf_oos_eval_metrics_m_df <- rf_oos_eval_metrics_m_df %>% dplyr::mutate(id = paste0(tickers,"-",dates), .before = tickers)

  #Consolidate
  expected_results <- dplyr::bind_rows(ols_oos_eval_metrics_m_df, glmnet_oos_eval_metrics_m_df, rf_oos_eval_metrics_m_df) %>% dplyr::arrange(id) %>% create_meta_dataframe()

  results <- derive_adapted_custom_signal_universe_m_df(
    meta_custom_objective = "max_hr",
    base_sb_backtest_results_list = list(ols_results, glmnet_results, rf_results),
    meta_custom_signal_universe_metrics_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL)

  expect_equal(results, expected_results)

  expect_true(
    mean((results@data$dates - zoo::index(ols_results@oos_testing_eval_metrics_m_xts@data)) - 90) %>% as.numeric() < 3)


})

test_that("derive_adapted_custom_signal_universe_m_df adequately creates consolidated_oos_eval_metrics_m_df when there are NAs in most recent target_m_df dates", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  current_date <- "2023-07-15"

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, type = "signals", meta_dataframe_name = "feat123")
  target_m_df <- create_meta_dataframe(toy_preprocessed_targets, type = "target", meta_dataframe_name = "target123")
  #Create adapted target_m_df with NAs in last dates
  adapted_target_m_df <- create_meta_dataframe(
    target_m_df@data %>%
      dplyr::arrange(desc(dates)) %>%
      dplyr::mutate(
        dplyr::across(dplyr::ends_with("_3m"), ~ ifelse(dates %in% unique(dates)[1:3], NA, .)),
        dplyr::across(dplyr::ends_with("_1m"), ~ ifelse(dates == unique(dates)[1], NA, .))
      ) %>%
      dplyr::arrange(id)
    , type = "target")


  ols_config <- create_sb_backtest_config(sb_algorithm = "ols", custom_objective = "squared_error", target_fwd_name = "fwd_premium_1m",
                                          training_sample_size = 9, rebalancing_months = 6, config_name = "ols1")

  glmnet_config <- create_sb_backtest_config(sb_algorithm = "glmnet", training_sample_size = 6, rebalancing_months = 6, target_fwd_name = "fwd_premium_1m", config_name = "glm1") %>%
    add_tuning_strategy(tuning_method = "grid_search", validation_sample_size = 3) %>%
    add_hyperparameter(hyperparameter = c("alpha", "lambda.min.ratio"), grid = list(c(0, 1), c(0.5, 0.9)))

  rf_config <- create_sb_backtest_config(sb_algorithm = "rf", training_sample_size = 6, rebalancing_months = 6, target_fwd_name = "fwd_premium_1m", config_name = "rf1") %>%
    add_tuning_strategy(tuning_method = "random_search", validation_sample_size = 3, n_iter = 2) %>%
    add_hyperparameter(hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
                       distribution_choice = c("uniform", "uniform", "lognormal", "uniform"),
                       pars = list(c(min=0.1, max = 0.9), c(min = 100L, max = 500L), c(meanlog = 1L, sdlog = 1L),
                                   c(min = 1L, max = 10L))
    )


  set.seed(123)
  suppressWarnings(
    ols_results <- run_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = adapted_target_m_df,
      config = ols_config,
      verbose = TRUE,
      parallel = FALSE
    )
  )

  suppressWarnings(
    glmnet_results <- run_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = adapted_target_m_df,
      config = glmnet_config,
      verbose = TRUE,
      parallel = FALSE
    )
  )

  suppressWarnings(
    rf_results <- run_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = adapted_target_m_df,
      config = rf_config,
      verbose = TRUE,
      parallel = FALSE
    )
  )

  #Derive oos_eval_metrics_m_df
  ols_oos_eval_metrics_m_df <- ols_results@oos_testing_eval_metrics_m_xts@data %>% as.data.frame() %>% tibble::rownames_to_column(var = "dates") %>%
    dplyr::mutate(tickers = ols_results@backtest_identifier, .before = dates)
  ols_oos_eval_metrics_m_df$dates <- as.Date(ols_oos_eval_metrics_m_df$dates) + months(1)
  ols_oos_eval_metrics_m_df <- ols_oos_eval_metrics_m_df %>% dplyr::mutate(id = paste0(tickers,"-",dates), .before = tickers)

  glmnet_oos_eval_metrics_m_df <- glmnet_results@oos_testing_eval_metrics_m_xts@data %>% as.data.frame() %>% tibble::rownames_to_column(var = "dates") %>%
    dplyr::mutate(tickers = glmnet_results@backtest_identifier, .before = dates)
  glmnet_oos_eval_metrics_m_df$dates <- as.Date(glmnet_oos_eval_metrics_m_df$dates) + months(1)
  glmnet_oos_eval_metrics_m_df <- glmnet_oos_eval_metrics_m_df %>% dplyr::mutate(id = paste0(tickers,"-",dates), .before = tickers)


  rf_oos_eval_metrics_m_df <- rf_results@oos_testing_eval_metrics_m_xts@data %>% as.data.frame() %>% tibble::rownames_to_column(var = "dates") %>%
    dplyr::mutate(tickers = rf_results@backtest_identifier, .before = dates)
  rf_oos_eval_metrics_m_df$dates <- as.Date(rf_oos_eval_metrics_m_df$dates) + months(1)
  rf_oos_eval_metrics_m_df <- rf_oos_eval_metrics_m_df %>% dplyr::mutate(id = paste0(tickers,"-",dates), .before = tickers)

  #Consolidate
  expected_results <- dplyr::bind_rows(ols_oos_eval_metrics_m_df, glmnet_oos_eval_metrics_m_df, rf_oos_eval_metrics_m_df) %>% dplyr::arrange(id) %>% create_meta_dataframe()
  results <- derive_adapted_custom_signal_universe_m_df(
    meta_custom_objective = "max_rss",
    base_sb_backtest_results_list = list(ols_results, glmnet_results, rf_results),
    meta_custom_signal_universe_metrics_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL)

  expect_equal(results,expected_results)


  expect_true(
    mean((results@data$dates - zoo::index(ols_results@oos_testing_eval_metrics_m_xts@data)) - 30) %>% as.numeric() < 3)



})








































