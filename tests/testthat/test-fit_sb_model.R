test_that("fit_sb_model works for SW", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  training_sample_size <- 9
  dates_m_vector <- unique(as.Date(toy_preprocessed_features$dates, format = "%Y-%m-%d"))

  current_date <- dates_m_vector[9] #Current date (first date)

  #Target vector
  target_fwd_name <- "fwd_premium_3m"
  target_vector <- toy_preprocessed_targets[, target_fwd_name]

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data ##Change chosen_signals_and_positions and features_m_df
  signal_universe_m_df[5,2] <- "low_idio_vol_mrkt_ewma"
  signal_universe_m_df[5,1] <- "low_idio_vol_mrkt_ewma-2023-03-15"

  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-03-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  selected_features_corrected_positions_m_df <-
    select_and_correct_signals(signals_m_df = toy_preprocessed_features,
    chosen_signals_and_positions = chosen_signals_and_positions)$selected_signals_corrected_positions_m_df

  expect_equal(colnames(selected_features_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_features_corrected_positions_m_df$low_idio_vol_mrkt_ewma, toy_preprocessed_features$idio_vol_mrkt_ewma*-1)

  #Features m refit
   ts_splits <-
    time_series_split(current_date = current_date, features_m_df = selected_features_corrected_positions_m_df,
                      target_m_df = toy_preprocessed_targets, target_fwd = 3, target_fwd_name = target_fwd_name,
                      dates_m_vector = dates_m_vector,
                      training_sample_size = training_sample_size
                      )

  selected_features_corrected_positions_m_refit <- ts_splits$refit$features_m_refit
  target_m_refit <- ts_splits$refit$target_m_refit


  #custom_obj
  custom_objective <- "max_info_ratio"
  most_recent_signal_universe_m_d_ref[, "exp_ret_score"] <- signal_transform(most_recent_signal_universe_m_d_ref$info_ratio ,
                                                                             upper_quantile_winsorization = 0.95,
                                                                             lower_quantile_winsorization =  0.05)
  #Create port
  signal_port <- set_portfolio_weights(port_construction_method = "sw",
                                       universe_m_d_ref = most_recent_signal_universe_m_d_ref)

  #Fit SB Model
  results <- fit_sb_model(sb_algorithm = "sw", target_fwd_name = target_fwd_name,
                          selected_features_corrected_positions_m_refit = selected_features_corrected_positions_m_refit,
                          target_m_refit = target_m_refit,
                          custom_objective_translated = custom_objective,
                          huber_delta = 1, quantile_tau = .5, early_stop = NULL,
                          keras_architecture_parameters = NULL, chosen_eval_metric_translated = NULL,
                          most_recent_signal_universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                          selected_backtest_returns_corrected_positions_m_xts_upd_ref = NULL,
                          selected_cov_matrix_benchmark_m_xts_upd_ref = NULL,
                          concentration_constraint_policy = NULL
                          )

  expect_equal(results@model@universe_m_d_ref, signal_port@universe_m_d_ref)
  expect_equal(results@eligible_signals, most_recent_signal_universe_m_d_ref %>%
                 dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))


  #Predict!
  d_ref <- which(as.Date(toy_preprocessed_features$dates) == as.Date(current_date))
  target_vector_ref <- toy_preprocessed_targets[d_ref, "fwd_premium_3m"]
  selected_features_corrected_positions_m_d_ref <- selected_features_corrected_positions_m_df %>% dplyr::filter(dates == current_date)
  predictions <- as.numeric(as.matrix(selected_features_corrected_positions_m_d_ref[,-c(1:3)]) %*% signal_port@weights) %>%
    signal_transform(lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization)

  results_preds <- predict(results, new_features_m_df = selected_features_corrected_positions_m_d_ref)

  expect_equal(predictions, results_preds)

  #Predict again with last model
  current_date <- dates_m_vector[10]

  d_ref <- which(as.Date(toy_preprocessed_features$dates) == as.Date(current_date))
  target_vector_ref <- toy_preprocessed_targets[d_ref, "fwd_premium_3m"]
  selected_features_corrected_positions_m_d_ref <- selected_features_corrected_positions_m_df %>% dplyr::filter(dates == current_date)
  predictions <- as.numeric(as.matrix(selected_features_corrected_positions_m_d_ref[,-c(1:3)]) %*% signal_port@weights) %>%
    signal_transform(lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization)

  results_preds <- predict(results, new_features_m_df = selected_features_corrected_positions_m_d_ref)

  expect_equal(predictions, results_preds)

})

test_that("fit_sb_model works for RP", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  #Create backtest
  set.seed(123)
  mocked_backtest_returns_m_xts <- xts::xts(data.frame(
    book_yield = rnorm(13, 1, 1),
    eps_yield = rnorm(13, 1, 1),
    fcf_yield = rnorm(13, 1, 1),
    sharpe_6m = rnorm(13, 1, 1),
    roe_3m = rnorm(13, 1, 1),
    low_idio_vol_mrkt_ewma = rnorm(13, 1, 1)
  ), order.by = as.Date(unique(toy_preprocessed_features$dates), format = "%Y-%m-%d"))

  mocked_cov_matrix_benchmark_m_xts <- xts::xts(data.frame(IBOV = rnorm(13, 1, 1)),
                                                      order.by = as.Date(unique(toy_preprocessed_features$dates), format = "%Y-%m-%d"))

  training_sample_size <- 9
  dates_m_vector <- unique(as.Date(toy_preprocessed_features$dates, format = "%Y-%m-%d"))

  current_date <- dates_m_vector[9] #Current date (first date)

  #Target vector
  target_fwd_name <- "fwd_premium_3m"
  target_vector <- toy_preprocessed_targets[, target_fwd_name]

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data ##Change chosen_signals_and_positions and features_m_df
  signal_universe_m_df[5,2] <- "low_idio_vol_mrkt_ewma"
  signal_universe_m_df[5,1] <- "low_idio_vol_mrkt_ewma-2023-03-15"

  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-03-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  selected_and_corrected_list <- select_and_correct_signals(signals_m_df = toy_preprocessed_features,
                                                            chosen_signals_and_positions = chosen_signals_and_positions,
                                                            backtest_returns_m_xts = mocked_backtest_returns_m_xts)

  selected_features_corrected_positions_m_df <- selected_and_corrected_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_m_xts <- selected_and_corrected_list$selected_backtest_returns_corrected_positions_m_xts

  expect_equal(colnames(selected_features_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_features_corrected_positions_m_df$low_idio_vol_mrkt_ewma, toy_preprocessed_features$idio_vol_mrkt_ewma*-1)

  #Features m refit
  ts_splits <-
    time_series_split(current_date = current_date, features_m_df = selected_features_corrected_positions_m_df,
                      target_m_df = toy_preprocessed_targets, target_fwd = 3, target_fwd_name = target_fwd_name,
                      dates_m_vector = dates_m_vector,
                      training_sample_size = training_sample_size
    )

  selected_features_corrected_positions_m_refit <- ts_splits$refit$features_m_refit
  target_m_refit <- ts_splits$refit$target_m_refit
  selected_backtest_returns_m_xts_upd_ref <- selected_backtest_returns_m_xts[c(1:9), ]
  mocked_selected_cov_matrix_benchmark_m_xts_upd_ref <- mocked_cov_matrix_benchmark_m_xts[c(1:9), ]

  #Create portfoli

  signal_port <- set_portfolio_weights(port_construction_method = "rp",
                                       universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                                       returns_m_xts_upd_ref = selected_backtest_returns_m_xts_upd_ref,
                                       selected_benchmark_m_xts_upd_ref = mocked_selected_cov_matrix_benchmark_m_xts_upd_ref,
                                       active_returns = TRUE, cov_matrix_sample_size = 9, cov_estimation_method = "sample"
                                       )

  #Fit SB Model
  results <- fit_sb_model(sb_algorithm = "rp", target_fwd_name = target_fwd_name,
                          selected_features_corrected_positions_m_refit = selected_features_corrected_positions_m_refit,
                          target_m_refit = target_m_refit,
                          custom_objective_translated = custom_objective,
                          huber_delta = 1, quantile_tau = .5, early_stop = NULL,
                          groups_m_d_ref = NULL,
                          keras_architecture_parameters = NULL, chosen_eval_metric_translated = NULL,
                          most_recent_signal_universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                          selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_m_xts_upd_ref,
                          selected_cov_matrix_benchmark_m_xts_upd_ref = mocked_selected_cov_matrix_benchmark_m_xts_upd_ref,
                          active_returns = TRUE, cov_matrix_sample_size = 9, cov_estimation_method = "sample",
                          concentration_constraint_policy = NULL
  )

  expect_equal(results@model@universe_m_d_ref, signal_port@universe_m_d_ref)
  expect_equal(results@eligible_signals, most_recent_signal_universe_m_d_ref %>%
                 dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))


  #Predict!
  d_ref <- which(as.Date(toy_preprocessed_features$dates) == as.Date(current_date))
  target_vector_ref <- toy_preprocessed_targets[d_ref, "fwd_premium_3m"]
  selected_features_corrected_positions_m_d_ref <- selected_features_corrected_positions_m_df %>% dplyr::filter(dates == current_date)
  predictions <- as.numeric(as.matrix(selected_features_corrected_positions_m_d_ref[,-c(1:3)]) %*% signal_port@weights) %>%
    signal_transform(lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization)

  results_preds <- predict(results, new_features_m_df = selected_features_corrected_positions_m_d_ref)

  expect_equal(predictions, results_preds)

  #Predict again with last model
  current_date <- dates_m_vector[10]

  d_ref <- which(as.Date(toy_preprocessed_features$dates) == as.Date(current_date))
  target_vector_ref <- toy_preprocessed_targets[d_ref, "fwd_premium_3m"]
  selected_features_corrected_positions_m_d_ref <- selected_features_corrected_positions_m_df %>% dplyr::filter(dates == current_date)
  predictions <- as.numeric(as.matrix(selected_features_corrected_positions_m_d_ref[,-c(1:3)]) %*% signal_port@weights) %>%
    signal_transform(lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization)

  results_preds <- predict(results, new_features_m_df = selected_features_corrected_positions_m_d_ref)

  expect_equal(predictions, results_preds)

})

test_that("fit_sb_model works for OLS", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  training_sample_size <- 9
  dates_m_vector <- unique(as.Date(toy_preprocessed_features$dates, format = "%Y-%m-%d"))

  current_date <- dates_m_vector[9] #Current date (first date)

  #Target vector
  target_fwd_name <- "fwd_premium_3m"
  target_vector <- toy_preprocessed_targets[, target_fwd_name]

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data ##Change chosen_signals_and_positions and features_m_df
  signal_universe_m_df[5,2] <- "low_idio_vol_mrkt_ewma"
  signal_universe_m_df[5,1] <- "low_idio_vol_mrkt_ewma-2023-03-15"

  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-03-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  selected_features_corrected_positions_m_df <-
    select_and_correct_signals(signals_m_df = toy_preprocessed_features,
                               chosen_signals_and_positions = chosen_signals_and_positions)$selected_signals_corrected_positions_m_df

  expect_equal(colnames(selected_features_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_features_corrected_positions_m_df$low_idio_vol_mrkt_ewma, toy_preprocessed_features$idio_vol_mrkt_ewma*-1)

  #Features m refit
  ts_splits <-
    time_series_split(current_date = current_date, features_m_df = selected_features_corrected_positions_m_df,
                      target_m_df = toy_preprocessed_targets, target_fwd = 3, target_fwd_name = target_fwd_name,
                      dates_m_vector = dates_m_vector,
                      training_sample_size = training_sample_size
    )

  selected_features_corrected_positions_m_refit <- ts_splits$refit$features_m_refit
  target_m_refit <- ts_splits$refit$target_m_refit
  selected_full_data_corrected_positions_m_refit_clean <- ts_splits$refit$full_data_m_refit_clean

  #RUN OLS
  lm_model <- lm(fwd_premium_3m ~ ., data = selected_full_data_corrected_positions_m_refit_clean)

  #Fit SB Model
  results <- fit_sb_model(sb_algorithm = "ols", target_fwd_name = target_fwd_name,
                          selected_features_corrected_positions_m_refit = selected_features_corrected_positions_m_refit,
                          target_m_refit = target_m_refit,
                          selected_full_data_corrected_positions_m_refit_clean = selected_full_data_corrected_positions_m_refit_clean,
                          custom_objective_translated = custom_objective,
                          huber_delta = 1, quantile_tau = .5, early_stop = NULL,
                          keras_architecture_parameters = NULL, chosen_eval_metric_translated = NULL,
                          most_recent_signal_universe_m_d_ref = most_recent_signal_universe_m_d_ref,
                          selected_backtest_returns_corrected_positions_m_xts_upd_ref = NULL,
                          selected_cov_matrix_benchmark_m_xts_upd_ref = NULL,
                          concentration_constraint_policy = NULL
  )

  expect_equal(coef(results@model), coef(lm_model))


  #Predict!
  d_ref <- which(as.Date(toy_preprocessed_features$dates) == as.Date(current_date))
  target_vector_ref <- toy_preprocessed_targets[d_ref, "fwd_premium_3m"]
  selected_features_corrected_positions_m_d_ref <- selected_features_corrected_positions_m_df %>% dplyr::filter(dates == current_date)
  predictions <- predict(lm_model, newdata = selected_features_corrected_positions_m_d_ref)

  results_preds <- predict(results, new_features_m_df = selected_features_corrected_positions_m_d_ref)

  expect_equal(as.numeric(predictions), results_preds)

  #Predict again with last model
  current_date <- dates_m_vector[10]

  d_ref <- which(as.Date(toy_preprocessed_features$dates) == as.Date(current_date))
  target_vector_ref <- toy_preprocessed_targets[d_ref, "fwd_premium_3m"]
  selected_features_corrected_positions_m_d_ref <- selected_features_corrected_positions_m_df %>% dplyr::filter(dates == current_date)
  predictions <- predict(lm_model, newdata = selected_features_corrected_positions_m_d_ref) %>% as.numeric()

  results_preds <- predict(results, new_features_m_df = selected_features_corrected_positions_m_d_ref)

  expect_equal(predictions, results_preds)

})









