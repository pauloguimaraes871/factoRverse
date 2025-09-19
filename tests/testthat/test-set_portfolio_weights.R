#Signals
test_that("set portfolio weights work for Custom Weights (signals)", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))

  current_date <- "2023-03-15"

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data
  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-03-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref$tickers
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  select_and_correct_signals_backtest_list <- select_and_correct_signals(signals_m_df = signals_m_df@data,
                                                                         chosen_signals_and_positions = chosen_signals_and_positions,
                                                                         backtest_returns_m_xts = mocked_backtest_returns_m_xts)

  selected_signals_corrected_positions_m_df <- select_and_correct_signals_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- select_and_correct_signals_backtest_list$selected_backtest_returns_corrected_positions_m_xts

  expect_equal(colnames(selected_signals_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_signals_corrected_positions_m_df$low_vol_36m, signals_m_df@data$vol_36m*-1)


  #ts_splits
  features_m_refit <- signals_m_df@data %>% dplyr::filter(dates <= "2023-03-15") #this is to mimick ts_split behavior with a target_fwd of 3


  #Create custom weights according to theme_sb
  most_recent_signal_universe_m_d_ref$weights <- most_recent_signal_universe_m_d_ref$theme_sb_bench_weights

  theme_sb_bench_weights_m_df <- most_recent_signal_universe_m_d_ref %>% dplyr::select(id, tickers, dates, theme_sb_bench_weights) %>% dplyr::rename(weights = theme_sb_bench_weights)

  results <- set_portfolio_weights(port_construction_method = "custom_weights",
                                   universe_m_d_ref = most_recent_signal_universe_m_d_ref %>% dplyr::select(-weights),
                                   custom_weights_m_d_ref = theme_sb_bench_weights_m_df
  )

  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id), results@universe_m_d_ref@data)
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(theme_sb_bench_weights > 0) %>% dplyr::pull(tickers))
  expect_equal(results@weights, most_recent_signal_universe_m_d_ref %>% dplyr::filter(theme_sb_bench_weights > 0) %>% dplyr::pull(weights))

  #Create custom weights according to theme_ss
  most_recent_signal_universe_m_d_ref$weights <- most_recent_signal_universe_m_d_ref$theme_ss_bench_weights

  theme_ss_bench_weights_m_df <- most_recent_signal_universe_m_d_ref %>% dplyr::select(id, tickers, dates, theme_ss_bench_weights) %>% dplyr::rename(weights = theme_ss_bench_weights)

  results <- set_portfolio_weights(port_construction_method = "custom_weights",
                                   universe_m_d_ref = most_recent_signal_universe_m_d_ref %>% dplyr::select(-weights),
                                   custom_weights_m_d_ref = theme_ss_bench_weights_m_df
  )

  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id), results@universe_m_d_ref@data)
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(theme_ss_bench_weights > 0) %>% dplyr::pull(tickers))
  expect_equal(results@weights, most_recent_signal_universe_m_d_ref %>% dplyr::filter(theme_ss_bench_weights > 0) %>% dplyr::pull(weights))


})

test_that("set portfolio weights work for EW (signals)", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))

  current_date <- "2023-06-15"

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data
  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-06-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref$tickers
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  select_and_correct_signals_backtest_list <- select_and_correct_signals(signals_m_df = signals_m_df@data,
                                                                         chosen_signals_and_positions = chosen_signals_and_positions,
                                                                         backtest_returns_m_xts = mocked_backtest_returns_m_xts)

  selected_signals_corrected_positions_m_df <- select_and_correct_signals_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- select_and_correct_signals_backtest_list$selected_backtest_returns_corrected_positions_m_xts

  expect_equal(colnames(selected_signals_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_signals_corrected_positions_m_df$low_vol_36m, signals_m_df@data$vol_36m*-1)


  #ts_splits
  features_m_refit <- signals_m_df@data %>% dplyr::filter(dates <= "2023-03-15") #this is to mimick ts_split behavior with a target_fwd of 3

  most_recent_signal_universe_m_d_ref$weights <- rep(0.20, 5)

  most_recent_signal_universe_m_d_ref <- most_recent_signal_universe_m_d_ref

  cov_matrix <- cov(selected_backtest_returns_corrected_positions_m_xts[1:9,])

  #RRC
  rrc <- relative_risk_contribution(most_recent_signal_universe_m_d_ref$weights, cov_matrix)
  most_recent_signal_universe_m_d_ref$rel_risk_contr <- rrc$rel_risk_contr
  most_recent_signal_universe_m_d_ref <- most_recent_signal_universe_m_d_ref %>% dplyr::relocate(rel_risk_contr, .before = weights)

  results <- set_portfolio_weights(port_construction_method = "ew", universe_m_d_ref = most_recent_signal_universe_m_d_ref %>% dplyr::select(-weights, -rel_risk_contr),
                                   returns_m_xts_upd_ref = mocked_backtest_returns_m_xts[1:9,]
  )

  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id), results@universe_m_d_ref@data)
  expect_equal(results@groups, NULL)
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@groups, NULL)
  expect_equal(results@exp_ret_score, NULL)
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@covariance_matrix, cov_matrix)
  expect_equal(results@rel_risk_contr, rrc$rel_risk_contr)

})

test_that("set portfolio weights work for SW (signals) ", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))

  current_date <- "2023-06-15"

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data
  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-06-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref$tickers
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  selected_signals_corrected_positions_m_df <- select_and_correct_signals(signals_m_df = signals_m_df@data,
                                                                          chosen_signals_and_positions = chosen_signals_and_positions)$selected_signals_corrected_positions_m_df

  expect_equal(colnames(selected_signals_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_signals_corrected_positions_m_df$low_vol_36m, signals_m_df@data$vol_36m*-1)


  #ts_splits
  features_m_refit <- signals_m_df@data %>% dplyr::filter(dates <= "2023-03-15") #this is to mimick ts_split behavior with a target_fwd of 3

  #custom_obj
  custom_objective <- "max_info_ratio"
  weights <- signal_transform(most_recent_signal_universe_m_d_ref$info_ratio, upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05)/sum(signal_transform(most_recent_signal_universe_m_d_ref$info_ratio, upper_quantile_winsorization = 0.95, lower_quantile_winsorization =  0.05))

  most_recent_signal_universe_m_d_ref[, "exp_ret_score"] <- signal_transform(most_recent_signal_universe_m_d_ref$info_ratio ,
                                                                             upper_quantile_winsorization = 0.95,
                                                                             lower_quantile_winsorization =  0.05)
  most_recent_signal_universe_m_d_ref$weights <- weights

  results <- set_portfolio_weights(port_construction_method = "sw", universe_m_d_ref = most_recent_signal_universe_m_d_ref %>% dplyr::select(-weights))


  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id), results@universe_m_d_ref@data)

  expect_equal(results@groups, NULL)
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@groups, NULL)
  expect_equal(results@exp_ret_score, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@covariance_matrix, NULL)
  expect_equal(results@rel_risk_contr, NULL)



  #custom_obj
  custom_objective <- "min_track_err"
  weights <- signal_transform(most_recent_signal_universe_m_d_ref$track_err*-1 , upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05)/sum(signal_transform(most_recent_signal_universe_m_d_ref$track_err*-1 , upper_quantile_winsorization = 0.95, lower_quantile_winsorization =  0.05))

  most_recent_signal_universe_m_d_ref[, "exp_ret_score"] <- signal_transform(most_recent_signal_universe_m_d_ref$track_err*-1 , upper_quantile_winsorization = 0.95,  lower_quantile_winsorization = 0.05)
  most_recent_signal_universe_m_d_ref$weights <- weights

  results <- set_portfolio_weights(port_construction_method = "sw", universe_m_d_ref = most_recent_signal_universe_m_d_ref %>% dplyr::select(-weights))

  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id),results@universe_m_d_ref@data)
  expect_equal(results@groups, NULL)
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@groups, NULL)
  expect_equal(results@exp_ret_score, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@covariance_matrix, NULL)
  expect_equal(results@rel_risk_contr, NULL)

})

test_that("set portfolio weights work for RP (signals) ", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))

  current_date <- "2023-06-15"

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data
  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-06-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref$tickers
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df@data, chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_m_xts = mocked_backtest_returns_m_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_cov_matrix_benchmark_m_xts <- benchmark_returns_m_xts[, "IBOV"]

  expect_equal(colnames(selected_signals_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_signals_corrected_positions_m_df$low_vol_36m, signals_m_df@data$vol_36m*-1)

  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:9),]
  selected_cov_matrix_benchmark_m_xts_upd_ref <- selected_cov_matrix_benchmark_m_xts[c(1:9), ]

  #ts_splits
  features_m_refit <- signals_m_df@data %>% dplyr::filter(dates <= "2023-03-15") #this is to mimick ts_split behavior with a target_fwd of 3

  #custom_obj
  custom_objective <- "max_info_ratio"


  #Calculate a sample cov active matrix
  selected_backtest_returns_corrected_positions_m_xts_upd_ref_active <-
    apply(selected_backtest_returns_corrected_positions_m_xts_upd_ref, 2, function(x){
      ((1+x/100)/(1+as.numeric(selected_cov_matrix_benchmark_m_xts_upd_ref)/100) - 1)*100
    })

  rp <- riskParityPortfolio::riskParityPortfolio(Sigma = cov(selected_backtest_returns_corrected_positions_m_xts_upd_ref_active))


  weights <- rp$w
  rrc <- relative_risk_contribution(rp$w, cov(selected_backtest_returns_corrected_positions_m_xts_upd_ref_active))
  expect_equal(rrc$rel_risk_contr, as.numeric(rp$relative_risk_contribution))
  expect_equal(rrc$tickers, names(rp$relative_risk_contribution))

  most_recent_signal_universe_m_d_ref$weights <- weights
  most_recent_signal_universe_m_d_ref <- dplyr::left_join(most_recent_signal_universe_m_d_ref, rrc, by = "tickers") %>%
    dplyr::relocate(rel_risk_contr, .before = weights)

  results <- set_portfolio_weights(port_construction_method = "rp", universe_m_d_ref = most_recent_signal_universe_m_d_ref %>%
                                     dplyr::select(-rel_risk_contr, -weights),
                                   cov_matrix_sample_size = 9, cov_estimation_method = "sample", groups_m_d_ref = NULL, active_returns = TRUE,
                                   returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                   selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref
  )

  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id), results@universe_m_d_ref@data)
  expect_equal(results@groups, NULL)
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@groups, NULL)
  expect_equal(results@exp_ret_score, NULL)
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@covariance_matrix, cov(selected_backtest_returns_corrected_positions_m_xts_upd_ref_active))
  expect_equal(results@rel_risk_contr, rrc$rel_risk_contr)


})

test_that("set portfolio weights work for RP + exp_ret_score_tilt ('inner') (signals)", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))

  current_date <- "2023-06-15"

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data
  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-06-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref$tickers
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df@data, chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_m_xts = mocked_backtest_returns_m_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_cov_matrix_benchmark_m_xts <- benchmark_returns_m_xts[, "IBOV"]

  expect_equal(colnames(selected_signals_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_signals_corrected_positions_m_df$low_vol_36m, signals_m_df@data$vol_36m*-1)

  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:9),]
  selected_cov_matrix_benchmark_m_xts_upd_ref <- selected_cov_matrix_benchmark_m_xts[c(1:9), ]

  #ts_splits
  features_m_refit <- signals_m_df@data %>% dplyr::filter(dates <= "2023-03-15") #this is to mimick ts_split behavior with a target_fwd of 3

  #custom_obj
  custom_objective <- "max_info_ratio"

  #Calculate a sample cov active matrix
  selected_backtest_returns_corrected_positions_m_xts_upd_ref_active <-
    apply(selected_backtest_returns_corrected_positions_m_xts_upd_ref, 2, function(x){
      ((1+x/100)/(1+as.numeric(selected_cov_matrix_benchmark_m_xts_upd_ref)/100) - 1)*100
    })

  #exp ret score
  most_recent_signal_universe_m_d_ref[, "exp_ret_score"] <- signal_transform(most_recent_signal_universe_m_d_ref$info_ratio ,
                                                                             upper_quantile_winsorization = 0.95,
                                                                             lower_quantile_winsorization =  0.05)

  #RP
  exp_ret_score <- most_recent_signal_universe_m_d_ref$exp_ret_score
  exp_ret_score_tilt_eta <- 0.5
  exp_ret_score_tilt <- "inner"

  cov_matrix <- cov(selected_backtest_returns_corrected_positions_m_xts_upd_ref_active)
  rp <- riskParityPortfolio::riskParityPortfolio(Sigma = cov_matrix,
                                                 lmd_mu = exp_ret_score_tilt_eta,
                                                 mu = exp_ret_score)

  #Relative Risk Contribution and Weights from rp
  most_recent_signal_universe_m_d_ref$rel_risk_contr <- rp$relative_risk_contribution
  most_recent_signal_universe_m_d_ref$weights <- rp$w

  exp_ret_score <- most_recent_signal_universe_m_d_ref$exp_ret_score

  #Set portfolio weights
  results <- set_portfolio_weights(port_construction_method = "rp",
                                   universe_m_d_ref = most_recent_signal_universe_m_d_ref %>%
                                     dplyr::select(-weights, -rel_risk_contr),
                                   cov_matrix_sample_size = 9, cov_estimation_method = "sample", groups_m_d_ref = NULL, active_returns = TRUE,
                                   exp_ret_score_tilt = exp_ret_score_tilt, exp_ret_score_tilt_eta,
                                   returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                   selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                   exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                                   exp_ret_score = exp_ret_score
  )

  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id), results@universe_m_d_ref@data)

  #Rel Risk Contribution is not the same across rows
  expect_true(all(
    (most_recent_signal_universe_m_d_ref$rel_risk_contr != most_recent_signal_universe_m_d_ref$rel_risk_contr[1])[-1] #First is obviously TRUE
    ))

  #Test that in relation to vanilla rp, there is a bias towards high exp_ret_score
  rp_vanilla <- riskParityPortfolio::riskParityPortfolio(Sigma = cov_matrix)
  weights_vanilla <- rp_vanilla$w
  weight_diff <- results@weights - weights_vanilla

  #Check that weight_diff is higher for signals with exp_ret_score above median
  exp_ret_score_median <- median(most_recent_signal_universe_m_d_ref$exp_ret_score)
  weight_diff_above_median <- weight_diff[which(most_recent_signal_universe_m_d_ref$exp_ret_score > exp_ret_score_median)]
  weight_diff_below_median <- weight_diff[which(most_recent_signal_universe_m_d_ref$exp_ret_score <= exp_ret_score_median)]
  expect_true(mean(weight_diff_above_median) > mean(weight_diff_below_median))



})

test_that("set portfolio weights work for HRP (signals)", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))

  current_date <- "2023-06-15"

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data
  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-06-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref$tickers
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df@data, chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_m_xts = mocked_backtest_returns_m_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_cov_matrix_benchmark_m_xts <- benchmark_returns_m_xts[, "IBOV"]

  expect_equal(colnames(selected_signals_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_signals_corrected_positions_m_df$low_vol_36m, signals_m_df@data$vol_36m*-1)

  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:9),]
  selected_cov_matrix_benchmark_m_xts_upd_ref <- selected_cov_matrix_benchmark_m_xts[c(1:9), ]

  #ts_splits
  features_m_refit <- signals_m_df@data %>% dplyr::filter(dates <= "2023-03-15") #this is to mimick ts_split behavior with a target_fwd of 3

  #custom_obj
  custom_objective <- "max_info_ratio"


  #Calculate a sample cov active matrix
  selected_backtest_returns_corrected_positions_m_xts_upd_ref_active <-
    apply(selected_backtest_returns_corrected_positions_m_xts_upd_ref, 2, function(x){
      ((1+x/100)/(1+as.numeric(selected_cov_matrix_benchmark_m_xts_upd_ref)/100) - 1)*100
    })

  covariance_matrix <- cov(selected_backtest_returns_corrected_positions_m_xts_upd_ref_active)
  correlation_matrix <- stats::cov2cor(covariance_matrix)

  distance <- sqrt(0.5 * (1 - correlation_matrix))
  euclidean_distance <- stats::dist(distance, method = "euclidean", diag = TRUE, upper = TRUE, p = 2)
  hc <- stats::hclust(euclidean_distance, method = "single", member = NULL)
  hc_order <- hc$order
  Sigma_ord <- covariance_matrix[hc_order, hc_order]

  # Bisection algorithm
  w <- rep(1, ncol(covariance_matrix))

  #Start at top level
  index_L <- c(1,2)
  index_R <- c(3,4,5)
  cov_L <- Sigma_ord[index_L, index_L, drop = FALSE]
  cov_R <- Sigma_ord[index_R, index_R, drop = FALSE]
  w_L <- 1/diag(cov_L)/sum(1/diag(cov_L))
  w_R <- 1/diag(cov_R)/sum(1/diag(cov_R))
  var_L <- as.numeric(t(w_L) %*% cov_L %*% w_L)
  var_R <- as.numeric(t(w_R) %*% cov_R %*% w_R)
  alpha <- 1 - (var_L/(var_L + var_R))
  w[index_L] <- w[index_L] * alpha
  w[index_R] <- w[index_R] * (1 - alpha)

  #1
  index_L1 <- c(1)
  index_R1 <- c(2)
  cov_L1 <- Sigma_ord[index_L1, index_L1, drop = FALSE]
  cov_R1 <- Sigma_ord[index_R1, index_R1, drop = FALSE]
  var_L1 <- as.numeric(cov_L1)
  var_R1 <- as.numeric(cov_R1)
  alpha1 <- 1 - (var_L1/(var_L1 + var_R1))
  w[index_L1] <- w[index_L1] * alpha1
  w[index_R1] <- w[index_R1] * (1 - alpha1)

  #2
  index_L2 <- c(3)
  index_R2 <- c(4,5)
  cov_L2 <- Sigma_ord[index_L2, index_L2, drop = FALSE]
  cov_R2 <- Sigma_ord[index_R2, index_R2, drop = FALSE]
  w_R2 <- 1/diag(cov_R2)/sum(1/diag(cov_R2))
  var_L2 <- as.numeric(cov_L2)
  var_R2 <- as.numeric(t(w_R2) %*% cov_R2 %*% w_R2)
  alpha2 <- 1 - (var_L2/(var_L2 + var_R2))
  w[index_L2] <- w[index_L2] * alpha2
  w[index_R2] <- w[index_R2] * (1 - alpha2)

  #3
  index_L3 <- c(4)
  index_R3 <- c(5)
  cov_L3 <- Sigma_ord[index_L3, index_L3, drop = FALSE]
  cov_R3 <- Sigma_ord[index_R3, index_R3, drop = FALSE]
  var_L3 <- as.numeric(cov_L3)
  var_R3 <- as.numeric(cov_R3)
  alpha3 <- 1 - (var_L3/(var_L3 + var_R3))
  w[index_L3] <- w[index_L3] * alpha3
  w[index_R3] <- w[index_R3] * (1 - alpha3)
  names(w) <- colnames(covariance_matrix)[hc_order]

  #Add rel risk contr
  rel_risk_contr <- relative_risk_contribution(w[colnames(covariance_matrix)], covariance_matrix)
  most_recent_signal_universe_m_d_ref$rel_risk_contr <- rel_risk_contr$rel_risk_contr
  most_recent_signal_universe_m_d_ref$weights <- w[colnames(covariance_matrix)]

  results <- set_portfolio_weights(port_construction_method = "hrp",
                                   universe_m_d_ref = most_recent_signal_universe_m_d_ref %>%
                                     dplyr::select(-rel_risk_contr, -weights),
                                   cov_matrix_sample_size = 9, cov_estimation_method = "sample", groups_m_d_ref = NULL, active_returns = TRUE,
                                   returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                   selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref
  )

  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id), results@universe_m_d_ref@data)
  expect_equal(results@groups, NULL)
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@groups, NULL)
  expect_equal(results@exp_ret_score, NULL)
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@covariance_matrix, cov(selected_backtest_returns_corrected_positions_m_xts_upd_ref_active))
  expect_equal(results@rel_risk_contr, rel_risk_contr$rel_risk_contr)
  expect_equal(results@clusters$order, hc$order)
  expect_equal(results@clusters$height, hc$height)
  expect_equal(results@clusters$merge, hc$merge)
  expect_equal(results@clusters$labels, hc$labels)


})

test_that("set portfolio weights work for HRP + exp_ret_score_tilt ('inner') (signals)", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))

  current_date <- "2023-06-15"

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data
  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-06-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref$tickers
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df@data, chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_m_xts = mocked_backtest_returns_m_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_cov_matrix_benchmark_m_xts <- benchmark_returns_m_xts[, "IBOV"]

  expect_equal(colnames(selected_signals_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_signals_corrected_positions_m_df$low_vol_36m, signals_m_df@data$vol_36m*-1)

  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:9),]
  selected_cov_matrix_benchmark_m_xts_upd_ref <- selected_cov_matrix_benchmark_m_xts[c(1:9), ]

  #ts_splits
  features_m_refit <- signals_m_df@data %>% dplyr::filter(dates <= "2023-03-15") #this is to mimick ts_split behavior with a target_fwd of 3

  #custom_obj
  custom_objective <- "min_act_dd_dev"


  #Calculate an ewma cov active matrix
  selected_backtest_returns_corrected_positions_m_xts_upd_ref_active <-
    apply(selected_backtest_returns_corrected_positions_m_xts_upd_ref, 2, function(x){
      ((1+x/100)/(1+as.numeric(selected_cov_matrix_benchmark_m_xts_upd_ref)/100) - 1)*100
    })

  covariance_matrix <- PerformanceAnalytics::M2.ewma(selected_backtest_returns_corrected_positions_m_xts_upd_ref_active)

  #exp_ret_score
  exp_ret_score <- most_recent_signal_universe_m_d_ref$act_dd_dev*-1
  exp_ret_score <- exp_ret_score %>% signal_transform(0.05, 0.95)
  exp_ret_score_tilt_eta <- 0.5
  exp_ret_score_tilt <- "inner"
  most_recent_signal_universe_m_d_ref$exp_ret_score <- exp_ret_score

  correlation_matrix <- stats::cov2cor(covariance_matrix)

  distance <- sqrt(0.5 * (1 - correlation_matrix))
  euclidean_distance <- stats::dist(distance, method = "euclidean", diag = TRUE, upper = TRUE, p = 2)
  hc <- stats::hclust(euclidean_distance, method = "single", member = NULL)
  hc_order <- hc$order
  Sigma_ord <- covariance_matrix[hc_order, hc_order]

  # Bisection algorithm
  w <- rep(1, ncol(covariance_matrix))
  exp_ret_score <- most_recent_signal_universe_m_d_ref$exp_ret_score[hc_order]
  names(exp_ret_score) <- colnames(covariance_matrix)[hc_order]

  #Start at top level
  index_L <- c(1,2)
  index_R <- c(3,4,5)
  cov_L <- Sigma_ord[index_L, index_L, drop = FALSE]
  cov_R <- Sigma_ord[index_R, index_R, drop = FALSE]
  w_L <- 1/diag(cov_L)/sum(1/diag(cov_L))
  w_R <- 1/diag(cov_R)/sum(1/diag(cov_R))
  var_L <- as.numeric(t(w_L) %*% cov_L %*% w_L)
  var_R <- as.numeric(t(w_R) %*% cov_R %*% w_R)
  mu_L <- sum(w_L * exp_ret_score[index_L])
  mu_R <- sum(w_R * exp_ret_score[index_R])
  rank <- c(mu_L, mu_R) %>% rank(ties.method = "average")
  gL <- rank[1]/2
  gR <- rank[2]/2
  A_L <- (1/var_L)*(gL^exp_ret_score_tilt_eta)
  A_R <- (1/var_R)*(gR^exp_ret_score_tilt_eta)
  alpha <- (A_L/(A_L + A_R))
  w[index_L] <- w[index_L] * alpha
  w[index_R] <- w[index_R] * (1 - alpha)

  #1
  index_L1 <- c(1)
  index_R1 <- c(2)
  cov_L1 <- Sigma_ord[index_L1, index_L1, drop = FALSE]
  cov_R1 <- Sigma_ord[index_R1, index_R1, drop = FALSE]
  var_L1 <- as.numeric(cov_L1)
  var_R1 <- as.numeric(cov_R1)
  mu_L1 <- exp_ret_score[index_L1]
  mu_R1 <- exp_ret_score[index_R1]
  rank1 <- c(mu_L1, mu_R1) %>% rank(ties.method = "average")
  gL1 <- rank1[1]/2
  gR1 <- rank1[2]/2
  A_L1 <- (1/var_L1)*(gL1^exp_ret_score_tilt_eta)
  A_R1 <- (1/var_R1)*(gR1^exp_ret_score_tilt_eta)
  alpha1 <- (A_L1/(A_L1 + A_R1))
  w[index_L1] <- w[index_L1] * alpha1
  w[index_R1] <- w[index_R1] * (1 - alpha1)

  #2
  index_L2 <- c(3)
  index_R2 <- c(4,5)
  cov_L2 <- Sigma_ord[index_L2, index_L2, drop = FALSE]
  cov_R2 <- Sigma_ord[index_R2, index_R2, drop = FALSE]
  w_R2 <- 1/diag(cov_R2)/sum(1/diag(cov_R2))
  var_L2 <- as.numeric(cov_L2)
  var_R2 <- as.numeric(t(w_R2) %*% cov_R2 %*% w_R2)
  mu_L2 <- exp_ret_score[index_L2]
  mu_R2 <- sum(w_R2 * exp_ret_score[index_R2])
  rank2 <- c(mu_L2, mu_R2) %>% rank(ties.method = "average")
  gL2 <- rank2[1]/2
  gR2 <- rank2[2]/2
  A_L2 <- (1/var_L2)*(gL2^exp_ret_score_tilt_eta)
  A_R2 <- (1/var_R2)*(gR2^exp_ret_score_tilt_eta)
  alpha2 <- (A_L2/(A_L2 + A_R2))
  w[index_L2] <- w[index_L2] * alpha2
  w[index_R2] <- w[index_R2] * (1 - alpha2)

  #3
  index_L3 <- c(4)
  index_R3 <- c(5)
  cov_L3 <- Sigma_ord[index_L3, index_L3, drop = FALSE]
  cov_R3 <- Sigma_ord[index_R3, index_R3, drop = FALSE]
  var_L3 <- as.numeric(cov_L3)
  var_R3 <- as.numeric(cov_R3)
  mu_L3 <- exp_ret_score[index_L3]
  mu_R3 <- exp_ret_score[index_R3]
  rank3 <- c(mu_L3, mu_R3) %>% rank(ties.method = "average")
  gL3 <- rank3[1]/2
  gR3 <- rank3[2]/2
  A_L3 <- (1/var_L3)*(gL3^exp_ret_score_tilt_eta)
  A_R3 <- (1/var_R3)*(gR3^exp_ret_score_tilt_eta)
  alpha3 <- (A_L3/(A_L3 + A_R3))
  w[index_L3] <- w[index_L3] * alpha3
  w[index_R3] <- w[index_R3] * (1 - alpha3)

  names(w) <- colnames(covariance_matrix)[hc_order]

  #Add rel risk contr
  rel_risk_contr <- relative_risk_contribution(w[colnames(covariance_matrix)], covariance_matrix)
  most_recent_signal_universe_m_d_ref$rel_risk_contr <- rel_risk_contr$rel_risk_contr
  most_recent_signal_universe_m_d_ref$weights <- w[colnames(covariance_matrix)]

  results <- set_portfolio_weights(port_construction_method = "hrp",
                                   universe_m_d_ref = most_recent_signal_universe_m_d_ref %>%
                                     dplyr::select(-rel_risk_contr, -weights),
                                   exp_ret_score_tilt = exp_ret_score_tilt, exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                                   cov_matrix_sample_size = 9, cov_estimation_method = "ewma", groups_m_d_ref = NULL, active_returns = TRUE,
                                   returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                   selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref
  )

  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id), results@universe_m_d_ref@data)
  expect_equal(results@groups, NULL)
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@groups, NULL)
  expect_equal(results@exp_ret_score, NULL)
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@covariance_matrix, covariance_matrix)
  expect_equal(results@rel_risk_contr, rel_risk_contr$rel_risk_contr)
  expect_equal(results@clusters$order, hc$order)
  expect_equal(results@clusters$height, hc$height)
  expect_equal(results@clusters$merge, hc$merge)
  expect_equal(results@clusters$labels, hc$labels)


  #Test that results have a bias towards assets with high exp_ret_score
  results_alt <- set_portfolio_weights(port_construction_method = "hrp",
                                   universe_m_d_ref = most_recent_signal_universe_m_d_ref %>%
                                     dplyr::select(-rel_risk_contr, -weights),
                                   exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
                                   cov_matrix_sample_size = 9, cov_estimation_method = "ewma", groups_m_d_ref = NULL, active_returns = TRUE,
                                   returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                   selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref
  )

  ## Compare weight difference between results and results_alt and see if results has higher weights for assets with higher exp_ret_score
  weight_diff <- results@universe_m_d_ref@data$weights - results_alt@universe_m_d_ref@data$weights
  exp_ret_score <- most_recent_signal_universe_m_d_ref$exp_ret_score

  # See if difference is positive for assets with above median exp_ret_score and negative for those below median
  median_exp_ret_score <- median(exp_ret_score)
  above_median <- exp_ret_score > median_exp_ret_score
  below_median <- exp_ret_score <= median_exp_ret_score
  expect_true(sum(weight_diff[above_median]) >= 0)  # Weights should be higher for above median exp_ret_score

})

test_that("set portfolio weights work for MVO (signals) - unconstrained", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))

  current_date <- "2023-06-15"

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data
  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-06-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref$tickers
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df@data, chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_m_xts = mocked_backtest_returns_m_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_cov_matrix_benchmark_m_xts <- benchmark_returns_m_xts[, "IBOV"]

  expect_equal(colnames(selected_signals_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_signals_corrected_positions_m_df$low_vol_36m, signals_m_df@data$vol_36m*-1)

  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:9),]
  selected_cov_matrix_benchmark_m_xts_upd_ref <- selected_cov_matrix_benchmark_m_xts[c(1:9), ]

  #ts_splits
  features_m_refit <- signals_m_df@data %>% dplyr::filter(dates <= "2023-03-15") #this is to mimick ts_split behavior with a target_fwd of 3

  #custom_obj
  custom_objective <- "min_act_dd_dev"


  #Calculate an ewma cov active matrix
  selected_backtest_returns_corrected_positions_m_xts_upd_ref_active <-
    apply(selected_backtest_returns_corrected_positions_m_xts_upd_ref, 2, function(x){
      ((1+x/100)/(1+as.numeric(selected_cov_matrix_benchmark_m_xts_upd_ref)/100) - 1)*100
    })

  ewma_cov <- PerformanceAnalytics::M2.ewma(selected_backtest_returns_corrected_positions_m_xts_upd_ref_active)

  #exp_ret_score
  exp_ret_score <- most_recent_signal_universe_m_d_ref$act_dd_dev*-1
  exp_ret_score <- exp_ret_score %>% signal_transform(0.05, 0.95)
  most_recent_signal_universe_m_d_ref$exp_ret_score <- exp_ret_score

  #Port Spec
  port_spec <- PortfolioAnalytics::portfolio.spec(assets = most_recent_signal_universe_m_d_ref$tickers)
  port_spec <- PortfolioAnalytics::add.constraint(port_spec, type = "full_investment")
  port_spec <- PortfolioAnalytics::add.constraint(port_spec, type = "box")

  set.seed(123)
  random_weights <- PortfolioAnalytics::random_portfolios(
    portfolio = port_spec,
    permutations = 2000,
    "sample"
  )



  #Expected returns
  returns <- random_weights %>% apply(1, function(row){
    sum(row * exp_ret_score)
  })

  #Expected risk
  risk <- random_weights %>% apply(1, function(row){
    sqrt(t(as.matrix(row)) %*% ewma_cov %*% as.matrix(row))
  })

  #sharpe
  sharpe = returns/risk
  opt_w <- random_weights[which.max(sharpe),]
  rrc <- relative_risk_contribution(opt_w, ewma_cov)
  most_recent_signal_universe_m_d_ref$rel_risk_contr <- rrc$rel_risk_contr
  most_recent_signal_universe_m_d_ref$weights <- opt_w


  set.seed(123)
  results <- set_portfolio_weights(port_construction_method = "mvo", universe_m_d_ref = most_recent_signal_universe_m_d_ref %>%
                                     dplyr::select(-rel_risk_contr, -weights),
                                   cov_matrix_sample_size = 9, cov_estimation_method = "ewma", groups_m_d_ref = NULL, active_returns = TRUE,
                                   returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                   selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref)

  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id), results@universe_m_d_ref@data)
  expect_equal(results@groups, NULL)
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@groups, NULL)
  expect_equal(results@exp_ret_score, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))
  expect_equal(results@eligible_assets, most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))
  expect_equal(results@covariance_matrix, ewma_cov)
  expect_equal(results@rel_risk_contr, rrc$rel_risk_contr)

})

test_that("set portfolio weights work for MVO (signals) - constrained (individual)", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))

  current_date <- "2023-06-15"

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data
  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-06-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref$tickers
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df@data, chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_m_xts = mocked_backtest_returns_m_xts,
    signal_themes_m_df = signal_themes_m_df@data
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df
  selected_cov_matrix_benchmark_m_xts <- benchmark_returns_m_xts[, "IBOV"]

  expect_equal(colnames(selected_signals_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_signals_corrected_positions_m_df$low_vol_36m, signals_m_df@data$vol_36m*-1)

  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:9),]
  selected_cov_matrix_benchmark_m_xts_upd_ref <- selected_cov_matrix_benchmark_m_xts[c(1:9), ]
  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df %>% dplyr::filter(dates == current_date)

  #ts_splits
  features_m_refit <- signals_m_df@data %>% dplyr::filter(dates <= "2023-03-15") #this is to mimick ts_split behavior with a target_fwd of 3

  #custom_obj
  custom_objective <- "min_act_dd_dev"


  #Calculate an ewma cov active matrix
  selected_backtest_returns_corrected_positions_m_xts_upd_ref_active <-
    apply(selected_backtest_returns_corrected_positions_m_xts_upd_ref, 2, function(x){
      ((1+x/100)/(1+as.numeric(selected_cov_matrix_benchmark_m_xts_upd_ref)/100) - 1)*100
    })

  pca2_cov <- PortfolioAnalytics::statistical.factor.model(
    selected_backtest_returns_corrected_positions_m_xts_upd_ref_active, round(log(5))) %>% PortfolioAnalytics::extractCovariance()

  #exp_ret_score
  exp_ret_score <- most_recent_signal_universe_m_d_ref$act_dd_dev*-1
  exp_ret_score <- exp_ret_score %>% signal_transform(0.05, 0.95)
  most_recent_signal_universe_m_d_ref$exp_ret_score <- exp_ret_score

  #Port Spec
  port_spec <- PortfolioAnalytics::portfolio.spec(assets = most_recent_signal_universe_m_d_ref$tickers)
  port_spec <- PortfolioAnalytics::add.constraint(port_spec, type = "full_investment")
  port_spec <- PortfolioAnalytics::add.constraint(port_spec, type = "box",
                                                  min = pmax(most_recent_signal_universe_m_d_ref$theme_sb_bench_weights - 0.2,
                                                             0),
                                                  max = most_recent_signal_universe_m_d_ref$theme_sb_bench_weights + 0.2)


  most_recent_signal_universe_m_d_ref$max_weight <- most_recent_signal_universe_m_d_ref$theme_sb_bench_weights + 0.2
  most_recent_signal_universe_m_d_ref$min_weight <- pmax(most_recent_signal_universe_m_d_ref$theme_sb_bench_weights - 0.2,
                                                         0)

  set.seed(123)
  random_weights <- PortfolioAnalytics::random_portfolios(
    portfolio = port_spec,
    permutations = 2000,
    "sample"
  )

  #Expected returns
  returns <- random_weights %>% apply(1, function(row){
    sum(row * exp_ret_score)
  })

  #Expected risk
  risk <- random_weights %>% apply(1, function(row){
    sqrt(t(as.matrix(row)) %*% pca2_cov %*% as.matrix(row))
  })

  #sharpe
  sharpe = returns/risk
  opt_w <- random_weights[which.max(returns),]
  rrc <- relative_risk_contribution(opt_w, pca2_cov)

  most_recent_signal_universe_m_d_ref$rel_risk_contr <- rrc$rel_risk_contr
  most_recent_signal_universe_m_d_ref$weights <- opt_w


  #get optimal port
  concentration_constraint_policy <- list(
    benchmark = "theme_sb",
    max_abs_active_individual_weight = 0.2,
    max_abs_active_group_weight = NULL
  )

  set.seed(123)
  results <- set_portfolio_weights(port_construction_method = "mvo", universe_m_d_ref = most_recent_signal_universe_m_d_ref %>%
                                     dplyr::select(-max_weight, -min_weight, -weights, -rel_risk_contr),
                                   cov_matrix_sample_size = 9, cov_estimation_method = "pca2",
                                   opt_objective = "return",
                                   groups_m_d_ref = selected_signal_themes_m_d_ref, active_returns = TRUE,
                                   returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                   selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                   concentration_constraint_policy = concentration_constraint_policy,
                                   lower_quantile_winsorization = 0.05, upper_quantile_winsorization = 0.95
  )

  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id), results@universe_m_d_ref@data)


})

test_that("set portfolio weights work for MVO (signals) - constrained (individual + group)", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))

  current_date <- "2023-03-15"

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data
  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-03-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df@data, chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_m_xts = mocked_backtest_returns_m_xts
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_cov_matrix_benchmark_m_xts <- benchmark_returns_m_xts[, "IBOV"]

  expect_equal(colnames(selected_signals_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_signals_corrected_positions_m_df$low_vol_36m, signals_m_df@data$vol_36m*-1)

  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:6),]
  selected_cov_matrix_benchmark_m_xts_upd_ref <- selected_cov_matrix_benchmark_m_xts[c(1:6), ]
  signal_themes_m_d_ref <- signal_themes_m_df@data %>% dplyr::filter(dates == "2023-03-15")

  #custom_obj
  custom_objective <- "min_act_dd_dev"


  #Calculate an ewma cov active matrix
  selected_backtest_returns_corrected_positions_m_xts_upd_ref_active <-
    apply(selected_backtest_returns_corrected_positions_m_xts_upd_ref, 2, function(x){
      ((1+x/100)/(1+as.numeric(selected_cov_matrix_benchmark_m_xts_upd_ref)/100) - 1)*100
    })

  pca2_cov <- PortfolioAnalytics::statistical.factor.model(
    selected_backtest_returns_corrected_positions_m_xts_upd_ref_active, round(log(4))) %>% PortfolioAnalytics::extractCovariance()
  colnames(pca2_cov) <- colnames(selected_backtest_returns_corrected_positions_m_xts_upd_ref_active)

  #exp_ret_score
  exp_ret_score <- most_recent_signal_universe_m_d_ref$act_dd_dev*-1
  exp_ret_score <- exp_ret_score %>% signal_transform(0.05, 0.95)
  most_recent_signal_universe_m_d_ref$exp_ret_score <- exp_ret_score
  exp_ret_score <- exp_ret_score[-2] #eliminate eps_yield (not eligible)

  #Port Spec theme ss
  portfolio_spec <- PortfolioAnalytics::portfolio.spec(assets = current_eligible_signals)
  portfolio_spec <- PortfolioAnalytics::add.constraint(portfolio_spec, type = "full_investment")
  portfolio_spec <- PortfolioAnalytics::add.constraint(portfolio_spec, type = "box",
                                                       min = pmax(most_recent_signal_universe_m_d_ref$theme_ss_bench_weights[-2] - 0.2,
                                                                  0),
                                                       max = most_recent_signal_universe_m_d_ref$theme_ss_bench_weights[-2] + 0.2) #eliminate eps

  portfolio_spec <- PortfolioAnalytics::add.constraint(portfolio_spec, type = "group",
                                                       groups = list(
                                                         theme.defensive = c(2,4),
                                                         theme.momentum = 3,
                                                         theme.value = c(1)
                                                       ),
                                                       group_min = c(0.133, 0.133, 0.133),
                                                       group_max = c(0.533, 0.533, 0.533))

  most_recent_signal_universe_m_d_ref$max_weight <- most_recent_signal_universe_m_d_ref$theme_ss_bench_weights + 0.2
  most_recent_signal_universe_m_d_ref$min_weight <- pmax(most_recent_signal_universe_m_d_ref$theme_ss_bench_weights - 0.2,
                                                         0)
  most_recent_signal_universe_m_d_ref$max_weight[2] <- 0
  most_recent_signal_universe_m_d_ref$min_weight[2] <- 0

  set.seed(123)
  random_weights <- PortfolioAnalytics::random_portfolios(
    portfolio = portfolio_spec,
    permutations = 2000,
    "sample"
  )

  #Expected returns
  returns <- random_weights %>% apply(1, function(row){
    sum(row * exp_ret_score)
  })

  #Expected risk
  risk <- random_weights %>% apply(1, function(row){
    sqrt(t(as.matrix(row)) %*% pca2_cov %*% as.matrix(row))
  })

  #sharpe
  sharpe = returns/risk
  opt_w <- random_weights[which.max(returns),] %>% as.data.frame() %>% tibble::rownames_to_column()
  colnames(opt_w) <- c("tickers", "weights")
  rrc <- relative_risk_contribution(opt_w$weights, pca2_cov)
  most_recent_signal_universe_m_d_ref <- dplyr::left_join(most_recent_signal_universe_m_d_ref, rrc, by = "tickers")
  most_recent_signal_universe_m_d_ref$rel_risk_contr[2] <- 0
  most_recent_signal_universe_m_d_ref <- dplyr::left_join(most_recent_signal_universe_m_d_ref, opt_w, by = "tickers")
  most_recent_signal_universe_m_d_ref$weights[2] <- 0

  #get optimal port
  concentration_constraint_policy <- list(
    benchmark = "theme_ss",
    max_abs_active_individual_weight = 0.2,
    max_abs_active_group_weight = c(theme = 0.2)
  )

  set.seed(123)
  results <- set_portfolio_weights(port_construction_method = "mvo",
                                   universe_m_d_ref = most_recent_signal_universe_m_d_ref %>%
                                     dplyr::select(-max_weight, -min_weight, -weights, -rel_risk_contr),
                                   cov_matrix_sample_size = 6, cov_estimation_method = "pca2",
                                   opt_objective = "return",
                                   groups_m_d_ref = signal_themes_m_d_ref, active_returns = TRUE,
                                   returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                   selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                   concentration_constraint_policy = concentration_constraint_policy,
                                   lower_quantile_winsorization = 0.05, upper_quantile_winsorization = 0.95
  )


  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id), results@universe_m_d_ref@data)

  #Port Spec theme SB
  most_recent_signal_universe_m_d_ref <- most_recent_signal_universe_m_d_ref %>% dplyr::select(-max_weight, -min_weight,
                                                                                               -weights, -rel_risk_contr)

  portfolio_spec <- PortfolioAnalytics::portfolio.spec(assets = current_eligible_signals)
  portfolio_spec <- PortfolioAnalytics::add.constraint(portfolio_spec, type = "full_investment")
  portfolio_spec <- PortfolioAnalytics::add.constraint(portfolio_spec, type = "box",
                                                       min = pmax(most_recent_signal_universe_m_d_ref$theme_sb_bench_weights[-2] - 0.1,
                                                                  0),
                                                       max = most_recent_signal_universe_m_d_ref$theme_sb_bench_weights[-2] + 0.1) #eliminate eps

  portfolio_spec <- PortfolioAnalytics::add.constraint(portfolio_spec, type = "group",
                                                       groups = list(
                                                         theme.defensive = c(2,4),
                                                         theme.momentum = 3,
                                                         theme.value = c(1)
                                                       ),
                                                       group_min = c(0.3, 0, 0.3),
                                                       group_max = c(0.7, 0.2, 0.7))

  most_recent_signal_universe_m_d_ref$max_weight <- most_recent_signal_universe_m_d_ref$theme_sb_bench_weights + 0.1
  most_recent_signal_universe_m_d_ref$min_weight <- pmax(most_recent_signal_universe_m_d_ref$theme_sb_bench_weights - 0.1,
                                                         0)
  most_recent_signal_universe_m_d_ref$max_weight[2] <- 0
  most_recent_signal_universe_m_d_ref$min_weight[2] <- 0

  set.seed(123)
  random_weights <- PortfolioAnalytics::random_portfolios(
    portfolio = portfolio_spec,
    permutations = 2000,
    "sample"
  )

  #Expected returns
  returns <- random_weights %>% apply(1, function(row){
    sum(row * exp_ret_score)
  })

  #Expected risk
  risk <- random_weights %>% apply(1, function(row){
    sqrt(t(as.matrix(row)) %*% pca2_cov %*% as.matrix(row))
  })

  #sharpe
  sharpe = returns/risk
  opt_w <- random_weights[which.min(risk),] %>% as.data.frame() %>% tibble::rownames_to_column()
  colnames(opt_w) <- c("tickers", "weights")
  rrc <- relative_risk_contribution(opt_w$weights, pca2_cov)
  most_recent_signal_universe_m_d_ref <- dplyr::left_join(most_recent_signal_universe_m_d_ref, rrc, by = "tickers")
  most_recent_signal_universe_m_d_ref$rel_risk_contr[2] <- 0

  most_recent_signal_universe_m_d_ref <- dplyr::left_join(most_recent_signal_universe_m_d_ref, opt_w, by = "tickers")
  most_recent_signal_universe_m_d_ref$weights[2] <- 0

  #get optimal port
  concentration_constraint_policy <- list(
    benchmark = "theme_sb",
    max_abs_active_individual_weight = 0.1,
    max_abs_active_group_weight = c(theme = 0.2)
  )

  set.seed(123)
  results <-  set_portfolio_weights(port_construction_method = "mvo",
                                    universe_m_d_ref = most_recent_signal_universe_m_d_ref %>%
                                      dplyr::select(-max_weight, -min_weight, -weights, -rel_risk_contr),
                                    cov_matrix_sample_size = 6, cov_estimation_method = "pca2",
                                    opt_objective = "risk",
                                    groups_m_d_ref = signal_themes_m_d_ref, active_returns = TRUE,
                                    returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                    selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                    concentration_constraint_policy = concentration_constraint_policy,
                                    lower_quantile_winsorization = 0.05, upper_quantile_winsorization = 0.95
  )

  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id), results@universe_m_d_ref@data)


})

test_that("set portfolio weights work for MVO (signals) - constrained (individual + group) + Ridge Penalty", {

  #Load
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))
  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_results.RData", sep =""))

  current_date <- "2023-03-15"

  #Select and correct signals
  signal_universe_m_df <- results@signal_universe_m_df@data
  most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == "2023-03-15")
  current_eligible_signals <- most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
  signals_positions <- ifelse(stringr::str_detect(current_eligible_signals, "low_"), "short", "long")
  chosen_signals_and_positions <- signals_positions
  names(chosen_signals_and_positions) <- stringr::str_remove(current_eligible_signals, "low_")

  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df@data, chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_m_xts = mocked_backtest_returns_m_xts
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_cov_matrix_benchmark_m_xts <- benchmark_returns_m_xts[, "IBOV"]

  expect_equal(colnames(selected_signals_corrected_positions_m_df)[-c(1:3)], current_eligible_signals)
  expect_equal(selected_signals_corrected_positions_m_df$low_vol_36m, signals_m_df@data$vol_36m*-1)

  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:6),]
  selected_cov_matrix_benchmark_m_xts_upd_ref <- selected_cov_matrix_benchmark_m_xts[c(1:6), ]
  signal_themes_m_d_ref <- signal_themes_m_df@data %>% dplyr::filter(dates == "2023-03-15")

  #custom_obj
  custom_objective <- "min_act_dd_dev"


  #Calculate an ewma cov active matrix
  selected_backtest_returns_corrected_positions_m_xts_upd_ref_active <-
    apply(selected_backtest_returns_corrected_positions_m_xts_upd_ref, 2, function(x){
      ((1+x/100)/(1+as.numeric(selected_cov_matrix_benchmark_m_xts_upd_ref)/100) - 1)*100
    })

  pca2_cov <- PortfolioAnalytics::statistical.factor.model(
    selected_backtest_returns_corrected_positions_m_xts_upd_ref_active, round(log(4))) %>% PortfolioAnalytics::extractCovariance()
  colnames(pca2_cov) <- colnames(selected_backtest_returns_corrected_positions_m_xts_upd_ref_active)

  #exp_ret_score
  exp_ret_score <- most_recent_signal_universe_m_d_ref$act_dd_dev*-1
  exp_ret_score <- exp_ret_score %>% signal_transform(0.05, 0.95)
  most_recent_signal_universe_m_d_ref$exp_ret_score <- exp_ret_score
  exp_ret_score <- exp_ret_score[-2] #eliminate eps_yield (not eligible)

  #Port Spec theme ss
  portfolio_spec <- PortfolioAnalytics::portfolio.spec(assets = current_eligible_signals)
  portfolio_spec <- PortfolioAnalytics::add.constraint(portfolio_spec, type = "full_investment")
  portfolio_spec <- PortfolioAnalytics::add.constraint(portfolio_spec, type = "box",
                                                       min = pmax(most_recent_signal_universe_m_d_ref$theme_ss_bench_weights[-2] - 0.2,
                                                                  0),
                                                       max = most_recent_signal_universe_m_d_ref$theme_ss_bench_weights[-2] + 0.2) #eliminate eps

  portfolio_spec <- PortfolioAnalytics::add.constraint(portfolio_spec, type = "group",
                                                       groups = list(
                                                         theme.defensive = c(2,4),
                                                         theme.momentum = 3,
                                                         theme.value = c(1)
                                                       ),
                                                       group_min = c(0.133, 0.133, 0.133),
                                                       group_max = c(0.533, 0.533, 0.533))

  most_recent_signal_universe_m_d_ref$max_weight <- most_recent_signal_universe_m_d_ref$theme_ss_bench_weights + 0.2
  most_recent_signal_universe_m_d_ref$min_weight <- pmax(most_recent_signal_universe_m_d_ref$theme_ss_bench_weights - 0.2,
                                                         0)
  most_recent_signal_universe_m_d_ref$max_weight[2] <- 0
  most_recent_signal_universe_m_d_ref$min_weight[2] <- 0

  set.seed(123)
  random_weights <- PortfolioAnalytics::random_portfolios(
    portfolio = portfolio_spec,
    permutations = 2000,
    "sample"
  )

  #Expected returns
  returns <- random_weights %>% apply(1, function(row){
    sum(row * exp_ret_score)
  })

  #Expected risk
  risk <- random_weights %>% apply(1, function(row){
    sqrt(t(as.matrix(row)) %*% pca2_cov %*% as.matrix(row))
  })

  #sharpe
  sharpe = returns/risk

  #Regularize with Ridge
  most_recent_signal_universe_m_d_ref <- most_recent_signal_universe_m_d_ref %>%
    dplyr::mutate(target_weights = theme_sb_bench_weights, .before = theme)

   ##Calculate difference of weights in relation to target
   diffs <- as.data.frame(t(random_weights)) %>%
     dplyr::mutate(tickers = rownames(.), .before = dplyr::everything()) %>%
     dplyr::left_join(most_recent_signal_universe_m_d_ref %>% dplyr::select(tickers, target_weights),
                      by = "tickers")

   ##Calculate squared difference between all cols in diffs and target_weights col
   diffs_vec <- vector(length = ncol(diffs) - 2)
   names(diffs_vec) <- colnames(diffs)[-c(1, ncol(diffs))]
   for(col in colnames(diffs)){

     #Skip if col is either 'tickers' or 'target_weights'
     if(col %in% c("tickers", "target_weights")) next

     diffs_vec[col] <- sum((diffs[[col]] - diffs$target_weights)^2)


   }

   ridge_pen <- 10

   #Choose the portfolio that maximizes sharpe_ratio - ridge_pen*diff
   best_port <- which.max(sharpe - ridge_pen*diffs_vec)
   weights_best_port <- random_weights[best_port, ,drop = FALSE]
   weights_best_port_df <- data.frame(tickers = colnames(weights_best_port),
                                      weights = as.numeric(weights_best_port[1,]))

  ## Add it to signal_universe_m_d_ref
  most_recent_signal_universe_m_d_ref <- most_recent_signal_universe_m_d_ref %>%
    dplyr::left_join(weights_best_port_df, by = "tickers")
  most_recent_signal_universe_m_d_ref$weights[2] <- 0

  ## Calculate rel risk contr
  rrc <- relative_risk_contribution(
    as.numeric(weights_best_port),
    pca2_cov
  )
  most_recent_signal_universe_m_d_ref <- most_recent_signal_universe_m_d_ref %>%
    dplyr::left_join(rrc, by = "tickers")
  most_recent_signal_universe_m_d_ref$rel_risk_contr[2] <- 0

  ## Relocate to before weights
  most_recent_signal_universe_m_d_ref <- most_recent_signal_universe_m_d_ref %>%
    dplyr::relocate(rel_risk_contr, .before = weights)


  #get optimal port
  concentration_constraint_policy <- list(
    benchmark = "theme_ss",
    max_abs_active_individual_weight = 0.2,
    max_abs_active_group_weight = c(theme = 0.2)
  )

  set.seed(123)
  results <- set_portfolio_weights(port_construction_method = "mvo",
                                   universe_m_d_ref = most_recent_signal_universe_m_d_ref %>%
                                     dplyr::select(-max_weight, -min_weight, -weights, -rel_risk_contr) %>%
                                     dplyr::mutate(target_weights = theme_sb_bench_weights,
                                                   .before = theme),
                                   cov_matrix_sample_size = 6, cov_estimation_method = "pca2",
                                   opt_objective = "sharpe",
                                   ridge_pen = ridge_pen,
                                   groups_m_d_ref = signal_themes_m_d_ref, active_returns = TRUE,
                                   returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                   selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                   concentration_constraint_policy = concentration_constraint_policy,
                                   lower_quantile_winsorization = 0.05, upper_quantile_winsorization = 0.95
  )


  expect_equal(most_recent_signal_universe_m_d_ref %>% dplyr::arrange(id), results@universe_m_d_ref@data)


  #Test that chosen portfolio is not the one that maximizes sharpe ratio
  expect_false(unname(best_port) == which.max(sharpe))

  #Test that chosen portfolio is closer to the target weights than the one that maximizes sharpe ratio
  expect_true(diffs_vec[best_port] < diffs_vec[which.max(sharpe)])

  #Run again with lower ridge pen and test that new port is further from target weights and has higher sharpe
  ridge_pen <- 0.05
  set.seed(123)
  results_2 <- set_portfolio_weights(port_construction_method = "mvo",
                                   universe_m_d_ref = most_recent_signal_universe_m_d_ref %>%
                                     dplyr::select(-max_weight, -min_weight, -weights, -rel_risk_contr) %>%
                                     dplyr::mutate(target_weights = theme_sb_bench_weights,
                                                   .before = theme),
                                   cov_matrix_sample_size = 6, cov_estimation_method = "pca2",
                                   opt_objective = "sharpe",
                                   ridge_pen = ridge_pen,
                                   groups_m_d_ref = signal_themes_m_d_ref, active_returns = TRUE,
                                   returns_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                   selected_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
                                   concentration_constraint_policy = concentration_constraint_policy,
                                   lower_quantile_winsorization = 0.05, upper_quantile_winsorization = 0.95
  )

  new_weights <- results_2@universe_m_d_ref@data$weights
  old_weights <- results@universe_m_d_ref@data$weights

  new_diffs <- sum((new_weights - most_recent_signal_universe_m_d_ref$theme_sb_bench_weights)^2)
  old_diffs <- sum((old_weights - most_recent_signal_universe_m_d_ref$theme_sb_bench_weights)^2)

  expect_true(new_diffs > old_diffs)

  new_sharpe <- sum(new_weights[-2] * exp_ret_score)/sqrt(t(as.matrix(new_weights[-2])) %*% pca2_cov %*% as.matrix(new_weights[-2]))
  old_sharpe <- sum(old_weights[-2] * exp_ret_score)/sqrt(t(as.matrix(old_weights[-2])) %*% pca2_cov %*% as.matrix(old_weights[-2]))
  expect_true(new_sharpe > old_sharpe)

})



#Stocks (in artificial_port_obj, all tickers are eligible, differently from toy_preprocessed)
test_that("set portfolio weights works for stocks (ew) - artificial_port_obj ", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.20, 0.20, 0.20, 0.20, 0.20)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(Gamma = "long"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Test EW
  expected_results <- stock_universe_m_d_ref
  expected_results$weights <- rep(0.25, 4)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "ew")

  expect_equal(results@universe_m_d_ref@data, expected_results)

})

test_that("set portfolio weights works for stocks (cw) - artificial_port_obj ", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.20, 0.20, 0.20, 0.20, 0.20)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(Gamma = "long"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Test CW
  expected_results <- stock_universe_m_d_ref
  expected_results$cap_score <- signal_transform(expected_results$mean_volfin_3m, lower_quantile_winsorization, upper_quantile_winsorization)
  expected_results$weights <- expected_results$cap_score/sum(expected_results$cap_score)

  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "cw",
                                   liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = "mean_volfin_3m")

  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)

})

test_that("set portfolio weights works for stocks (sw) - artificial_port_obj ", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.20, 0.20, 0.20, 0.20, 0.20)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(Gamma = "long"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Test CS
  expected_results <- stock_universe_m_d_ref
  expected_results$cap_score <- signal_transform(expected_results$mean_volfin_3m, lower_quantile_winsorization, upper_quantile_winsorization)
  expected_results$weights <- (expected_results$cap_score * expected_results$exp_ret_score)/sum((expected_results$cap_score * expected_results$exp_ret_score))

  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "cs",
                                   liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = "mean_volfin_3m")

  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)

})

test_that("set portfolio weights works for stocks (cs) - artificial_port_obj ", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.20, 0.20, 0.20, 0.20, 0.20)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(Gamma = "long"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Test SW
  expected_results <- stock_universe_m_d_ref
  expected_results$weights[expected_results$is_eligible == 1] <- (expected_results$exp_ret_score[expected_results$is_eligible == 1])/
    sum(expected_results$exp_ret_score[expected_results$is_eligible == 1])
  expected_results$weights[which(is.na(expected_results$weights))] <- 0

  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "sw",
                                   liquidity_m_d_ref = liquidity_m_d_ref)

  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 1) %>% dplyr::slice_max(weights, n = 50) %>% dplyr::pull(tickers),
               results@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 1) %>% dplyr::slice_max(exp_ret_score, n = 50) %>% dplyr::pull(tickers))
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)

})

test_that("set portfolio weights works for stocks (rp) - artificial_port_obj ", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.20, 0.20, 0.20, 0.20, 0.20)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(Gamma = "long"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Test RP
  expected_results <- stock_universe_m_d_ref

  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]

  covariance_matrix <- estimate_covariance_matrix(tickers = c("Stock A", "Stock C", "Stock D", "Stock E"), returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  rp_results <- riskParityPortfolio::riskParityPortfolio(Sigma = covariance_matrix)
  expected_results$rel_risk_contr <- rp_results$relative_risk_contribution
  expected_results$weights <- rp_results$w

  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "rp",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method
  )


  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)

})

test_that("set_portfolio weights works for stocks (rp + exp_ret_score_tilt = 'inner')", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.20, 0.20, 0.20, 0.20, 0.20)
  exp_ret_score_tilt_eta <- 0.5

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(Gamma = "long"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Test RP
  expected_results <- stock_universe_m_d_ref

  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]

  covariance_matrix <- estimate_covariance_matrix(tickers = c("Stock A", "Stock C", "Stock D", "Stock E"), returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  rp_results <- riskParityPortfolio::riskParityPortfolio(Sigma = covariance_matrix,
                                                         mu = expected_results$exp_ret_score,
                                                         lmd_mu = exp_ret_score_tilt_eta)
  expected_results$rel_risk_contr <- rp_results$relative_risk_contribution
  expected_results$weights <- rp_results$w

  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "rp",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                   exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                                   exp_ret_score_tilt = "inner"
  )


  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)

  # Test that relative risk contribution is biased towards the stock with highest expected return
  stock_with_highest_exp_ret <- expected_results %>% dplyr::filter(exp_ret_score == max(exp_ret_score)) %>% dplyr::pull(tickers)
  stock_with_highest_rrc <- results@universe_m_d_ref@data %>% dplyr::filter(rel_risk_contr == max(rel_risk_contr)) %>% dplyr::pull(tickers)
  expect_equal(stock_with_highest_exp_ret, stock_with_highest_rrc)





})

test_that("set_portfolio weights works for stocks (hrp + exp_ret_score_tilt = 'inner')", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.20, 0.20, 0.20, 0.20, 0.20)
  exp_ret_score_tilt_eta <- 0.5

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                                          chosen_score_metric_and_position = c(Gamma = "long"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Force 1 ineligible stock to test HRP with less than full universe
  stock_universe_m_d_ref$is_eligible[1] <- 0

  #Test HRP
  expected_results <- stock_universe_m_d_ref
  exp_ret_score <- stock_universe_m_d_ref %>%
    dplyr::filter(is_eligible == 1L) %>%
    dplyr::pull(exp_ret_score)

  names(exp_ret_score) <- stock_universe_m_d_ref %>%
    dplyr::filter(is_eligible == 1L) %>%
    dplyr::pull(tickers)


  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]

  covariance_matrix <- estimate_covariance_matrix(tickers = c("Stock C", "Stock D", "Stock E"),
                                                  returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref)
  #Helper IVP
  ivp <- function(C){
    d <- diag(C)

    w <- 1 / d
    w <- w / sum(w)
  }

  # Calculate correlation, then distnace matrix and then hc
  corr <- cov2cor(covariance_matrix)
  dist <- as.dist(sqrt(0.5 * (1 - corr)))
  hc <- hclust(dist, method = "single")
  hc_order <- hc$order
  eligible_tickers_ordered <- rownames(covariance_matrix)[hc_order]

  #weights
  w <- rep(1, nrow(covariance_matrix))

  #Order Sigma
  ordered_sigma <- covariance_matrix[hc_order, hc_order]
  ordered_exp_ret_score <- exp_ret_score[eligible_tickers_ordered]
  index_L <- 1
  index_R <- c(2,3)
  cov_L <- ordered_sigma[index_L, index_L, drop = FALSE]
  cov_R <- ordered_sigma[index_R, index_R, drop = FALSE]
  w_R <- ivp(cov_R)
  var_L <- as.numeric(cov_L)
  var_R <- as.numeric(t(w_R) %*% cov_R %*% w_R)
  mu_L <- exp_ret_score[rownames(cov_L)]
  mu_R <- sum(w_R * exp_ret_score[rownames(cov_R)])
  rk <- rank(c(mu_L, mu_R), ties.method = "average")
  gL <- rk[1]/2; gR <- rk[2]/2
  A_L <- (1/var_L) * (gL ^ exp_ret_score_tilt_eta)
  A_R <- (1/var_R) * (gR ^ exp_ret_score_tilt_eta)
  alpha <- as.numeric(A_L/(A_L + A_R))
  w[index_L] <- w[index_L] * alpha
  w[index_R] <- w[index_R] * (1 - alpha)

  #2nd split
  index_L <- 2
  index_R <- 3
  cov_L <- ordered_sigma[index_L, index_L, drop = FALSE]
  cov_R <- ordered_sigma[index_R, index_R, drop = FALSE]
  var_L <- as.numeric(cov_L)
  var_R <- as.numeric(cov_R)
  mu_L <- exp_ret_score[rownames(cov_L)]
  mu_R <- exp_ret_score[rownames(cov_R)]
  rk <- rank(c(mu_L, mu_R), ties.method = "average")
  gL <- rk[1]/2; gR <- rk[2]/2
  A_L <- (1/var_L) * (gL ^ exp_ret_score_tilt_eta)
  A_R <- (1/var_R) * (gR ^ exp_ret_score_tilt_eta)
  alpha <- A_L/(A_L + A_R)
  w[index_L] <- w[index_L] * alpha
  w[index_R] <- w[index_R] * (1 - alpha)
  names(w) <- rownames(ordered_sigma)

  #Add exp_ret_score
  rrc <- relative_risk_contribution(w[rownames(covariance_matrix)], covariance_matrix)

  expected_results$rel_risk_contr <- c(0, rrc$rel_risk_contr)
  expected_results$weights <- c(0, w[rownames(covariance_matrix)])

  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "hrp",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                   active_returns = FALSE,
                                   exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                                   exp_ret_score_tilt = "inner"
  )

  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)

  # Test that relative risk contribution is biased towards the stock with highest expected return
  stock_with_highest_exp_ret <- expected_results %>%
    dplyr::filter(is_eligible == 1L) %>%
    dplyr::filter(exp_ret_score == max(exp_ret_score)) %>%
    dplyr::pull(tickers)

  stock_with_highest_rrc <- results@universe_m_d_ref@data %>%
    dplyr::filter(rel_risk_contr == max(rel_risk_contr)) %>%
    dplyr::pull(tickers)

  expect_true(stock_with_highest_rrc %in% stock_with_highest_exp_ret)





})

test_that("set portfolio weights works for stocks (mvo_unc) - artificial_port_obj ", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.20, 0.20, 0.20, 0.20, 0.20)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(Gamma = "long"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Test MVO Unconstrained
  expected_results <- stock_universe_m_d_ref
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]

  covariance_matrix <- estimate_covariance_matrix(tickers = stock_universe_m_d_ref$tickers, returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  #Portfolio
  port_spec <- PortfolioAnalytics::portfolio.spec(assets = expected_results$tickers)
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "full_investment")
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "box")

  set.seed(123)
  rp_weights <- PortfolioAnalytics::random_portfolios(portfolio = port_spec_constrained,
                                                      permutations = 2000,
                                                      rp_method = "sample")

  #Best Portfolio for Sharpe
  rp_weights <- as.matrix(rp_weights)  # Portfolio weights
  exp_ret_score <- as.matrix(stock_universe_m_d_ref$exp_ret_score)  # Expected return vector
  cov_matrix <- as.matrix(covariance_matrix)  # Covariance matrix
  # Calculate Portfolio Return (Expected Return)
  portfolio_return <- rp_weights %*% exp_ret_score  # Matrix multiplication
  # Calculate Portfolio Risk (Standard Deviation)
  portfolio_risk <- sqrt(rowSums((rp_weights %*% cov_matrix) * rp_weights))
  # Optimal Sharpe
  optimal_sharpe_weights <- rp_weights[which.max(portfolio_return/portfolio_risk),]
  optimal_ret <-  portfolio_return[which.max(portfolio_return/portfolio_risk),]
  optimal_risk <- portfolio_risk[which.max(portfolio_return/portfolio_risk)]

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "mvo",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method
  )

  #Check for random weights generation
  expected_weights <- t(rp_weights) %>% as.data.frame() %>% tibble::rownames_to_column("tickers")
  expect_equal(results@random_port_weights, expected_weights)
  expect_equal(results@weights, optimal_sharpe_weights %>% unname())
  expect_equal(2.375, optimal_ret, tolerance = 1e-2)
  expect_equal(0.795, optimal_risk, tolerance = 1e-2)

  #Best Portfolio for Return
  optimal_ret_weights <-  rp_weights[which.max(portfolio_return),]

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "mvo",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                   opt_objective = "return"
  )
  expect_equal(results@weights, optimal_ret_weights %>% unname())

  #Best Portfolio for Risk
  optimal_risk_weights <-  rp_weights[which.min(portfolio_risk),]

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "mvo",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                   opt_objective = "risk"
  )
  expect_equal(results@weights, optimal_risk_weights %>% unname())
  expected_results$rel_risk_contr <- relative_risk_contribution(optimal_risk_weights %>% unname(), covariance_matrix)$rel_risk_contr
  expected_results$weights <- optimal_risk_weights

  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)


})

test_that("set portfolio weights works for stocks (mvo_con) - artificial_port_obj ", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.20, 0.20, 0.20, 0.20, 0.20)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(Gamma = "long"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Test MVO Constrained
  expected_results <- stock_universe_m_d_ref
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]

  covariance_matrix <-  estimate_covariance_matrix(tickers = stock_universe_m_d_ref$tickers, returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                   cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                   active_returns = FALSE, groups_m_d_ref = stock_groups_m_d_ref
  )


  #Portfolio
  port_spec <- PortfolioAnalytics::portfolio.spec(assets = stock_universe_m_d_ref$tickers)
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "full_investment")
  #Box constraints
  eligible_universe_m_d_ref <- generate_box_constraints(universe_m_d_ref = stock_universe_m_d_ref,
                                                        liquidity_constraint_policy = liquidity_constraint_policy,
                                                        turnover_constraint_policy = turnover_constraint_policy,
                                                        concentration_constraint_policy = concentration_constraint_policy)

  port_spec_constrained <- PortfolioAnalytics::add.constraint(type = "box", portfolio = port_spec_constrained,
                                                              min = eligible_universe_m_d_ref$min_weight,
                                                              max = eligible_universe_m_d_ref$max_weight)
  #Group constraints
  group_constraints_helper <- generate_group_constraints(universe_m_d_ref = stock_universe_m_d_ref, concentration_constraint_policy = concentration_constraint_policy,
                                                         groups_m_d_ref = stock_groups_m_d_ref)

  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec_constrained,
                                                              type = "group",
                                                              groups = group_constraints_helper$eligible_assets_group_membership_list,
                                                              group_min = group_constraints_helper$group_constraint_min,
                                                              group_max = group_constraints_helper$group_constraint_max
  )
  expected_results$max_weight <- eligible_universe_m_d_ref$max_weight
  expected_results$min_weight <- eligible_universe_m_d_ref$min_weight

  #Generate random ports
  set.seed(123)
  rp_weights <- PortfolioAnalytics::random_portfolios(portfolio = port_spec_constrained,
                                                      permutations = 2000,
                                                      rp_method = "sample")

  #Best Portfolio for Sharpe
  rp_weights <- as.matrix(rp_weights)  # Portfolio weights
  exp_ret_score <- as.matrix(stock_universe_m_d_ref$exp_ret_score)  # Expected return vector
  cov_matrix <- as.matrix(covariance_matrix)  # Covariance matrix
  # Calculate Portfolio Return (Expected Return)
  portfolio_return <- rp_weights %*% exp_ret_score  # Matrix multiplication
  # Calculate Portfolio Risk (Standard Deviation)
  portfolio_risk <- sqrt(rowSums((rp_weights %*% cov_matrix) * rp_weights))
  # Optimal Sharpe
  optimal_sharpe_weights <- rp_weights[which.max(portfolio_return/portfolio_risk),]
  optimal_ret <-  portfolio_return[which.max(portfolio_return/portfolio_risk),]
  optimal_risk <- portfolio_risk[which.max(portfolio_return/portfolio_risk)]

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "mvo",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                   liquidity_constraint_policy = liquidity_constraint_policy,
                                   turnover_constraint_policy = turnover_constraint_policy,
                                   concentration_constraint_policy = concentration_constraint_policy
  )

  #Check for random weights generation
  expected_weights <- t(rp_weights) %>% as.data.frame() %>% tibble::rownames_to_column("tickers")
  expect_equal(results@random_port_weights, expected_weights)
  expect_equal(results@weights, optimal_sharpe_weights %>% unname())
  expect_equal(1.485, optimal_ret, tolerance = 1e-2)
  expect_equal(0.672, optimal_risk, tolerance = 1e-2)

  #Best Portfolio for Return
  optimal_ret_weights <-  rp_weights[which.max(portfolio_return),]

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "mvo",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                   opt_objective = "return", liquidity_constraint_policy = liquidity_constraint_policy,
                                   turnover_constraint_policy = turnover_constraint_policy,
                                   concentration_constraint_policy = concentration_constraint_policy
  )
  expect_equal(results@weights, optimal_ret_weights %>% unname())

  #Best Portfolio for Risk
  optimal_risk_weights <-  rp_weights[which.min(portfolio_risk),]

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "mvo",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                   opt_objective = "risk", liquidity_constraint_policy = liquidity_constraint_policy,
                                   turnover_constraint_policy = turnover_constraint_policy,
                                   concentration_constraint_policy = concentration_constraint_policy
  )
  expect_equal(results@weights, optimal_risk_weights %>% unname())
  expected_results$rel_risk_contr <- relative_risk_contribution(optimal_risk_weights %>% unname(), covariance_matrix)$rel_risk_contr
  expected_results$weights <- optimal_risk_weights

  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)


  #Check that constraints match expectations
  #Upper box
  expect_true(all(
    all(rp_weights[,1] <= eligible_universe_m_d_ref$max_weight[1]),
    all(rp_weights[,2] <= eligible_universe_m_d_ref$max_weight[2]),
    all(rp_weights[,3] <= eligible_universe_m_d_ref$max_weight[3]),
    all(rp_weights[,4] <= eligible_universe_m_d_ref$max_weight[4])))

  #Lower box
  expect_true(all(
    all(rp_weights[,1] >= eligible_universe_m_d_ref$min_weight[1]),
    all(rp_weights[,2] >= eligible_universe_m_d_ref$min_weight[2]),
    all(rp_weights[,3] >= eligible_universe_m_d_ref$min_weight[3]),
    all(rp_weights[,4] >= eligible_universe_m_d_ref$min_weight[4])))

  #Group
  sector_cyclical <- rp_weights[,3] + rp_weights[,4]
  sector_financial <- rp_weights[,2]
  sector_oil <- rp_weights[,1]
  subsector_education <- rp_weights[,4]
  subsector_insurance <- rp_weights[,2]
  subsector_oil <- rp_weights[,1]
  subsector_retail <- rp_weights[,3]

  #Lower Group
  expect_true(all(
    all(sector_cyclical >= group_constraints_helper$group_constraint_min[1]),
    all(sector_financial >= group_constraints_helper$group_constraint_min[2]),
    all(sector_oil >= group_constraints_helper$group_constraint_min[3]),
    all(subsector_education >= group_constraints_helper$group_constraint_min[4]),
    all(subsector_insurance >= group_constraints_helper$group_constraint_min[5]),
    all(subsector_oil >= group_constraints_helper$group_constraint_min[6]),
    all(subsector_retail >= group_constraints_helper$group_constraint_min[7])
  ))

  #Upper group
  expect_true(all(
    all(sector_cyclical <= group_constraints_helper$group_constraint_max[1]),
    all(sector_financial <= group_constraints_helper$group_constraint_max[2]),
    all(sector_oil <= group_constraints_helper$group_constraint_max[3]),
    all(subsector_education <= group_constraints_helper$group_constraint_max[4]),
    all(subsector_insurance <= group_constraints_helper$group_constraint_max[5]),
    all(subsector_oil <= group_constraints_helper$group_constraint_max[6]),
    all(subsector_retail <= group_constraints_helper$group_constraint_max[7])
  ))


})

test_that("set portfolio weights works for stocks (ew) - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(vol_36m = "short")

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 6, port_construction_method = "ew",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             min_eligible_assets_fallback = NULL, scaler_m_df = NULL, chosen_scaler = NULL, scaler_shrinkage = NULL,
                             use_raw_for_eligibility = NULL, exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = NULL, cov_matrix_sample_size = NULL, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_list,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2023-04-15"


  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test EW
  expected_results <- stock_universe_m_d_ref
  expected_results <- expected_results %>% dplyr::mutate(weights = dplyr::if_else(is_eligible == 1, 1/sum(is_eligible), 0))
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "ew")

  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)

})

test_that("set portfolio weights works for stocks (cw) - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(vol_36m = "short")

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 6, port_construction_method = "cw",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             scaler_m_df = NULL, chosen_scaler = NULL, scaler_shrinkage = NULL,
                             min_eligible_assets_fallback = NULL, use_raw_for_eligibility = NULL, exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = NULL, cov_matrix_sample_size = NULL, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_list,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test CW
  expected_results <- stock_universe_m_d_ref
  expected_results <- expected_results %>%
    dplyr::mutate(cap_score = signal_transform(mean_volfin_3m, lower_quantile_winsorization, upper_quantile_winsorization))
  expected_results$weights[expected_results$is_eligible == 1] <- expected_results$cap_score[expected_results$is_eligible == 1]/sum(expected_results$cap_score[expected_results$is_eligible == 1])
  expected_results$weights[which(is.na(expected_results$weights))] <- 0

  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "cw",
                                   liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = "mean_volfin_3m")

  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)

})

test_that("set portfolio weights works for stocks (sw) - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(vol_36m = "short")

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 6, port_construction_method = "sw",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             min_eligible_assets_fallback = NULL, scaler_m_df = NULL, chosen_scaler = NULL, scaler_shrinkage = NULL,
                             use_raw_for_eligibility = NULL, exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = NULL, cov_matrix_sample_size = NULL, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_list,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test SW
  expected_results <- stock_universe_m_d_ref
  expected_results$weights[expected_results$is_eligible == 1] <- (expected_results$exp_ret_score[expected_results$is_eligible == 1])/
    sum(expected_results$exp_ret_score[expected_results$is_eligible == 1])
  expected_results$weights[which(is.na(expected_results$weights))] <- 0

  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "sw",
                                   liquidity_m_d_ref = liquidity_m_d_ref)

  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 1) %>% dplyr::slice_max(weights, n = 50) %>% dplyr::pull(tickers),
               results@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 1) %>% dplyr::slice_max(exp_ret_score, n = 50) %>% dplyr::pull(tickers))
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)

})

test_that("set portfolio weights works for stocks (cs) - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(vol_36m = "short")

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 6, port_construction_method = "cw",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             min_eligible_assets_fallback = NULL, scaler_m_df = NULL, chosen_scaler = NULL, scaler_shrinkage = NULL,
                             use_raw_for_eligibility = NULL, exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = NULL, cov_matrix_sample_size = NULL, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_list,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test CS
  expected_results <- stock_universe_m_d_ref
  expected_results$cap_score <- signal_transform(expected_results$mean_volfin_3m, lower_quantile_winsorization, upper_quantile_winsorization)
  expected_results$weights[expected_results$is_eligible == 1] <- (expected_results$cap_score[expected_results$is_eligible == 1] * expected_results$exp_ret_score[expected_results$is_eligible == 1])/
    sum((expected_results$cap_score[expected_results$is_eligible == 1] * expected_results$exp_ret_score[expected_results$is_eligible == 1]))
  expected_results$weights[which(is.na(expected_results$weights))] <- 0

  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "cs",
                                   liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = "mean_volfin_3m")

  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)

})

test_that("set portfolio weights works for stocks (rp) - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range and others
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(vol_36m = "short")
  cov_matrix_sample_size <- 60
  cov_estimation_method <- "cc"

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 6, port_construction_method = "rp",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             min_eligible_assets_fallback = NULL, scaler_m_df = NULL, chosen_scaler = NULL, scaler_shrinkage = NULL,
                             use_raw_for_eligibility = NULL, exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = cov_estimation_method, cov_matrix_sample_size = cov_matrix_sample_size, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = daily_stock_returns_m_xts, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_list,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test RP
  expected_results <- stock_universe_m_d_ref
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  eligible_tickers <- expected_results %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  covariance_matrix <- estimate_covariance_matrix(tickers = eligible_tickers, returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  rp_results <- riskParityPortfolio::riskParityPortfolio(Sigma = covariance_matrix)

  expected_results[which(expected_results$is_eligible == 1), "rel_risk_contr"] <-
    rp_results$relative_risk_contribution[expected_results[which(expected_results$is_eligible == 1), "tickers"]] #Fill only eligibles

  expected_results[which(expected_results$is_eligible == 1), "weights"] <-
    rp_results$w[expected_results[which(expected_results$is_eligible == 1), "tickers"]] #Fill only eligibles

  expected_results$rel_risk_contr[which(is.na(expected_results$rel_risk_contr))] <- 0
  expected_results$weights[which(is.na(expected_results$weights))] <- 0


  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "rp",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                   active_returns = FALSE
  )


  expect_equal(results@universe_m_d_ref@data, expected_results)


})

test_that("set portfolio weights works for stocks (rp + exp_ret_score_tilt = 'inner') - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range and others
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(vol_36m = "short")
  cov_matrix_sample_size <- 60
  cov_estimation_method <- "cc"
  exp_ret_score_tilt <- "inner"
  exp_ret_score_tilt_eta <- 0.5

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 6, port_construction_method = "rp",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             min_eligible_assets_fallback = NULL, scaler_m_df = NULL, chosen_scaler = NULL, scaler_shrinkage = NULL,
                             use_raw_for_eligibility = NULL, exp_ret_score_tilt = exp_ret_score_tilt,
                             exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = cov_estimation_method, cov_matrix_sample_size = cov_matrix_sample_size, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = daily_stock_returns_m_xts, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_list,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                                          chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test RP
  expected_results <- stock_universe_m_d_ref
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  eligible_tickers <- expected_results %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  covariance_matrix <- estimate_covariance_matrix(tickers = eligible_tickers, returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  rp_results <- riskParityPortfolio::riskParityPortfolio(
    Sigma = covariance_matrix,
    mu = stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1L) %>% dplyr::pull(exp_ret_score),
    lmd_mu = exp_ret_score_tilt_eta
  )

  expected_results[which(expected_results$is_eligible == 1), "rel_risk_contr"] <-
    rp_results$relative_risk_contribution[expected_results[which(expected_results$is_eligible == 1), "tickers"]] #Fill only eligibles

  expected_results[which(expected_results$is_eligible == 1), "weights"] <-
    rp_results$w[expected_results[which(expected_results$is_eligible == 1), "tickers"]] #Fill only eligibles

  expected_results$rel_risk_contr[which(is.na(expected_results$rel_risk_contr))] <- 0
  expected_results$weights[which(is.na(expected_results$weights))] <- 0


  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "rp",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                   exp_ret_score_tilt = exp_ret_score_tilt, exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                                   active_returns = FALSE
  )


  expect_equal(results@universe_m_d_ref@data, expected_results)

  #Test that tilt worked (weights should not be equal to pure RP)
  vanilla_rp <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "rp",
                                      returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                      cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                      active_returns = FALSE)@universe_m_d_ref@data
  expect_false(
    identical(results@universe_m_d_ref@data$weights,
              vanilla_rp$weights
              )
    )

  #Test that in relation to vanilla RP, high expected return stocks have a higher weight
  combined <- results@universe_m_d_ref@data %>%
    dplyr::select(tickers, weights) %>%
    dplyr::rename(weights_tilted = weights) %>%
    dplyr::left_join(
      vanilla_rp %>%
        dplyr::select(tickers, weights) %>%
        dplyr::rename(weights_vanilla = weights),
      by = "tickers"
    ) %>%
    dplyr::left_join(
      stock_universe_m_d_ref %>%
        dplyr::select(tickers, exp_ret_score),
      by = "tickers"
    ) %>%
    dplyr::mutate(weight_diff = weights_tilted - weights_vanilla)

  # In the top 10 exp_ret_score stocks, at least 8 should have a positive weight difference
  top_10 <- combined %>%
    dplyr::filter(weights_tilted > 0) %>%
    dplyr::filter(weights_vanilla > 0) %>%
    dplyr::slice_max(order_by = exp_ret_score, n = 10)

  expect_gte(sum(top_10$weight_diff > 0), 8)

  # Test that relative risk contribution is not equal across tilted version
  expect_true(
    sd(results@universe_m_d_ref@data$rel_risk_contr[results@universe_m_d_ref@data$is_eligible == 1]) >
    sd(vanilla_rp$rel_risk_contr[vanilla_rp$is_eligible == 1])
  )


})

test_that("set portfolio weights works for stocks (rp + exp_ret_score_tilt = 'final') - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range and others
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(vol_36m = "short")
  cov_matrix_sample_size <- 60
  cov_estimation_method <- "cc"
  exp_ret_score_tilt <- "final"
  exp_ret_score_tilt_eta <- 0.5

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 6, port_construction_method = "rp",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             min_eligible_assets_fallback = NULL, scaler_m_df = NULL, chosen_scaler = NULL, scaler_shrinkage = NULL,
                             use_raw_for_eligibility = NULL, exp_ret_score_tilt = exp_ret_score_tilt,
                             exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = cov_estimation_method, cov_matrix_sample_size = cov_matrix_sample_size, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = daily_stock_returns_m_xts, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_list,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                                          chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test RP
  expected_results <- stock_universe_m_d_ref
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  eligible_tickers <- expected_results %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  covariance_matrix <- estimate_covariance_matrix(tickers = eligible_tickers, returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  rp_results <- riskParityPortfolio::riskParityPortfolio(Sigma = covariance_matrix)

  expected_results[which(expected_results$is_eligible == 1), "rel_risk_contr"] <-
    rp_results$relative_risk_contribution[expected_results[which(expected_results$is_eligible == 1), "tickers"]] #Fill only eligibles

  expected_results[which(expected_results$is_eligible == 1), "weights"] <-
    rp_results$w[expected_results[which(expected_results$is_eligible == 1), "tickers"]] #Fill only eligibles

  expected_results$rel_risk_contr[which(is.na(expected_results$rel_risk_contr))] <- 0
  expected_results$weights[which(is.na(expected_results$weights))] <- 0

  ## Apply final tilt
  expected_results$weights <- expected_results$weights * (expected_results$exp_ret_score ^ exp_ret_score_tilt_eta)
  expected_results$weights <- expected_results$weights / sum(expected_results$weights)

  ##Recalculate rel risk contribution
  rel_risk_contr <-  relative_risk_contribution(
    weights = expected_results %>% dplyr::filter(is_eligible == 1L) %>% dplyr::pull(weights),
    covariance_matrix
  )
  expected_results$rel_risk_contr <- NULL
  expected_results <- expected_results %>% dplyr::left_join(rel_risk_contr, by = "tickers")
  expected_results$rel_risk_contr[which(is.na(expected_results$rel_risk_contr))] <- 0
  expected_results <- expected_results %>% dplyr::relocate(rel_risk_contr, .before = weights)


  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "rp",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                   exp_ret_score_tilt = exp_ret_score_tilt, exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                                   active_returns = FALSE
  )


  expect_equal(results@universe_m_d_ref@data, expected_results)

  #Test that tilt worked (weights should not be equal to pure RP)
  vanilla_rp <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "rp",
                                      returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                      cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                      active_returns = FALSE)@universe_m_d_ref@data
  expect_false(
    identical(results@universe_m_d_ref@data$weights,
              vanilla_rp$weights
    )
  )

  #Test that in relation to vanilla RP, high expected return stocks have a higher weight
  combined <- results@universe_m_d_ref@data %>%
    dplyr::select(tickers, weights) %>%
    dplyr::rename(weights_tilted = weights) %>%
    dplyr::left_join(
      vanilla_rp %>%
        dplyr::select(tickers, weights) %>%
        dplyr::rename(weights_vanilla = weights),
      by = "tickers"
    ) %>%
    dplyr::left_join(
      stock_universe_m_d_ref %>%
        dplyr::select(tickers, exp_ret_score),
      by = "tickers"
    ) %>%
    dplyr::mutate(weight_diff = weights_tilted - weights_vanilla)

  # In the top 10 exp_ret_score stocks, at least 8 should have a positive weight difference
  top_10 <- combined %>%
    dplyr::filter(weights_tilted > 0) %>%
    dplyr::filter(weights_vanilla > 0) %>%
    dplyr::slice_max(order_by = exp_ret_score, n = 10)

  expect_gte(sum(top_10$weight_diff > 0), 8)

  # Test that relative risk contribution is not equal across tilted version
  expect_true(
    sd(results@universe_m_d_ref@data$rel_risk_contr[results@universe_m_d_ref@data$is_eligible == 1]) >
      sd(vanilla_rp$rel_risk_contr[vanilla_rp$is_eligible == 1])
  )



})

test_that("set portfolio weights works for stocks (hrp and hrp + exp_ret_score_tilt = 'final') - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range and others
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(vol_36m = "short")
  cov_matrix_sample_size <- 60
  cov_estimation_method <- "cc"
  exp_ret_score_tilt <- "final"
  exp_ret_score_tilt_eta <- 0.5

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 6, port_construction_method = "rp",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             min_eligible_assets_fallback = NULL, scaler_m_df = NULL, chosen_scaler = NULL, scaler_shrinkage = NULL,
                             use_raw_for_eligibility = NULL, exp_ret_score_tilt = exp_ret_score_tilt,
                             exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = cov_estimation_method, cov_matrix_sample_size = cov_matrix_sample_size, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = daily_stock_returns_m_xts, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_list,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                                          chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test HRP
  expected_results <- stock_universe_m_d_ref
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  eligible_tickers <- expected_results %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  covariance_matrix <- estimate_covariance_matrix(tickers = eligible_tickers, returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )


  correlation <- stats::cov2cor(covariance_matrix)
  distance <- stats::dist(sqrt(0.5 * (1 - correlation)), method = "euclidean", diag = TRUE, upper = TRUE, p = 2)
  clusters <- stats::hclust(distance , method = "complete", members = NULL)
  clusters_order <- clusters$order

  weights <- rep(1,ncol(covariance_matrix))
  index <- list(clusters_order)

  while (length(index) > 0) {
    index_recreated <- list()
    for (i in index) {
      ### Bisection
      middle <- floor(length(i)/2)
      indexL <- i[1:middle]
      indexR <- i[-c(1:middle)]
      covL <- as.matrix(covariance_matrix[indexL, indexL])
      covR <- as.matrix(covariance_matrix[indexR, indexR])

      ### Calculate weights
      w_L <- 1/diag(covL) / sum(1/diag(covL))
      w_R <- 1/diag(covR) / sum(1/diag(covR))

      ### Variance
      v_L <- as.numeric(t(w_L) %*% covL %*% w_L)
      v_R <- as.numeric(t(w_R) %*% covR %*% w_R)

      ### Alpha
      alpha <- as.numeric(1 - v_L/(v_L + v_R))

      ### Update weights
      weights[indexL] <- weights[indexL] * alpha
      weights[indexR] <- weights[indexR] * (1 - alpha)

      ### Recreate index
      if (length(indexL) > 1) index_recreated <- c(index_recreated, list(indexL))
      if (length(indexR) > 1) index_recreated <- c(index_recreated, list(indexR))
      index = index_recreated
    }
  }

  # Pass pass weights
  names(weights) <- colnames(covariance_matrix)

  expected_results <- expected_results %>%
    dplyr::left_join(
      data.frame(
        tickers = names(weights),
        weights = weights
      ),
      by = "tickers"
    )
  expected_results[which(is.na(expected_results$weights)), "weights"] <- 0

  ## Apply final tilt
  expected_results$weights <- expected_results$weights * (expected_results$exp_ret_score ^ exp_ret_score_tilt_eta)
  expected_results$weights <- expected_results$weights / sum(expected_results$weights)

  ## Recalculare RRC
  rel_risk_contr <-  relative_risk_contribution(
    weights = expected_results %>% dplyr::filter(is_eligible == 1L) %>% dplyr::pull(weights),
    covariance_matrix
  )

  expected_results <- expected_results %>%
    dplyr::left_join(rel_risk_contr, by = "tickers")
  expected_results[which(is.na(expected_results$rel_risk_contr)), "rel_risk_contr"] <- 0

  ## Relocate
  expected_results <- expected_results %>% dplyr::relocate(rel_risk_contr, .before = weights)


  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "hrp",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                   exp_ret_score_tilt = "final", exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                                   active_returns = FALSE
  )
  expect_equal(results@universe_m_d_ref@data, expected_results)

  #Test that tilt worked (weights should not be equal to pure HRP)
  vanilla_hrp <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "hrp",
                                      returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                      cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                      active_returns = FALSE)@universe_m_d_ref@data
  expect_false(
    identical(results@universe_m_d_ref@data$weights,
              vanilla_hrp$weights
    )
  )

  expect_equal(
    weights,
    vanilla_hrp$weights[match(names(weights), vanilla_hrp$tickers)] %>%
      setNames(names(weights))
  )

  #Test that in relation to vanilla HRP, high expected return stocks have a higher weight
  combined <- results@universe_m_d_ref@data %>%
    dplyr::select(tickers, weights) %>%
    dplyr::rename(weights_tilted = weights) %>%
    dplyr::left_join(
      vanilla_hrp %>%
        dplyr::select(tickers, weights) %>%
        dplyr::rename(weights_vanilla = weights),
      by = "tickers"
    ) %>%
    dplyr::left_join(
      stock_universe_m_d_ref %>%
        dplyr::select(tickers, exp_ret_score),
      by = "tickers"
    ) %>%
    dplyr::mutate(weight_diff = weights_tilted - weights_vanilla)

  # In the top 10 exp_ret_score stocks, at least 8 should have a positive weight difference
  top_10 <- combined %>%
    dplyr::filter(weights_tilted > 0) %>%
    dplyr::filter(weights_vanilla > 0) %>%
    dplyr::slice_max(order_by = exp_ret_score, n = 10)

  expect_gte(sum(top_10$weight_diff > 0), 8)

  # In the bottom 10 exp_ret_score stocks, at least 8 should have a negative weight difference
  bottom_10 <- combined %>%
    dplyr::filter(weights_tilted > 0) %>%
    dplyr::filter(weights_vanilla > 0) %>%
    dplyr::slice_min(order_by = exp_ret_score, n = 10)

  expect_gte(sum(bottom_10$weight_diff < 0), 8)


})

test_that("set portfolio weights works for stocks (hrp + exp_ret_score_tilt = 'inner') - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range and others
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(vol_36m = "short")
  cov_matrix_sample_size <- 60
  cov_estimation_method <- "cc"
  exp_ret_score_tilt <- "final"
  exp_ret_score_tilt_eta <- 0.5

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 6, port_construction_method = "rp",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             min_eligible_assets_fallback = NULL, scaler_m_df = NULL, chosen_scaler = NULL, scaler_shrinkage = NULL,
                             use_raw_for_eligibility = NULL, exp_ret_score_tilt = exp_ret_score_tilt,
                             exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = cov_estimation_method, cov_matrix_sample_size = cov_matrix_sample_size, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = daily_stock_returns_m_xts, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_list,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                                          chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test HRP
  expected_results <- stock_universe_m_d_ref
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  eligible_tickers <- expected_results %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  covariance_matrix <- estimate_covariance_matrix(tickers = eligible_tickers, returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )


  correlation <- stats::cov2cor(covariance_matrix)
  distance <- stats::as.dist(sqrt(0.5 * (1 - correlation)))
  clusters <- stats::hclust(distance , method = "complete")
  clusters_order <- clusters$order

  weights <- rep(1,ncol(covariance_matrix))
  index <- list(clusters_order)

  while (length(index) > 0) {
    index_recreated <- list()
    for (i in index) {
      if (length(i) <= 1L) next
      ### Bisection
      middle <- floor(length(i)/2)
      indexL <- i[1:middle]
      indexR <- i[-c(1:middle)]
      covL <- as.matrix(covariance_matrix[indexL, indexL, drop = FALSE])
      covR <- as.matrix(covariance_matrix[indexR, indexR, drop = FALSE])

      ### Calculate weights
      w_L <- 1 / diag(covL); w_L <- w_L / sum(w_L)
      w_R <- 1 / diag(covR); w_R <- w_R / sum(w_R)

      ### Variance
      v_L <- as.numeric(t(w_L) %*% covL %*% w_L)
      v_R <- as.numeric(t(w_R) %*% covR %*% w_R)

      ### Mu
      mu_L <- as.numeric(sum(w_L * expected_results$exp_ret_score[
        match(rownames(covL), expected_results$tickers)
      ]))
      mu_R <- as.numeric(sum(w_R * expected_results$exp_ret_score[
        match(rownames(covR), expected_results$tickers)
      ]))

      ### Rank
      r <- rank(c(mu_L, mu_R), ties.method = "average")
      gL <- r[1]/2
      gR <- r[2]/2

      ### Allocated
      A_L <- (1/v_L)*(gL^exp_ret_score_tilt_eta)
      A_R <- (1/v_R)*(gR^exp_ret_score_tilt_eta)

      ### Alpha
      alpha <- A_L / (A_L + A_R)

      ### Update weights
      weights[indexL] <- weights[indexL] * alpha
      weights[indexR] <- weights[indexR] * (1 - alpha)

      ### Recreate index
      if (length(indexL) > 1) index_recreated <- c(index_recreated, list(indexL))
      if (length(indexR) > 1) index_recreated <- c(index_recreated, list(indexR))

    }
    index <- index_recreated
  }

  # Pass pass weights
  names(weights) <- colnames(covariance_matrix)

  expected_results <- expected_results %>%
    dplyr::left_join(
      data.frame(
        tickers = names(weights),
        weights = weights
      ),
      by = "tickers"
    )
  expected_results[which(is.na(expected_results$weights)), "weights"] <- 0


  ## Recalculare RRC
  rel_risk_contr <-  relative_risk_contribution(
    weights = expected_results %>% dplyr::filter(is_eligible == 1L) %>% dplyr::pull(weights),
    covariance_matrix
  )

  expected_results <- expected_results %>%
    dplyr::left_join(rel_risk_contr, by = "tickers")
  expected_results[which(is.na(expected_results$rel_risk_contr)), "rel_risk_contr"] <- 0


  ## Relocate
  expected_results <- expected_results %>% dplyr::relocate(rel_risk_contr, .before = weights)


  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "hrp",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                   exp_ret_score_tilt = "inner", exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
                                   linkage = "complete",
                                   active_returns = FALSE
  )
  expect_equal(results@universe_m_d_ref@data, expected_results)

  #Test that tilt worked (weights should not be equal to pure HRP)
  vanilla_hrp <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "hrp",
                                       returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                       cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                       active_returns = FALSE)@universe_m_d_ref@data
  expect_false(
    identical(results@universe_m_d_ref@data$weights,
              vanilla_hrp$weights
    )
  )


  #Test that in relation to vanilla HRP, high expected return stocks have a higher weight
  combined <- results@universe_m_d_ref@data %>%
    dplyr::select(tickers, weights) %>%
    dplyr::rename(weights_tilted = weights) %>%
    dplyr::left_join(
      vanilla_hrp %>%
        dplyr::select(tickers, weights) %>%
        dplyr::rename(weights_vanilla = weights),
      by = "tickers"
    ) %>%
    dplyr::left_join(
      stock_universe_m_d_ref %>%
        dplyr::select(tickers, exp_ret_score),
      by = "tickers"
    ) %>%
    dplyr::mutate(weight_diff = weights_tilted - weights_vanilla)

  # In the top 10 exp_ret_score stocks, at least 8 should have a positive weight difference
  top_10 <- combined %>%
    dplyr::filter(weights_tilted > 0) %>%
    dplyr::filter(weights_vanilla > 0) %>%
    dplyr::slice_max(order_by = exp_ret_score, n = 10)

  expect_gte(sum(top_10$weight_diff > 0), 8)

  # In the bottom 10 exp_ret_score stocks, at least 8 should have a negative weight difference
  bottom_10 <- combined %>%
    dplyr::filter(weights_tilted > 0) %>%
    dplyr::filter(weights_vanilla > 0) %>%
    dplyr::slice_min(order_by = exp_ret_score, n = 10)

  expect_gte(sum(bottom_10$weight_diff < 0), 8)


})

test_that("set portfolio weights works for stocks (mvo_unc) - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range and others
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(vol_36m = "short")
  cov_matrix_sample_size <- 60
  cov_estimation_method <- "cc"

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 6, port_construction_method = "mvo",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             min_eligible_assets_fallback = NULL, scaler_m_df = NULL, chosen_scaler = NULL, scaler_shrinkage = NULL,
                             use_raw_for_eligibility = NULL, exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = cov_estimation_method, cov_matrix_sample_size = cov_matrix_sample_size, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = daily_stock_returns_m_xts, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_list,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )


  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test MVO Unconstrained
  expected_results <- stock_universe_m_d_ref
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  eligible_tickers <- expected_results %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  covariance_matrix <- estimate_covariance_matrix(tickers = eligible_tickers, returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )


  #Portfolio
  port_spec <- PortfolioAnalytics::portfolio.spec(assets = eligible_tickers)
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "full_investment")
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "box")

  set.seed(123)
  rp_weights <- PortfolioAnalytics::random_portfolios(portfolio = port_spec_constrained,
                                                      permutations = 2000,
                                                      rp_method = "sample")

  #Best Portfolio for Sharpe
  rp_weights <- as.matrix(rp_weights)  # Portfolio weights
  exp_ret_score <- as.matrix(stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))  # Expected return vector
  cov_matrix <- as.matrix(covariance_matrix)  # Covariance matrix
  # Calculate Portfolio Return (Expected Return)
  portfolio_return <- rp_weights %*% exp_ret_score  # Matrix multiplication
  # Calculate Portfolio Risk (Standard Deviation)
  portfolio_risk <- sqrt(rowSums((rp_weights %*% cov_matrix) * rp_weights))
  # Optimal Sharpe
  optimal_sharpe_weights <- rp_weights[which.max(portfolio_return/portfolio_risk),]
  optimal_ret <-  portfolio_return[which.max(portfolio_return/portfolio_risk),]
  optimal_risk <- portfolio_risk[which.max(portfolio_return/portfolio_risk)]

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "mvo",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method
  )

  #Check for random weights generation
  expected_weights <- t(rp_weights) %>% as.data.frame() %>% tibble::rownames_to_column("tickers")
  expect_equal(results@random_port_weights, expected_weights)
  expect_equal(results@weights, optimal_sharpe_weights %>% unname())
  expect_equal(1.907, optimal_ret, tolerance = 1e-2)
  expect_equal(0.547, optimal_risk, tolerance = 1e-2)

  #Best Portfolio for Return
  optimal_ret_weights <-  rp_weights[which.max(portfolio_return),]

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "mvo",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                   opt_objective = "return"
  )
  expect_equal(results@weights, optimal_ret_weights %>% unname())

  #Best Portfolio for Risk
  optimal_risk_weights <-  rp_weights[which.min(portfolio_risk),]

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "mvo",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method,
                                   opt_objective = "risk"
  )
  expect_equal(results@weights, optimal_risk_weights %>% unname())
  expected_results[which(expected_results$is_eligible == 1), "rel_risk_contr"] <- relative_risk_contribution(optimal_risk_weights %>% unname(), covariance_matrix)$rel_risk_contr
  expected_results[which(expected_results$is_eligible == 0), "rel_risk_contr"] <- 0

  expected_results[which(expected_results$is_eligible == 1), "weights"] <- optimal_risk_weights
  expected_results[which(expected_results$is_eligible == 0), "weights"] <- 0


  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)


})

test_that("set portfolio weights works for stocks (mvo_con) - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test MVO Constrained
  expected_results <- stock_universe_m_d_ref
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  eligible_tickers <- expected_results %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  covariance_matrix <- estimate_covariance_matrix(tickers = eligible_tickers, returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 60, cov_estimation_method = "cc",
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )


  #Portfolio
  port_spec <- PortfolioAnalytics::portfolio.spec(assets = eligible_tickers)
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "full_investment")
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "box")

  #Box constraints
  eligible_universe_m_d_ref <- generate_box_constraints(universe_m_d_ref = stock_universe_m_d_ref,
                                                        liquidity_constraint_policy = liquidity_constraint_policy,
                                                        concentration_constraint_policy = concentration_constraint_policy)

  port_spec_constrained <- PortfolioAnalytics::add.constraint(type = "box", portfolio = port_spec_constrained,
                                                              min = eligible_universe_m_d_ref$min_weight,
                                                              max = eligible_universe_m_d_ref$max_weight)

  expected_results[which(expected_results$is_eligible == 1), "max_weight"] <- eligible_universe_m_d_ref$max_weight
  expected_results[which(expected_results$is_eligible == 0), "max_weight"] <- 0

  expected_results[which(expected_results$is_eligible == 1), "min_weight"] <- eligible_universe_m_d_ref$min_weight
  expected_results[which(expected_results$is_eligible == 0), "min_weight"] <- 0


  #Group constraints
  group_constraints_helper <- generate_group_constraints(universe_m_d_ref = stock_universe_m_d_ref, concentration_constraint_policy = concentration_constraint_policy,
                                                         groups_m_d_ref = stock_groups_m_d_ref)

  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec_constrained,
                                                              type = "group",
                                                              groups = group_constraints_helper$eligible_assets_group_membership_list,
                                                              group_min = group_constraints_helper$group_constraint_min,
                                                              group_max = group_constraints_helper$group_constraint_max
  )

  set.seed(123)
  rp_weights <- PortfolioAnalytics::random_portfolios(portfolio = port_spec_constrained,
                                                      permutations = 2000,
                                                      rp_method = "sample")

  #Best Portfolio for Sharpe
  rp_weights <- as.matrix(rp_weights)  # Portfolio weights
  exp_ret_score <- as.matrix(stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))  # Expected return vector
  cov_matrix <- as.matrix(covariance_matrix)  # Covariance matrix
  # Calculate Portfolio Return (Expected Return)
  portfolio_return <- rp_weights %*% exp_ret_score  # Matrix multiplication
  # Calculate Portfolio Risk (Standard Deviation)
  portfolio_risk <- sqrt(rowSums((rp_weights %*% cov_matrix) * rp_weights))
  # Optimal Sharpe
  optimal_sharpe_weights <- rp_weights[which.max(portfolio_return/portfolio_risk),]
  optimal_ret <-  portfolio_return[which.max(portfolio_return/portfolio_risk),]
  optimal_risk <- portfolio_risk[which.max(portfolio_return/portfolio_risk)]

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "mvo",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = 60, cov_estimation_method = "cc",
                                   liquidity_constraint_policy = liquidity_constraint_policy,
                                   concentration_constraint_policy = concentration_constraint_policy
  )

  #Check for random weights generation
  expected_weights <- t(rp_weights) %>% as.data.frame() %>% tibble::rownames_to_column("tickers")
  expect_equal(results@random_port_weights, expected_weights)
  expect_equal(results@weights, optimal_sharpe_weights %>% unname())
  expect_equal(1.911, optimal_ret, tolerance = 1e-2)
  expect_equal(0.574, optimal_risk, tolerance = 1e-2)

  #Best Portfolio for Return
  optimal_ret_weights <-  rp_weights[which.max(portfolio_return),]

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "mvo",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = 60, cov_estimation_method = "cc",
                                   opt_objective = "return",
                                   liquidity_constraint_policy = liquidity_constraint_policy,
                                   concentration_constraint_policy = concentration_constraint_policy

  )
  expect_equal(results@weights, optimal_ret_weights %>% unname())

  #Best Portfolio for Risk
  optimal_risk_weights <-  rp_weights[which.min(portfolio_risk),]

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "mvo",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = 60, cov_estimation_method = "cc",
                                   opt_objective = "risk",
                                   liquidity_constraint_policy = liquidity_constraint_policy,
                                   concentration_constraint_policy = concentration_constraint_policy

  )

  expect_equal(results@weights, optimal_risk_weights %>% unname())
  expected_results[which(expected_results$is_eligible == 1), "rel_risk_contr"] <- relative_risk_contribution(optimal_risk_weights %>% unname(), covariance_matrix)$rel_risk_contr
  expected_results[which(expected_results$is_eligible == 0), "rel_risk_contr"] <- 0

  expected_results[which(expected_results$is_eligible == 1), "weights"] <- optimal_risk_weights
  expected_results[which(expected_results$is_eligible == 0), "weights"] <- 0

  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)


  #Check that constraints match expectations
  #Upper box
  expect_true(
    all(
      sapply(seq_len(ncol(rp_weights)), function(j) {
        all(rp_weights[, j] <= eligible_universe_m_d_ref$max_weight[j])
      })
    )
  )

  #Lower box
  expect_true(
    all(
      sapply(seq_len(ncol(rp_weights)), function(j) {
        all(rp_weights[, j] >= eligible_universe_m_d_ref$min_weight[j])
      })
    )
  )

  #Group constraints
  groups <- c("Agro", "Bancos e Serviços Financeiros", "Consumo Cíclico", "Consumo Não-Cíclico", "Indústria", "Materiais Básicos",
              "Petróleo gás e biocombustíveis", "Utilidade Pública")
  #Check if all group weights match
  for(i in 1:length(groups)){
    sector_tickers <- eligible_universe_m_d_ref %>% dplyr::filter(sectors == groups[i]) %>% dplyr::pull(tickers)
    sector_weights <- rp_weights[, sector_tickers] %>% rowSums()
    expect_true(all(sector_weights <= group_constraints_helper$group_constraint_max[i]))
    expect_true(all(sector_weights >= group_constraints_helper$group_constraint_min[i]))
  }

  #Group constraints
  groups <- c("Doméstico Cíclico", "Doméstico Defensivo", "Exportador", "Indústria")
  #Check if all group weights match
  for(i in 9:(9 + length(groups) - 1)){
    sector_tickers <- eligible_universe_m_d_ref %>% dplyr::filter(macro_sector == groups[i - 8]) %>% dplyr::pull(tickers)
    sector_weights <- rp_weights[, sector_tickers] %>% rowSums()
    expect_true(all(sector_weights <= group_constraints_helper$group_constraint_max[i]))
    expect_true(all(sector_weights >= group_constraints_helper$group_constraint_min[i]))
  }

})

test_that("set portfolio weights works for stocks (mvo_con + resampling + ridge_pen) - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  ridge_pen <- 1
  n_resamples <- 3
  exp_ret_score_jitter <- 0.02
  cov_jitter <- 0.01
  concentration_constraint_policy$max_abs_active_group_weight <- NULL

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                                          chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Set ibov_bench_weights as target_port_m_d_ref
  target_port_m_d_ref <- stock_universe_m_d_ref %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::left_join(benchmark_weights_m_d_ref %>%
                       dplyr::select(id, ibov), by = "id") %>%
    dplyr::rename(target_weights = ibov)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    target_port_m_d_ref = target_port_m_d_ref,
    ridge_pen = ridge_pen,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test MVO Constrained
  expected_results <- stock_universe_m_d_ref
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  eligible_tickers <- expected_results %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  covariance_matrix <- estimate_covariance_matrix(tickers = eligible_tickers, returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 60, cov_estimation_method = "cc",
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  set.seed(123)
  optimal_weights_list <- list()
  jittered_exp_ret_score_list <- list()
  jittered_cov_matrix_list <- list()
  rp_weights_list <- list()
  for(i in seq_len(n_resamples + 1)){
    #Portfolio
    port_spec <- PortfolioAnalytics::portfolio.spec(assets = eligible_tickers)
    port_spec_constr <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "full_investment")
    port_spec_constr <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "box")

    #Box constraints
    eligible_universe_m_d_ref <- generate_box_constraints(universe_m_d_ref = stock_universe_m_d_ref,
                                                          liquidity_constraint_policy = liquidity_constraint_policy,
                                                          concentration_constraint_policy = concentration_constraint_policy)

    port_spec_constr <- PortfolioAnalytics::add.constraint(type = "box", portfolio = port_spec_constr,
                                                                min = eligible_universe_m_d_ref$min_weight,
                                                                max = eligible_universe_m_d_ref$max_weight)

    expected_results[which(expected_results$is_eligible == 1), "max_weight"] <- eligible_universe_m_d_ref$max_weight
    expected_results[which(expected_results$is_eligible == 0), "max_weight"] <- 0

    expected_results[which(expected_results$is_eligible == 1), "min_weight"] <- eligible_universe_m_d_ref$min_weight
    expected_results[which(expected_results$is_eligible == 0), "min_weight"] <- 0

    #Metrics
    exp_ret_score <- as.matrix(stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score))  # Expected return vector
    #Jitter exp_ret_score
    if (i == 1){
      jittered_exp_ret_score <- exp_ret_score  #No jitter for first iteration
    } else {
      jittered_exp_ret_score <- exp_ret_score + stats::rnorm(length(exp_ret_score), mean = 0,
                                                             sd = exp_ret_score_jitter * sd(exp_ret_score))
    }

    jittered_exp_ret_score_list[[i]] <- jittered_exp_ret_score

    cov_matrix <- as.matrix(covariance_matrix)  # Covariance matrix

    #Jitter cov matrix
    if (i == 1){
      jittered_cov_matrix <- cov_matrix  #No jitter for first iteration
    } else {
      ev <- eigen(cov_matrix, symmetric = TRUE)
      mult <- exp(stats::rnorm(n = length(ev$values), mean = 0, sd = cov_jitter))
      jittered_cov_matrix <- ev$vectors %*% diag(ev$values * mult) %*% t(ev$vectors)
      dimnames(jittered_cov_matrix) <- dimnames(cov_matrix)
    }
    jittered_cov_matrix_list[[i]] <- jittered_cov_matrix

    rp_weights <- PortfolioAnalytics::random_portfolios(portfolio = port_spec_constr,
                                                        permutations = 2000,
                                                        rp_method = "sample")

    rp_weights_list[[i]] <- rp_weights

    #Best Portfolio for Risk
    rp_weights <- as.matrix(rp_weights)  # Portfolio weights

    # Calculate Portfolio Return (Expected Return)
    portfolio_return <- rp_weights %*% jittered_exp_ret_score  # Matrix multiplication
    # Calculate Portfolio Risk (Standard Deviation)
    portfolio_risk <- sqrt(rowSums((rp_weights %*% jittered_cov_matrix) * rp_weights))
    # Difference to target_port
    portfolio_diff <- data.frame(t(rp_weights)) %>%
      tibble::rownames_to_column("tickers") %>%
      dplyr::left_join(target_port_m_d_ref %>% dplyr::select(tickers, target_weights), by = "tickers")

    target_port <- portfolio_diff %>% dplyr::select(tickers, target_weights)
    portfolio_diff <- dplyr::select(portfolio_diff, -tickers, -target_weights) - target_port$target_weights
    ## Sum squared difference
    sum_squared_diff <- colSums(portfolio_diff^2)

    # Combine with risk
    combined_metric <- (portfolio_risk) + (ridge_pen * sum_squared_diff)

    # Optimal
    optimal_risk <- combined_metric[which.min(combined_metric)]
    optimal_weights <- rp_weights[which.min(combined_metric),]

    optimal_weights_list[[i]] <- optimal_weights

  }

  #Take the average of weights
  avg_weights <- Reduce("+", optimal_weights_list) / length(optimal_weights_list)

  #Join with expected results
  base_weights_df <- data.frame(
    tickers = names(optimal_weights_list[[1]]),
    base_weights = optimal_weights_list[[1]] %>% unname()
  )
  expected_results <- expected_results %>%
    dplyr::left_join(base_weights_df, by = "tickers")
  expected_results$base_weights[which(is.na(expected_results$base_weights))] <- 0

  avg_weights_df <- data.frame(
    tickers = names(avg_weights),
    weights = avg_weights %>% unname()
  )
  expected_results <- expected_results %>%
    dplyr::left_join(avg_weights_df, by = "tickers")
  expected_results$weights[which(is.na(expected_results$weights))] <- 0

  #Rel risk contribution
  rrc <- relative_risk_contribution(
    weights = expected_results %>% dplyr::filter(is_eligible == 1L) %>% dplyr::pull(weights),
    covariance_matrix = covariance_matrix
  )

  expected_results <- expected_results %>%
    dplyr::left_join(rrc, by = "tickers")
  expected_results$rel_risk_contr[which(is.na(expected_results$rel_risk_contr))] <- 0
  expected_results <- expected_results %>%
    dplyr::relocate(rel_risk_contr, .before = weights)



  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "mvo",
                                   returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, groups_m_d_ref = stock_groups_m_d_ref,
                                   cov_matrix_sample_size = 60, cov_estimation_method = "cc",
                                   opt_objective = "risk", ridge_pen = ridge_pen,
                                   n_resamples = n_resamples,
                                   liquidity_constraint_policy = liquidity_constraint_policy,
                                   concentration_constraint_policy = concentration_constraint_policy
  )


  # Test that base weights match
  expect_equal(expected_results, results@universe_m_d_ref@data)

  # Test that base and optimal do not match
  expect_false(all(results@universe_m_d_ref@data$base_weights == results@universe_m_d_ref@data$weights))

  # Test that base solution is not the one that minimizes risk
  risk_vec <- vector(length = ncol(results@random_port_weights) - 1)
  for(i in seq_len(ncol(results@random_port_weights))){
    if (i == 1) next
    weights_i <- results@random_port_weights[, i] %>% unname()
    risk_i <- sqrt(t(weights_i) %*% covariance_matrix %*% weights_i)
    risk_vec[i] <- risk_i
  }
  base_port_risk <- sqrt(
    t(results@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 1L) %>% dplyr::pull(base_weights))
    %*% covariance_matrix %*%
      (results@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 1L) %>% dplyr::pull(base_weights))
  )

  expect_gte(base_port_risk, min(risk_vec[-1]))


  #Check that constraints match expectations
  #Upper box
  expect_true(all(results@universe_m_d_ref@data$base_weights <= results@universe_m_d_ref@data$max_weight))

  #Lower box
  expect_true(all(results@universe_m_d_ref@data$base_weights >= results@universe_m_d_ref@data$min_weight))


})

test_that("set portfolio weights works for stocks (custom_weights) - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range and other preps
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(vol_36m = "short")
  custom_stock_weights_m_df <- benchmark_weights_m_df %>% dplyr::rename(weights = ibov)


  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 6, port_construction_method = "custom_weights",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             min_eligible_assets_fallback = NULL,
                             scaler_m_df = NULL, chosen_scaler = NULL, scaler_shrinkage = NULL,
                             use_raw_for_eligibility = NULL, exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = NULL, cov_matrix_sample_size = NULL, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_list,
                             custom_stock_weights_m_df = custom_stock_weights_m_df, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  custom_stock_weights_m_d_ref <- custom_stock_weights_m_df %>% dplyr::filter(dates == current_date)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #Test custom weights
  expected_results <- stock_universe_m_d_ref
  expected_results <- expected_results %>% dplyr::left_join(custom_stock_weights_m_d_ref %>% dplyr::select(-tickers, -dates), by = "id")

  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "custom_weights",
                                   custom_weights_m_d_ref = custom_stock_weights_m_d_ref)

  expect_equal(results@universe_m_d_ref@data, expected_results)
  expect_equal(results@universe_m_d_ref@data$weights %>% sum(), 1)
  expect_equal(results@universe_m_d_ref@data$weights, results@universe_m_d_ref@data$ibov_bench_weights)

})












