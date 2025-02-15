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
                             min_eligible_assets_fallback = NULL,
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
                             min_eligible_assets_fallback = NULL,
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
                             min_eligible_assets_fallback = NULL,
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
                             min_eligible_assets_fallback = NULL,
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
                             min_eligible_assets_fallback = NULL,
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
                             min_eligible_assets_fallback = NULL,
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












