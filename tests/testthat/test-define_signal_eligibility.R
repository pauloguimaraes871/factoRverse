test_that("define_signal_elibility works for frequentist setting", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-06-15"

  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  target_m_upd_ref <- target_m_df[which(target_m_df$dates <= current_date),]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]
  selected_benchmark_returns_upd_ref <- benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)]
  priors_m_upd_ref_list <- list(jkp_emerging = priors_m_df_list$jkp_emerging[which(priors_m_df_list$jkp_emerging$dates <= current_date), ])
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]


  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(signal_selection_policy = signal_selection_policy, signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref)
  selected_signals_backtest_returns_upd_ref <- selected_signals_and_backtest_list$selected_signals_backtest_returns_upd_ref

  #Select priors
  selected_priors_informative_data_m_upd_ref <- priors_m_upd_ref_list[[signal_selection_policy$chosen_informative_data]]

  #expected results
  expected_result <- data.frame(id = paste0(colnames(selected_signals_backtest_returns_upd_ref)[-1],"-",current_date), tickers = colnames(selected_signals_backtest_returns_upd_ref)[-1], dates = current_date)
  expected_result$dates <- as.Date(expected_result$dates, format = "%Y-%m-%d")
  expected_result$mean_active_return <- selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x) mean(x))
  expected_result$tracking_error <- selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x) sd(x))
  expected_result$IR <- expected_result$mean_active_return/expected_result$tracking_error

  lm_model_summary_list <- purrr::map(lapply(selected_signals_backtest_returns_upd_ref[,-1], as.vector), ~ summary(lm(.x ~ selected_benchmark_returns_upd_ref$IBOV)))

  expected_result$alpha <- sapply(lm_model_summary_list, function(x) x$coefficients[1])
  expected_result$AP <- sapply(lm_model_summary_list, function(x) x$coefficients[5])
  expected_result$beta <- sapply(lm_model_summary_list, function(x) x$coefficients[2])
  expected_result$treynor <- expected_result$mean_active_return/expected_result$beta
  expected_result$p_value <- sapply(lm_model_summary_list, function(x) x$coefficients[7])
  expected_result$adjusted_p_value <- p.adjust(expected_result$p_value, signal_selection_policy$p_correction_method)

  #final signal
  expected_result$final_signal <- signal_transform(expected_result[, signal_selection_policy$chosen_sb_metric], upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization)


  #Classify
  concentration_constraint_policy_test <- list(benchmark = signal_selection_policy$sb_benchmark_weighting, max_abs_active_group_weight = signal_selection_policy$max_abs_active_group_weight)
  expected_result <- list(
    signal_universe_m_d_ref = classify_investment_universe(expected_result, signal_significance_threshold = signal_selection_policy$signal_significance_threshold,
                                                            groups_m_d_ref = signals_groups_m_d_ref, concentration_constraint_policy = concentration_constraint_policy_test,
                                                            asset_object = "signals")
  )

  #results
  results <- define_signal_eligibility(selected_signals_backtest_returns_upd_ref = selected_signals_backtest_returns_upd_ref,
                                       selected_benchmark_returns_upd_ref = selected_benchmark_returns_upd_ref,
                                       signal_selection_policy = signal_selection_policy,
                                       signals_groups_m_d_ref = signals_groups_m_d_ref)

  expect_equal(results,expected_result)


})

test_that("define_signal_elibility works for frequentist setting when there is a backtest with short length", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-06-15"

  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  target_m_upd_ref <- target_m_df[which(target_m_df$dates <= current_date),]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]
  selected_benchmark_returns_upd_ref <- benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)]
  priors_m_upd_ref_list <- list(jkp_emerging = priors_m_df_list$jkp_emerging[which(priors_m_df_list$jkp_emerging$dates <= current_date), ])
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]

  #repalce with NA
  backtest_returns_upd_ref$low_Beta[1:3] <- NA

  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(signal_selection_policy = signal_selection_policy, signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref)
  selected_signals_backtest_returns_upd_ref <- selected_signals_and_backtest_list$selected_signals_backtest_returns_upd_ref

  #Select priors
  selected_priors_informative_data_m_upd_ref <- priors_m_upd_ref_list[[signal_selection_policy$chosen_informative_data]]

  #expected results
  expected_result <- data.frame(id = paste0(colnames(selected_signals_backtest_returns_upd_ref)[-1],"-",current_date), tickers = colnames(selected_signals_backtest_returns_upd_ref)[-1], dates = current_date)
  expected_result$dates <- as.Date(expected_result$dates, format = "%Y-%m-%d")
  expected_result$mean_active_return <- selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x) mean(x))
  expected_result$tracking_error <- selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x) sd(x))
  expected_result$IR <- expected_result$mean_active_return/expected_result$tracking_error

  lm_model_summary_list <- purrr::map(lapply(selected_signals_backtest_returns_upd_ref[,-1], as.vector), ~ summary(lm(.x ~ selected_benchmark_returns_upd_ref$IBOV)))

  expected_result$alpha <- sapply(lm_model_summary_list, function(x) x$coefficients[1])
  expected_result$AP <- sapply(lm_model_summary_list, function(x) x$coefficients[5])
  expected_result$beta <- sapply(lm_model_summary_list, function(x) x$coefficients[2])
  expected_result$treynor <- expected_result$mean_active_return/expected_result$beta
  expected_result$p_value <- sapply(lm_model_summary_list, function(x) x$coefficients[7])
  expected_result$adjusted_p_value <- p.adjust(expected_result$p_value, signal_selection_policy$p_correction_method)

  #adjust for backtset with inadequate length
  expected_result[2,-c(1:3)] <- NA

  #final signal
  expected_result$final_signal <- signal_transform(expected_result[, signal_selection_policy$chosen_sb_metric], upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization)


  #Classify
  concentration_constraint_policy_test <- list(benchmark = signal_selection_policy$sb_benchmark_weighting, max_abs_active_group_weight = signal_selection_policy$max_abs_active_group_weight)
  expected_result <- list(
    signal_universe_m_d_ref = classify_investment_universe(expected_result, signal_significance_threshold = signal_selection_policy$signal_significance_threshold,
                                                  groups_m_d_ref = signals_groups_m_d_ref, concentration_constraint_policy = concentration_constraint_policy_test,
                                                  asset_object = "signals")
  )

  expect_equal(
    suppressWarnings(define_signal_eligibility(selected_signals_backtest_returns_upd_ref = selected_signals_backtest_returns_upd_ref,
                                         selected_benchmark_returns_upd_ref = selected_benchmark_returns_upd_ref,
                                         signal_selection_policy = signal_selection_policy,
                                         signals_groups_m_d_ref = signals_groups_m_d_ref)),
               expected_result)


})

test_that("define_signal_elibility works for bayesian setting", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-06-15"

  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  target_m_upd_ref <- target_m_df[which(target_m_df$dates <= current_date),]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]

  selected_benchmark_returns_upd_ref <- benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)]
  priors_m_upd_ref_list <- list(jkp_emerging = priors_m_df_list$jkp_emerging[which(priors_m_df_list$jkp_emerging$dates <= current_date), ])
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]

  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(signal_selection_policy = signal_selection_policy, signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref)
  selected_signals_backtest_returns_upd_ref <- selected_signals_and_backtest_list$selected_signals_backtest_returns_upd_ref

  #Select priors
  selected_priors_informative_data_m_upd_ref <- priors_m_upd_ref_list[[signal_selection_policy$chosen_informative_data]]

  #expected results
  expected_result <- data.frame(id = paste0(colnames(selected_signals_backtest_returns_upd_ref)[-1],"-",current_date), tickers = colnames(selected_signals_backtest_returns_upd_ref)[-1], dates = current_date)
  expected_result$dates <- as.Date(expected_result$dates, format = "%Y-%m-%d")
  expected_result$mean_active_return <- selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x) mean(x))
  expected_result$tracking_error <- selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x) sd(x))
  expected_result$IR <- expected_result$mean_active_return/expected_result$tracking_error

  lm_model_summary_list <- purrr::map(lapply(selected_signals_backtest_returns_upd_ref[,-1], as.vector), ~ summary(lm(.x ~ selected_benchmark_returns_upd_ref$IBOV)))

  expected_result$alpha <- sapply(lm_model_summary_list, function(x) x$coefficients[1])
  expected_result$AP <- sapply(lm_model_summary_list, function(x) x$coefficients[5])
  expected_result$beta <- sapply(lm_model_summary_list, function(x) x$coefficients[2])
  expected_result$treynor <- expected_result$mean_active_return/expected_result$beta
  expected_result$p_value <- sapply(lm_model_summary_list, function(x) x$coefficients[7])

  #Bayesian adjustment
  set.seed(123)
  bayesian_results <- suppressWarnings(
    bayesian_adjustment(
    signal_universe_m_d_ref = expected_result,
    selected_signals_backtest_returns_upd_ref = selected_signals_backtest_returns_upd_ref,
    selected_benchmark_returns_vector_upd_ref = selected_benchmark_returns_upd_ref$IBOV,
    selected_priors_informative_data_m_upd_ref = selected_priors_informative_data_m_upd_ref,
    priors_type = signal_selection_policy$priors_type,
    signals_groups_m_d_ref = signals_groups_m_d_ref
  )
  )

  expected_result <- bayesian_results$posterior_signal_universe_m_d_ref
  #final signal
  expected_result$final_signal <- signal_transform(expected_result[, paste0("posterior_", signal_selection_policy$chosen_sb_metric)],
                                                   lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization)

  #Classify
  concentration_constraint_policy_test <- list(benchmark = signal_selection_policy$sb_benchmark_weighting, max_abs_active_group_weight = signal_selection_policy$max_abs_active_group_weight)
  #Adjust significance threshold to have at least one significant signal
  signal_selection_policy$signal_significance_threshold <- 1.30 #this makes no sense
  signal_selection_policy$p_correction_method <- "bayesian" #this makes no sense

  expected_result <- list(
    signal_universe_m_d_ref = classify_investment_universe(expected_result, signal_significance_threshold = signal_selection_policy$signal_significance_threshold,
                                                           groups_m_d_ref = signals_groups_m_d_ref, concentration_constraint_policy = concentration_constraint_policy_test,
                                                           asset_object = "signals"),
    bayesian_fit_list = bayesian_results$bayesian_fit_list
  )

  #results
  set.seed(123)
  results <- suppressWarnings(define_signal_eligibility(
    selected_signals_backtest_returns_upd_ref = selected_signals_backtest_returns_upd_ref,
    selected_benchmark_returns_upd_ref = selected_benchmark_returns_upd_ref,
    signal_selection_policy = signal_selection_policy,
    signals_groups_m_d_ref = signals_groups_m_d_ref,
    selected_priors_informative_data_m_upd_ref = selected_priors_informative_data_m_upd_ref
  ))

  expect_equal(results$signal_universe_m_d_ref, expected_result$signal_universe_m_d_ref)


})
