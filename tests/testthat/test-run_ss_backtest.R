test_that("run_ss_backtest works for vanilla frequentist setting", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))

  set.seed(123)
  mocked_backtest_returns_df <- data.frame(
    dates = unique(signals_m_df$dates),
    book_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.035),
    dy_med_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.025),
    eps_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.005, sd = 0.03),
    mom_res_12m = rnorm(length(unique(signals_m_df$dates)), mean = 0.015, sd = 0.035),
    roe_3m = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.02),
    sharpe_6m = rnorm(length(unique(signals_m_df$dates)), mean = 0.025, sd = 0.035),
    low_vol_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.0075)
  )

  #Change EPS Yield to be in second rebalancing
  mocked_backtest_returns_df$eps_yield[c(7:9)] <- c(3,3,3)

  data_availability_cutoff <- 6
  rebalancing_months <- 6
  split_method <- "expanding"
  market_factor_proxy <- "IBOV"
  signal_selection_policy$signal_significance_threshold <- 0.15



  #Run SS Backtest
  results <- run_ss_backtest_internal(
    #Dates
    rebalancing_months = rebalancing_months, data_availability_cutoff = data_availability_cutoff, split_method = split_method,
    #Signals
    signals_m_df = signals_m_df,
    chosen_signals = signal_selection_policy$chosen_signals, signal_positions = signal_selection_policy$signal_positions,
    #Backest and benchmarks
    backtest_returns_df = mocked_backtest_returns_df, benchmark_returns_df = benchmark_returns_df, market_factor_proxy = market_factor_proxy,
    #P-value
    p_correction_method = signal_selection_policy$p_correction_method,
    signal_significance_threshold = signal_selection_policy$signal_significance_threshold,
    #Themes representativeness
    enable_theme_representativeness = signal_selection_policy$enable_theme_representativeness,
    #Bayes Rules!
    priors_m_df = NULL, user_priors = NULL,
    model_spec_theme_level = "random_intercept", brms_control = list(iter = 2000, chains = 4, thin = 1, seed = NA, adapt_delta = 0.99),
    lmer_optimizer = "nloptwrap", v = 30,
    #Signal Themes
    signal_themes_m_df = signal_themes_m_df,
    lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization,
    verbose = TRUE
  )

  ####Expected Results
  ####################
  ####################
  signal_universe_m_d_ref_list <- list()
  bayesian_fit_nested_list <- list()
  eligible_signals_list <- list()
  dates_m_vector <- unique(signals_m_df$dates)

  check_inputs_ss_backtest(rebalancing_months = rebalancing_months, data_availability_cutoff = data_availability_cutoff,
                           signals_m_df = signals_m_df,
                           chosen_signals = signal_selection_policy$chosen_signals, signal_positions = signal_selection_policy$signal_positions,
                           backtest_returns_df = mocked_backtest_returns_df, benchmark_returns_df = benchmark_returns_df,
                           p_correction_method = signal_selection_policy$p_correction_method,
                           enable_theme_representativeness = TRUE, market_factor_proxy = market_factor_proxy,
                           signal_significance_threshold = signal_selection_policy$signal_significance_threshold,
                           priors_m_df = NULL, signal_themes_m_df = signal_themes_m_df
                           )

  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    chosen_signals = signal_selection_policy$chosen_signals, signal_positions = signal_selection_policy$signal_positions,
    backtest_returns_df = mocked_backtest_returns_df
  )

  expect_equal(selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_df,
               mocked_backtest_returns_df[, c("dates", "book_yield", "eps_yield", "roe_3m", "sharpe_6m", "low_vol_36m")])

  expect_equal(selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df[,c("id", "tickers", "dates", "book_yield", "eps_yield", "roe_3m", "sharpe_6m")],
               signals_m_df[,c("id", "tickers", "dates", "book_yield", "eps_yield", "roe_3m", "sharpe_6m")])

  expect_equal(selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df$low_vol_36m,
               signals_m_df$vol_36m * -1)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_df <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_df
  selected_market_factor_proxy_df <- benchmark_returns_df[, c("dates", market_factor_proxy)]

  #First Rebalancing Month
  ##########################
  current_date <- dates_m_vector[data_availability_cutoff]
  selected_backtest_returns_corrected_positions_upd_ref <-
  selected_backtest_returns_corrected_positions_df[which(selected_backtest_returns_corrected_positions_df$dates <= current_date),]
  selected_market_factor_proxy_vector_upd_ref <- selected_market_factor_proxy_df[which(selected_market_factor_proxy_df$dates <= current_date), market_factor_proxy]
  signal_themes_m_d_ref <- signal_themes_m_df[which(signal_themes_m_df$dates == current_date),]

  mean_active_return <- apply(selected_backtest_returns_corrected_positions_upd_ref[,-1], 2, function(x) mean(x, na.rm = TRUE))
  tracking_error <- apply(selected_backtest_returns_corrected_positions_upd_ref[,-1], 2, function(x) sd(x, na.rm = TRUE))
  IR = mean_active_return / tracking_error
  lm_model_list <- list()
  for(i in 2:ncol(selected_backtest_returns_corrected_positions_upd_ref)){
    lm_model_list[[i - 1]] <- summary(
      lm(selected_backtest_returns_corrected_positions_upd_ref[,i] ~ selected_market_factor_proxy_vector_upd_ref)
    )
  }
  alpha <- sapply(lm_model_list, function(x) x$coefficients[1,1])
  beta <- sapply(lm_model_list, function(x) x$coefficients[2,1])
  alpha_t_stat <- sapply(lm_model_list, function(x) x$coefficients[1,3])
  treynor <- mean_active_return / beta
  p_value <- sapply(lm_model_list, function(x) x$coefficients[1,4])

  #Create signal universe
  signal_universe_m_d_ref_1 <- data.frame(
    id = paste0(colnames(selected_backtest_returns_corrected_positions_df)[-c(1)],"-",current_date),
    tickers = colnames(selected_backtest_returns_corrected_positions_df)[-c(1)],
    dates = current_date,
    mean_active_return = mean_active_return,
    tracking_error = tracking_error,
    IR = IR,
    alpha = alpha,
    alpha_t_stat = alpha_t_stat,
    beta = beta,
    treynor = treynor,
    p_value = p_value,
    row.names = NULL
  )
  signal_universe_m_d_ref_1$adjusted_p_value <- p.adjust(signal_universe_m_d_ref_1$p_value,
                                                         method = signal_selection_policy$p_correction_method)

  signal_universe_m_d_ref_1[, "final_signal"] <- signal_transform(
    signal_universe_m_d_ref_1$alpha_t_stat,
    upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization
  )

  #Create benchmarks
  top_assets <- rep(0, length(signal_universe_m_d_ref_1$adjusted_p_value))
  top_assets[which(signal_universe_m_d_ref_1$adjusted_p_value <= signal_selection_policy$signal_significance_threshold)] <- 1
  signal_universe_m_d_ref_1$top_assets <- top_assets
  se_benchmarks <- create_se_benchmarks(signal_universe_m_d_ref_1, signal_themes_m_d_ref = signal_themes_m_d_ref)


  benchmark_weights_m_d_ref <- create_se_benchmarks(
    signal_universe_m_d_ref_1, signal_themes_m_d_ref = signal_themes_m_d_ref
  )

  expect_equal(benchmark_weights_m_d_ref$theme_ss, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  expect_equal(benchmark_weights_m_d_ref$theme_sb, c(0.5, 0, 0.25, 0, 0.25))
  signal_universe_m_d_ref_1$theme_ss_bench_weights <- benchmark_weights_m_d_ref$theme_ss
  signal_universe_m_d_ref_1$theme_sb_bench_weights <- benchmark_weights_m_d_ref$theme_sb
  signal_universe_m_d_ref_1$theme <- c("value", "value", "defensive", "momentum", "defensive")
  signal_universe_m_d_ref_1$is_eligible <- c(1,0,1,1,1)

  expect_equal(signal_universe_m_d_ref_1,
               classify_investment_universe(signals_m_d_ref = signal_universe_m_d_ref_1[,c(1:13)],
                                            signal_significance_threshold = signal_selection_policy$signal_significance_threshold,
                                            groups_m_d_ref = signal_themes_m_d_ref,
                                            concentration_constraint_policy = list(
                                              benchmark = c("theme_ss", "theme_sb"),
                                              max_abs_active_group_weight = 0.1
                                            ),
                                            asset_object = "signals"
                                            )
               )

  signal_universe_m_d_ref_1$final_signal <- NULL
  signal_eligibility_results_list[[1]] <- signal_universe_m_d_ref_1

  eligible_signals_1 <- signal_eligibility_results_list[[1]]$tickers[
    which(signal_eligibility_results_list[[1]]$is_eligible == 1)
  ]
  eligible_signals_list[[1]] <- eligible_signals_1
  eligible_signals_list[[2]] <- eligible_signals_1
  eligible_signals_list[[3]] <- eligible_signals_1


  #Second Rebalancing Month
  ##########################
  current_date <- dates_m_vector[9]
  selected_backtest_returns_corrected_positions_upd_ref <-
    selected_backtest_returns_corrected_positions_df[which(selected_backtest_returns_corrected_positions_df$dates <= current_date),]
  selected_market_factor_proxy_vector_upd_ref <- selected_market_factor_proxy_df[which(selected_market_factor_proxy_df$dates <= current_date), market_factor_proxy]
  signal_themes_m_d_ref <- signal_themes_m_df[which(signal_themes_m_df$dates == current_date),]

  mean_active_return <- apply(selected_backtest_returns_corrected_positions_upd_ref[,-1], 2, function(x) mean(x, na.rm = TRUE))
  tracking_error <- apply(selected_backtest_returns_corrected_positions_upd_ref[,-1], 2, function(x) sd(x, na.rm = TRUE))
  IR = mean_active_return / tracking_error
  lm_model_list <- list()
  for(i in 2:ncol(selected_backtest_returns_corrected_positions_upd_ref)){
    lm_model_list[[i - 1]] <- summary(
      lm(selected_backtest_returns_corrected_positions_upd_ref[,i] ~ selected_market_factor_proxy_vector_upd_ref)
    )
  }
  alpha <- sapply(lm_model_list, function(x) x$coefficients[1,1])
  beta <- sapply(lm_model_list, function(x) x$coefficients[2,1])
  alpha_t_stat <- sapply(lm_model_list, function(x) x$coefficients[1,3])
  treynor <- mean_active_return / beta
  p_value <- sapply(lm_model_list, function(x) x$coefficients[1,4])

  #Create signal universe
  signal_universe_m_d_ref_2 <- data.frame(
    id = paste0(colnames(selected_backtest_returns_corrected_positions_df)[-c(1)],"-",current_date),
    tickers = colnames(selected_backtest_returns_corrected_positions_df)[-c(1)],
    dates = current_date,
    mean_active_return = mean_active_return,
    tracking_error = tracking_error,
    IR = IR,
    alpha = alpha,
    alpha_t_stat = alpha_t_stat,
    beta = beta,
    treynor = treynor,
    p_value = p_value,
    row.names = NULL
  )
  signal_universe_m_d_ref_2$adjusted_p_value <- p.adjust(signal_universe_m_d_ref_2$p_value,
                                                         method = signal_selection_policy$p_correction_method)

  signal_universe_m_d_ref_2[, "final_signal"] <- signal_transform(
    signal_universe_m_d_ref_2$alpha_t_stat,
    upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization
  )

  #Create benchmarks
  top_assets <- rep(1, length(signal_universe_m_d_ref_2$adjusted_p_value))
  signal_universe_m_d_ref_2$top_assets <- top_assets
  se_benchmarks <- create_se_benchmarks(signal_universe_m_d_ref_2, signal_themes_m_d_ref = signal_themes_m_d_ref)


  benchmark_weights_m_d_ref <- create_se_benchmarks(
    signal_universe_m_d_ref_2, signal_themes_m_d_ref = signal_themes_m_d_ref
  )

  expect_equal(benchmark_weights_m_d_ref$theme_ss, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  expect_equal(benchmark_weights_m_d_ref$theme_sb, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  signal_universe_m_d_ref_2$theme_ss_bench_weights <- benchmark_weights_m_d_ref$theme_ss
  signal_universe_m_d_ref_2$theme_sb_bench_weights <- benchmark_weights_m_d_ref$theme_sb
  signal_universe_m_d_ref_2$theme <- c("value", "value", "defensive", "momentum", "defensive")
  signal_universe_m_d_ref_2$is_eligible <- c(1,1,1,1,1)

  expect_equal(signal_universe_m_d_ref_2,
               classify_investment_universe(signals_m_d_ref = signal_universe_m_d_ref_2[,c(1:13)],
                                            signal_significance_threshold = signal_selection_policy$signal_significance_threshold,
                                            groups_m_d_ref = signal_themes_m_d_ref,
                                            concentration_constraint_policy = list(
                                              benchmark = c("theme_ss", "theme_sb"),
                                              max_abs_active_group_weight = 0.1
                                            ),
                                            asset_object = "signals"
               )
  )

  signal_universe_m_d_ref_2$final_signal <- NULL
  signal_eligibility_results_list[[2]] <- signal_universe_m_d_ref_2

  eligible_signals_2 <- signal_eligibility_results_list[[2]]$tickers[
    which(signal_eligibility_results_list[[2]]$is_eligible == 1)
  ]
  eligible_signals_list[[4]] <- eligible_signals_2
  eligible_signals_list[[5]] <- eligible_signals_2
  eligible_signals_list[[6]] <- eligible_signals_2
  eligible_signals_list[[7]] <- eligible_signals_2


  names(signal_eligibility_results_list) <- c(dates_m_vector[data_availability_cutoff],
                                              current_date)

  names(eligible_signals_list) <- dates_m_vector[data_availability_cutoff:length(dates_m_vector)]

})
