test_that("run_ss_backtest works for vanilla no-pooled frequentist setting", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))

  set.seed(123)
  mocked_backtest_returns_xts <- xts::as.xts(data.frame(
    book_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.035),
    dy_med_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.025),
    eps_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.005, sd = 0.03),
    mom_res_12m = rnorm(length(unique(signals_m_df$dates)), mean = 0.015, sd = 0.035),
    roe_3m = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.02),
    sharpe_6m = rnorm(length(unique(signals_m_df$dates)), mean = 0.025, sd = 0.035),
    low_vol_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.0075)
  ), order.by = unique(signals_m_df$dates))

  #Change EPS Yield to be in second rebalancing
  mocked_backtest_returns_xts$eps_yield[c(7:9)] <- c(3,3,3)
  chosen_signals_and_positions <- c(book_yield = "long", eps_yield = "long", roe_3m = "long", sharpe_6m = "long", vol_36m = "short")

  frequentist_ss_config <- create_ss_backtest_config(initial_sample_size = 6, rebalancing_months = 6,
                                                     split_method = "expanding", config_name = "frequentist_ss", active_returns = TRUE,
                                                     chosen_signals_and_positions = chosen_signals_and_positions
                                                     ) %>%
                            add_alpha_test_strategy(model_structure = "no_pooled",
                                                    signal_significance_threshold = 0.15, p_correction_method = "none",
                                                    market_factor_proxy = "IBOV", enable_theme_representativeness = TRUE)

  signals_m_df <- create_meta_dataframe(signals_m_df, "signals_123")
  signal_themes_m_df <- create_meta_dataframe(signal_themes_m_df, "st_11", type =  "groups")



  results <- suppressWarnings( #This is for NA warning of NAs at the end of run_ss_backtest
    run_ss_backtest(frequentist_ss_config,
                             signals_m_df = signals_m_df, backtest_returns_xts = mocked_backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts,
                             signal_themes_m_df = signal_themes_m_df,
                             verbose = TRUE
                             )
  )

  ####Expected Results
  ####################
  ####################
  signals_m_df <- signals_m_df@data
  signal_themes_m_df <- signal_themes_m_df@data
  initial_sample_size <- frequentist_ss_config@initial_sample_size
  rebalancing_months <- frequentist_ss_config@rebalancing_months
  initial_sample_size <- frequentist_ss_config@initial_sample_size
  market_factor_proxy <- frequentist_ss_config@alpha_test_strategy@market_factor_proxy
  model_structure <- frequentist_ss_config@alpha_test_strategy@model_structure
  active_returns <- frequentist_ss_config@active_returns

  signal_universe_m_d_ref_list <- list()
  bayesian_fit_nested_list <- list()
  dates_m_vector <- unique(signals_m_df$dates)

  check_inputs_ss_backtest(rebalancing_months = rebalancing_months,
                           signals_m_df = signals_m_df, initial_sample_size = initial_sample_size, active_returns = active_returns,
                           custom_signal_universe_metrics_m_df = NULL,
                           chosen_signals_and_positions = chosen_signals_and_positions,  model_structure = model_structure,
                           backtest_returns_xts = mocked_backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts,
                           p_correction_method = signal_selection_policy$p_correction_method, forced_signals = NULL,
                           enable_theme_representativeness = TRUE, market_factor_proxy = market_factor_proxy,
                           signal_significance_threshold = signal_selection_policy$signal_significance_threshold,
                           priors_m_df = NULL, signal_themes_m_df = signal_themes_m_df
                           )

  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    signal_themes_m_df = signal_themes_m_df,
    chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_xts = mocked_backtest_returns_xts
  )

  expect_equal(selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts,
               mocked_backtest_returns_xts[, c("book_yield", "eps_yield", "roe_3m", "sharpe_6m", "low_vol_36m")])

  expect_equal(selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df[,c("id", "tickers", "dates", "book_yield", "eps_yield", "roe_3m", "sharpe_6m")],
               signals_m_df[,c("id", "tickers", "dates", "book_yield", "eps_yield", "roe_3m", "sharpe_6m")])

  expect_equal(selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df$low_vol_36m,
               signals_m_df$vol_36m * -1)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, c(market_factor_proxy)]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  #First Rebalancing Month
  ##########################
  current_date <- dates_m_vector[initial_sample_size]
  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:6),]
  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:6), market_factor_proxy]
  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]


  summarize_performance_results <- summarize_performance(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_structure = model_structure,
    active_returns = active_returns
  )

  signal_universe_m_d_ref_1 <- summarize_performance_results$signal_universe_m_d_ref

  signal_universe_m_d_ref_1$adjusted_p_value <- p.adjust(signal_universe_m_d_ref_1$p_value,
                                                         method = frequentist_ss_config@alpha_test_strategy@p_correction_method)

  signal_universe_m_d_ref_1[, "exp_ret_score"] <- signal_transform(
    signal_universe_m_d_ref_1$alpha_t_stat,
    upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization
  )

  #Create benchmarks
  top_assets <- rep(0, length(signal_universe_m_d_ref_1$adjusted_p_value))
  top_assets[which(signal_universe_m_d_ref_1$adjusted_p_value <= frequentist_ss_config@alpha_test_strategy@signal_significance_threshold)] <- 1
  signal_universe_m_d_ref_1$top_assets <- top_assets
  se_benchmarks <- create_se_benchmarks(signal_universe_m_d_ref_1, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref)


  benchmark_weights_m_d_ref <- create_se_benchmarks(
    signal_universe_m_d_ref_1, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref
  )

  expect_equal(benchmark_weights_m_d_ref$theme_ss, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  expect_equal(benchmark_weights_m_d_ref$theme_sb, c(0.5, 0, 0.25, 0, 0.25))
  signal_universe_m_d_ref_1$theme_ss_bench_weights <- benchmark_weights_m_d_ref$theme_ss
  signal_universe_m_d_ref_1$theme_sb_bench_weights <- benchmark_weights_m_d_ref$theme_sb
  signal_universe_m_d_ref_1$theme <- c("value", "value", "defensive", "momentum", "defensive")
  signal_universe_m_d_ref_1$is_eligible <- c(1,0,1,1,1)

  expect_equal(signal_universe_m_d_ref_1,
               classify_investment_universe(signals_m_d_ref = signal_universe_m_d_ref_1[,c(1:48)],
                                            signal_significance_threshold = frequentist_ss_config@alpha_test_strategy@signal_significance_threshold,
                                            groups_m_d_ref = selected_signal_themes_m_d_ref,
                                            concentration_constraint_policy = list(
                                              benchmark = c("theme_ss", "theme_sb"),
                                              max_abs_active_group_weight = 0.1
                                            ),
                                            asset_object = "signals"
                                            )
               )

  signal_universe_m_d_ref_1$exp_ret_score <- NULL
  signal_eligibility_results_list <- list()
  signal_eligibility_results_list[[1]] <- signal_universe_m_d_ref_1

  eligible_signals_1 <- signal_eligibility_results_list[[1]]$tickers[
    which(signal_eligibility_results_list[[1]]$is_eligible == 1)
  ]


  #Second Rebalancing Month
  ##########################
  current_date <- dates_m_vector[9]

  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:9),]
  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:9), market_factor_proxy]
  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]

  summarize_performance_results <- summarize_performance(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_structure = model_structure,
    active_returns = active_returns
  )

  signal_universe_m_d_ref_2 <- summarize_performance_results$signal_universe_m_d_ref

  signal_universe_m_d_ref_2$adjusted_p_value <- p.adjust(signal_universe_m_d_ref_2$p_value,
                                                         method = frequentist_ss_config@alpha_test_strategy@p_correction_method)

  signal_universe_m_d_ref_2[, "exp_ret_score"] <- signal_transform(
    signal_universe_m_d_ref_2$alpha_t_stat,
    upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization
  )

  #Create benchmarks
  top_assets <- rep(1, length(signal_universe_m_d_ref_2$adjusted_p_value))
  signal_universe_m_d_ref_2$top_assets <- top_assets
  se_benchmarks <- create_se_benchmarks(signal_universe_m_d_ref_2, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref)


  benchmark_weights_m_d_ref <- create_se_benchmarks(
    signal_universe_m_d_ref_2, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref
  )

  expect_equal(benchmark_weights_m_d_ref$theme_ss, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  expect_equal(benchmark_weights_m_d_ref$theme_sb, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  signal_universe_m_d_ref_2$theme_ss_bench_weights <- benchmark_weights_m_d_ref$theme_ss
  signal_universe_m_d_ref_2$theme_sb_bench_weights <- benchmark_weights_m_d_ref$theme_sb
  signal_universe_m_d_ref_2$theme <- c("value", "value", "defensive", "momentum", "defensive")
  signal_universe_m_d_ref_2$is_eligible <- c(1,1,1,1,1)

  expect_equal(signal_universe_m_d_ref_2,
               classify_investment_universe(signals_m_d_ref = signal_universe_m_d_ref_2[,c(1:48)],
                                            signal_significance_threshold = frequentist_ss_config@alpha_test_strategy@signal_significance_threshold,
                                            groups_m_d_ref = selected_signal_themes_m_d_ref,
                                            concentration_constraint_policy = list(
                                              benchmark = c("theme_ss", "theme_sb"),
                                              max_abs_active_group_weight = 0.1
                                            ),
                                            asset_object = "signals"
               )
  )

  signal_universe_m_d_ref_2$exp_ret_score <- NULL
  signal_eligibility_results_list[[2]] <- signal_universe_m_d_ref_2

  eligible_signals_2 <- signal_eligibility_results_list[[2]]$tickers[
    which(signal_eligibility_results_list[[2]]$is_eligible == 1)
  ]


  expected_signal_universe <- rbind(signal_eligibility_results_list[[1]], signal_eligibility_results_list[[2]])
  expected_signal_universe <- expected_signal_universe[order(expected_signal_universe$id),]
  rownames(expected_signal_universe) <- NULL
  rownames(results@signal_universe_m_df@data) <- NULL
  expect_equal(results@signal_universe_m_df@data,expected_signal_universe)

  expect_equal(results@signal_universe_m_df@data %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers),
               c(eligible_signals_1, eligible_signals_2)[order(c(eligible_signals_1, eligible_signals_2))])

  expected_signal_universe_m_d_ref <- results@signal_universe_m_df@data %>% dplyr::filter(dates == "2023-06-15")
  rownames(expected_signal_universe_m_d_ref) <- NULL
  rownames(results@final_signal_universe_m_d_ref@data) <- NULL
  expect_equal(results@final_signal_universe_m_d_ref@data, expected_signal_universe_m_d_ref)


})

test_that("run_ss_backtest works for inclusion of forced variables", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))

  set.seed(123)
  mocked_backtest_returns_xts <- xts::as.xts(data.frame(
    book_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.035),
    dy_med_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.025),
    eps_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.005, sd = 0.03),
    mom_res_12m = rnorm(length(unique(signals_m_df$dates)), mean = 0.015, sd = 0.035),
    roe_3m = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.02),
    sharpe_6m = rnorm(length(unique(signals_m_df$dates)), mean = 0.025, sd = 0.035),
    low_vol_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.0075)
  ), order.by = unique(signals_m_df$dates))

  #Change EPS Yield to be in second rebalancing
  mocked_backtest_returns_xts$eps_yield[c(7:9)] <- c(3,3,3)
  chosen_signals_and_positions <- c(book_yield = "long", eps_yield = "long", roe_3m = "long", sharpe_6m = "long", vol_36m = "short", setorc1 = "force")

  frequentist_ss_config <- create_ss_backtest_config(initial_sample_size = 6, rebalancing_months = 6,
                                                     split_method = "expanding", config_name = "frequentist_ss", active_returns = TRUE,
                                                     chosen_signals_and_positions = chosen_signals_and_positions
  ) %>%
    add_alpha_test_strategy(model_structure = "no_pooled",
                            signal_significance_threshold = 0.15, p_correction_method = "none",
                            market_factor_proxy = "IBOV", enable_theme_representativeness = TRUE)

  signals_m_df <- create_meta_dataframe(signals_m_df, "signals_123")
  signal_themes_m_df <- create_meta_dataframe(signal_themes_m_df, "st_11", type =  "groups")



  results <- suppressWarnings( #This is for NA warning of NAs at the end of run_ss_backtest
    run_ss_backtest(frequentist_ss_config,
                    signals_m_df = signals_m_df, backtest_returns_xts = mocked_backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts,
                    signal_themes_m_df = signal_themes_m_df,
                    verbose = TRUE
    )
  )

  expect_true("setorc1" %in% results@signal_universe_m_df@data$tickers)
  expect_equal(length(which(results@signal_universe_m_df@data$tickers == "setorc1")), 2)
  expect_equal(results@signal_universe_m_df@data %>% dplyr::filter(tickers == "setorc1") %>% dplyr::pull(is_eligible), c(1,1))
  expect_equal(results@signal_universe_m_df@data %>% dplyr::filter(tickers == "setorc1") %>% dplyr::pull(theme_ss_bench_weights), c(0,0))
  expect_equal(results@signal_universe_m_df@data %>% dplyr::filter(tickers == "setorc1") %>% dplyr::pull(theme_sb_bench_weights), c(0,0))
  expect_equal(results@signal_universe_m_df@data %>% dplyr::filter(tickers == "setorc1") %>% dplyr::pull(theme), c("forced","forced"))
  expect_true(all(sapply(results@signal_universe_m_df@data %>% dplyr::filter(tickers == "setorc1") %>%
                  dplyr::select(-id, -tickers, -dates, -theme, -theme_ss_bench_weights, -theme_sb_bench_weights, -is_eligible),
                  function(x) is.na(x))))

})

test_that("run_ss_backtest works for vanilla no-pooled frequentist setting when p_correction is holm", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))

  set.seed(123)
  mocked_backtest_returns_xts <- xts::as.xts(data.frame(
    book_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.035),
    dy_med_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.025),
    eps_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.005, sd = 0.03),
    mom_res_12m = rnorm(length(unique(signals_m_df$dates)), mean = 0.015, sd = 0.035),
    roe_3m = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.02),
    sharpe_6m = rnorm(length(unique(signals_m_df$dates)), mean = 0.025, sd = 0.035),
    low_vol_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.0075)
  ), order.by = unique(signals_m_df$dates))

  chosen_signals_and_positions <- c(book_yield = "long", eps_yield = "long", roe_3m = "long", sharpe_6m = "long", vol_36m = "short")

  frequentist_ss_config <- create_ss_backtest_config(initial_sample_size = 6, rebalancing_months = 6,
                                                     split_method = "expanding", config_name = "frequentist_ss", chosen_signals_and_positions = chosen_signals_and_positions) %>%
    add_alpha_test_strategy(signal_significance_threshold = 0.15, p_correction_method = "holm", model_structure = "no_pooled",
                            market_factor_proxy = "IBOV", enable_theme_representativeness = TRUE)


  signals_m_df <- create_meta_dataframe(signals_m_df, "signals_123")
  signal_themes_m_df <- create_meta_dataframe(signal_themes_m_df, "st_11")



  results <- run_ss_backtest(frequentist_ss_config,
                             signals_m_df = signals_m_df, backtest_returns_xts = mocked_backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts,signal_themes_m_df = signal_themes_m_df,
                             verbose = TRUE
  )


  ####Expected Results
  ####################
  ####################
  signals_m_df <- signals_m_df@data
  signal_themes_m_df <- signal_themes_m_df@data
  initial_sample_size <- frequentist_ss_config@initial_sample_size
  rebalancing_months <- frequentist_ss_config@rebalancing_months
  initial_sample_size <- frequentist_ss_config@initial_sample_size
  market_factor_proxy <- frequentist_ss_config@alpha_test_strategy@market_factor_proxy
  model_structure <- frequentist_ss_config@alpha_test_strategy@model_structure
  active_returns <- frequentist_ss_config@active_returns



  signal_universe_m_d_ref_list <- list()
  bayesian_fit_nested_list <- list()
  eligible_signals_list <- list()
  dates_m_vector <- unique(signals_m_df$dates)

  check_inputs_ss_backtest(rebalancing_months = rebalancing_months,
                           signals_m_df = signals_m_df, initial_sample_size = initial_sample_size, active_returns = active_returns,
                           custom_signal_universe_metrics_m_df = NULL,
                           chosen_signals_and_positions = chosen_signals_and_positions, model_structure = model_structure,
                           backtest_returns_xts = mocked_backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts,
                           p_correction_method = signal_selection_policy$p_correction_method, forced_signals = NULL,
                           enable_theme_representativeness = TRUE, market_factor_proxy = market_factor_proxy,
                           signal_significance_threshold = signal_selection_policy$signal_significance_threshold,
                           priors_m_df = NULL, signal_themes_m_df = signal_themes_m_df
  )

  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    signal_themes_m_df = signal_themes_m_df,
    chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_xts = mocked_backtest_returns_xts
  )


  expect_equal(selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts,
               mocked_backtest_returns_xts[, c("book_yield", "eps_yield", "roe_3m", "sharpe_6m", "low_vol_36m")])

  expect_equal(selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df[,c("id", "tickers", "dates", "book_yield", "eps_yield", "roe_3m", "sharpe_6m")],
               signals_m_df[,c("id", "tickers", "dates", "book_yield", "eps_yield", "roe_3m", "sharpe_6m")])

  expect_equal(selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df$low_vol_36m,
               signals_m_df$vol_36m * -1)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, c(market_factor_proxy)]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  #First Rebalancing Month
  ##########################
  current_date <- dates_m_vector[initial_sample_size]
  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:6),]
  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:6), market_factor_proxy]
  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]


  summarize_performance_results <- summarize_performance(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_structure = model_structure,
    active_returns = active_returns
  )

  signal_universe_m_d_ref_1 <- summarize_performance_results$signal_universe_m_d_ref

  signal_universe_m_d_ref_1$adjusted_p_value <- p.adjust(signal_universe_m_d_ref_1$p_value,
                                                         method = frequentist_ss_config@alpha_test_strategy@p_correction_method)

  signal_universe_m_d_ref_1[, "exp_ret_score"] <- signal_transform(
    signal_universe_m_d_ref_1$alpha_t_stat,
    upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization
  )

  #Create benchmarks
  top_assets <- rep(0, length(signal_universe_m_d_ref_1$adjusted_p_value))
  top_assets[which(signal_universe_m_d_ref_1$adjusted_p_value <= frequentist_ss_config@alpha_test_strategy@signal_significance_threshold)] <- 1
  signal_universe_m_d_ref_1$top_assets <- top_assets
  se_benchmarks <- create_se_benchmarks(signal_universe_m_d_ref_1, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref)


  benchmark_weights_m_d_ref <- create_se_benchmarks(
    signal_universe_m_d_ref_1, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref
  )

  expect_equal(benchmark_weights_m_d_ref$theme_ss, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  expect_equal(benchmark_weights_m_d_ref$theme_sb, c(0.5, 0, 0.25, 0, 0.25))
  signal_universe_m_d_ref_1$theme_ss_bench_weights <- benchmark_weights_m_d_ref$theme_ss
  signal_universe_m_d_ref_1$theme_sb_bench_weights <- benchmark_weights_m_d_ref$theme_sb
  signal_universe_m_d_ref_1$theme <- c("value", "value", "defensive", "momentum", "defensive")
  signal_universe_m_d_ref_1$is_eligible <- c(1,0,1,1,1)



  expect_equal(signal_universe_m_d_ref_1,
               classify_investment_universe(signals_m_d_ref = signal_universe_m_d_ref_1[,c(1:48)],
                                            signal_significance_threshold = frequentist_ss_config@alpha_test_strategy@signal_significance_threshold,
                                            groups_m_d_ref = selected_signal_themes_m_d_ref,
                                            concentration_constraint_policy = list(
                                              benchmark = c("theme_ss", "theme_sb"),
                                              max_abs_active_group_weight = 0.1
                                            ),
                                            asset_object = "signals"
               )
  )

  signal_universe_m_d_ref_1$exp_ret_score <- NULL
  signal_eligibility_results_list <- list()
  signal_eligibility_results_list[[1]] <- signal_universe_m_d_ref_1

  eligible_signals_1 <- signal_eligibility_results_list[[1]]$tickers[
    which(signal_eligibility_results_list[[1]]$is_eligible == 1)
  ]
  eligible_signals_list[[1]] <- data.frame(tickers = eligible_signals_1)
  eligible_signals_list[[2]] <- data.frame(tickers = eligible_signals_1)
  eligible_signals_list[[3]] <- data.frame(tickers = eligible_signals_1)


  #Second Rebalancing Month
  ##########################
  current_date <- dates_m_vector[9]
  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:9),]
  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:9), market_factor_proxy]
  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]

  summarize_performance_results <- summarize_performance(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_structure = model_structure,
    active_returns = active_returns
  )

  signal_universe_m_d_ref_2 <- summarize_performance_results$signal_universe_m_d_ref

  signal_universe_m_d_ref_2$adjusted_p_value <- p.adjust(signal_universe_m_d_ref_2$p_value,
                                                         method = frequentist_ss_config@alpha_test_strategy@p_correction_method)

  signal_universe_m_d_ref_2[, "exp_ret_score"] <- signal_transform(
    signal_universe_m_d_ref_2$alpha_t_stat,
    upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization
  )

  #Create benchmarks
  top_assets <- rep(0, length(signal_universe_m_d_ref_2$adjusted_p_value))
  top_assets[which(signal_universe_m_d_ref_2$adjusted_p_value <= frequentist_ss_config@alpha_test_strategy@signal_significance_threshold)] <- 1

  signal_universe_m_d_ref_2$top_assets <- top_assets
  se_benchmarks <- create_se_benchmarks(signal_universe_m_d_ref_2, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref)


  benchmark_weights_m_d_ref <- create_se_benchmarks(
    signal_universe_m_d_ref_2, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref
  )

  expect_equal(benchmark_weights_m_d_ref$theme_ss, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  expect_equal(benchmark_weights_m_d_ref$theme_sb, c(1/3, 0, 1/3/2, 1/3, 1/3/2))
  signal_universe_m_d_ref_2$theme_ss_bench_weights <- benchmark_weights_m_d_ref$theme_ss
  signal_universe_m_d_ref_2$theme_sb_bench_weights <- benchmark_weights_m_d_ref$theme_sb
  signal_universe_m_d_ref_2$theme <- c("value", "value", "defensive", "momentum", "defensive")
  signal_universe_m_d_ref_2$is_eligible <- c(1,0,1,1,1)

  expect_equal(signal_universe_m_d_ref_2,
               classify_investment_universe(signals_m_d_ref = signal_universe_m_d_ref_2[,c(1:48)],
                                            signal_significance_threshold = frequentist_ss_config@alpha_test_strategy@signal_significance_threshold,
                                            groups_m_d_ref = selected_signal_themes_m_d_ref,
                                            concentration_constraint_policy = list(
                                              benchmark = c("theme_ss", "theme_sb"),
                                              max_abs_active_group_weight = 0.1
                                            ),
                                            asset_object = "signals"
               )
  )

  signal_universe_m_d_ref_2$exp_ret_score <- NULL
  signal_eligibility_results_list[[2]] <- signal_universe_m_d_ref_2

  eligible_signals_2 <- signal_eligibility_results_list[[2]]$tickers[
    which(signal_eligibility_results_list[[2]]$is_eligible == 1)
  ]
  eligible_signals_list[[4]] <- data.frame(tickers = eligible_signals_2)
  eligible_signals_list[[5]] <- data.frame(tickers = eligible_signals_2)
  eligible_signals_list[[6]] <- data.frame(tickers = eligible_signals_2)
  eligible_signals_list[[7]] <- data.frame(tickers = eligible_signals_2)

  names(eligible_signals_list) <- dates_m_vector[initial_sample_size:length(dates_m_vector)]

  expected_signal_universe <- rbind(signal_eligibility_results_list[[1]], signal_eligibility_results_list[[2]])
  expected_signal_universe <- expected_signal_universe[order(expected_signal_universe$id),]

  rownames(expected_signal_universe) <- NULL
  rownames(results@signal_universe_m_df@data) <- NULL

  expect_equal(results@signal_universe_m_df@data, expected_signal_universe)

  rownames(signal_universe_m_d_ref_2[order(signal_universe_m_d_ref_2$id),]) <- NULL
  rownames(results@final_signal_universe_m_d_ref@data) <- NULL

  expect_true(
  all.equal(results@final_signal_universe_m_d_ref@data, signal_universe_m_d_ref_2[order(signal_universe_m_d_ref_2$id),], check.attributes = FALSE) #Ignore rownames
  )

})

test_that("run_ss_backtest works for vanilla pooled frequentist setting when p_correction is FDR", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))

  set.seed(123)
  mocked_backtest_returns_xts <- xts::as.xts(data.frame(
    book_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.035),
    dy_med_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.025),
    eps_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.005, sd = 0.03),
    mom_res_12m = rnorm(length(unique(signals_m_df$dates)), mean = 0.015, sd = 0.035),
    roe_3m = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.02),
    sharpe_6m = rnorm(length(unique(signals_m_df$dates)), mean = 0.025, sd = 0.035),
    low_vol_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.0075)
  ), order.by = unique(signals_m_df$dates))

  chosen_signals_and_positions <- c(book_yield = "long", eps_yield = "long", roe_3m = "long", sharpe_6m = "long", vol_36m = "short")

  frequentist_ss_config <- create_ss_backtest_config(initial_sample_size = 6, rebalancing_months = 6,
                                                     split_method = "expanding", config_name = "frequentist_ss", chosen_signals_and_positions = chosen_signals_and_positions) %>%
    add_alpha_test_strategy(signal_significance_threshold = 0.85, p_correction_method = "fdr", model_structure = "partial_pooled",
                            market_factor_proxy = "IBOV", enable_theme_representativeness = TRUE,
                            theme_level_intercept = "theme_specific", theme_level_slope = "fixed")


  signals_m_df <- create_meta_dataframe(signals_m_df, "signals_123")
  signal_themes_m_df <- create_meta_dataframe(signal_themes_m_df, "st_11")


  results <- run_ss_backtest(frequentist_ss_config,
                             signals_m_df = signals_m_df, backtest_returns_xts = mocked_backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts,signal_themes_m_df = signal_themes_m_df,
                             verbose = TRUE
  )


  ####Expected Results
  ####################
  ####################
  signals_m_df <- signals_m_df@data
  signal_themes_m_df <- signal_themes_m_df@data
  initial_sample_size <- frequentist_ss_config@initial_sample_size
  rebalancing_months <- frequentist_ss_config@rebalancing_months
  initial_sample_size <- frequentist_ss_config@initial_sample_size
  market_factor_proxy <- frequentist_ss_config@alpha_test_strategy@market_factor_proxy
  model_structure <- frequentist_ss_config@alpha_test_strategy@model_structure
  active_returns <- frequentist_ss_config@active_returns
  lmer_control <- frequentist_ss_config@alpha_test_strategy@lmer_control

  signal_universe_m_d_ref_list <- list()
  bayesian_fit_nested_list <- list()
  eligible_signals_list <- list()
  dates_m_vector <- unique(signals_m_df$dates)

  check_inputs_ss_backtest(rebalancing_months = rebalancing_months,
                           signals_m_df = signals_m_df, initial_sample_size = initial_sample_size, active_returns = active_returns,
                           custom_signal_universe_metrics_m_df = NULL,
                           chosen_signals_and_positions = chosen_signals_and_positions, model_structure = model_structure,
                           backtest_returns_xts = mocked_backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts,
                           p_correction_method = signal_selection_policy$p_correction_method, lmer_control = lmer_control,
                           enable_theme_representativeness = TRUE, market_factor_proxy = market_factor_proxy, forced_signals = NULL,
                           signal_significance_threshold = signal_selection_policy$signal_significance_threshold,
                           priors_m_df = NULL, signal_themes_m_df = signal_themes_m_df
  )

  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    signal_themes_m_df = signal_themes_m_df,
    chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_xts = mocked_backtest_returns_xts
  )


  expect_equal(selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts,
               mocked_backtest_returns_xts[, c("book_yield", "eps_yield", "roe_3m", "sharpe_6m", "low_vol_36m")])

  expect_equal(selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df[,c("id", "tickers", "dates", "book_yield", "eps_yield", "roe_3m", "sharpe_6m")],
               signals_m_df[,c("id", "tickers", "dates", "book_yield", "eps_yield", "roe_3m", "sharpe_6m")])

  expect_equal(selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df$low_vol_36m,
               signals_m_df$vol_36m * -1)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, c(market_factor_proxy)]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  #First Rebalancing Month
  ##########################
  current_date <- dates_m_vector[initial_sample_size]
  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:6),]
  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:6), market_factor_proxy]
  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]

  model_spec_theme_level <- paste0(frequentist_ss_config@alpha_test_strategy@theme_level_intercept,"_intercept_",
                                   frequentist_ss_config@alpha_test_strategy@theme_level_slope,"_slope")

  summarize_performance_results <- summarize_performance(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_spec_theme_level = model_spec_theme_level,
    lmer_control = lmer_control,
    model_structure = model_structure,
    active_returns = active_returns
  )

  signal_universe_m_d_ref_1 <- summarize_performance_results$signal_universe_m_d_ref
  adj_p_value <- p.adjust(unique(signal_universe_m_d_ref_1$p_value), method = frequentist_ss_config@alpha_test_strategy@p_correction_method)

  signal_universe_m_d_ref_1$adjusted_p_value <- adj_p_value[c(1,1,2,3,2)]

  signal_universe_m_d_ref_1[, "exp_ret_score"] <- signal_transform(
    signal_universe_m_d_ref_1$alpha_t_stat,
    upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization
  )

  #Create benchmarks
  top_assets <- rep(0, length(signal_universe_m_d_ref_1$adjusted_p_value))
  top_assets[which(signal_universe_m_d_ref_1$adjusted_p_value <= frequentist_ss_config@alpha_test_strategy@signal_significance_threshold)] <- 1
  signal_universe_m_d_ref_1$top_assets <- top_assets
  se_benchmarks <- create_se_benchmarks(signal_universe_m_d_ref_1, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref)


  benchmark_weights_m_d_ref <- create_se_benchmarks(
    signal_universe_m_d_ref_1, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref
  )

  expect_equal(benchmark_weights_m_d_ref$theme_ss, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  expect_equal(benchmark_weights_m_d_ref$theme_sb, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  signal_universe_m_d_ref_1$theme_ss_bench_weights <- benchmark_weights_m_d_ref$theme_ss
  signal_universe_m_d_ref_1$theme_sb_bench_weights <- benchmark_weights_m_d_ref$theme_sb
  signal_universe_m_d_ref_1$theme <- c("value", "value", "defensive", "momentum", "defensive")
  signal_universe_m_d_ref_1$is_eligible <- c(1,1,1,1,1)


  expect_equal(signal_universe_m_d_ref_1,
               classify_investment_universe(signals_m_d_ref = signal_universe_m_d_ref_1[,c(1:50)],
                                            signal_significance_threshold = frequentist_ss_config@alpha_test_strategy@signal_significance_threshold,
                                            groups_m_d_ref = selected_signal_themes_m_d_ref,
                                            concentration_constraint_policy = list(
                                              benchmark = c("theme_ss", "theme_sb"),
                                              max_abs_active_group_weight = 0.1
                                            ),
                                            asset_object = "signals"
               )
  )

  signal_universe_m_d_ref_1$exp_ret_score <- NULL
  signal_eligibility_results_list <- list()
  signal_eligibility_results_list[[1]] <- signal_universe_m_d_ref_1

  eligible_signals_1 <- signal_eligibility_results_list[[1]]$tickers[
    which(signal_eligibility_results_list[[1]]$is_eligible == 1)
  ]
  eligible_signals_list[[1]] <- data.frame(tickers = eligible_signals_1)
  eligible_signals_list[[2]] <- data.frame(tickers = eligible_signals_1)
  eligible_signals_list[[3]] <- data.frame(tickers = eligible_signals_1)


  #Second Rebalancing Month
  ##########################
  current_date <- dates_m_vector[9]
  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:9),]
  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:9), market_factor_proxy]
  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]

  summarize_performance_results <- summarize_performance(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_structure = model_structure,
    model_spec_theme_level = model_spec_theme_level,
    lmer_control = lmer_control,
    active_returns = active_returns
  )

  signal_universe_m_d_ref_2 <- summarize_performance_results$signal_universe_m_d_ref
  adj_p_value <- p.adjust(unique(signal_universe_m_d_ref_2$p_value), method = frequentist_ss_config@alpha_test_strategy@p_correction_method)

  signal_universe_m_d_ref_2$adjusted_p_value <- adj_p_value[c(1,1,2,3,2)]

  signal_universe_m_d_ref_2[, "exp_ret_score"] <- signal_transform(
    signal_universe_m_d_ref_2$alpha_t_stat,
    upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization
  )

  #Create benchmarks
  top_assets <- rep(0, length(signal_universe_m_d_ref_2$adjusted_p_value))
  top_assets[which(signal_universe_m_d_ref_2$adjusted_p_value <= frequentist_ss_config@alpha_test_strategy@signal_significance_threshold)] <- 1

  signal_universe_m_d_ref_2$top_assets <- top_assets
  se_benchmarks <- create_se_benchmarks(signal_universe_m_d_ref_2, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref)


  benchmark_weights_m_d_ref <- create_se_benchmarks(
    signal_universe_m_d_ref_2, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref
  )

  expect_equal(benchmark_weights_m_d_ref$theme_ss, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  expect_equal(benchmark_weights_m_d_ref$theme_sb, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  signal_universe_m_d_ref_2$theme_ss_bench_weights <- benchmark_weights_m_d_ref$theme_ss
  signal_universe_m_d_ref_2$theme_sb_bench_weights <- benchmark_weights_m_d_ref$theme_sb
  signal_universe_m_d_ref_2$theme <- c("value", "value", "defensive", "momentum", "defensive")
  signal_universe_m_d_ref_2$is_eligible <- c(1,1,1,1,1)

  expect_equal(signal_universe_m_d_ref_2,
               classify_investment_universe(signals_m_d_ref = signal_universe_m_d_ref_2[,c(1:50)],
                                            signal_significance_threshold = frequentist_ss_config@alpha_test_strategy@signal_significance_threshold,
                                            groups_m_d_ref = selected_signal_themes_m_d_ref,
                                            concentration_constraint_policy = list(
                                              benchmark = c("theme_ss", "theme_sb"),
                                              max_abs_active_group_weight = 0.1
                                            ),
                                            asset_object = "signals"
               )
  )

  signal_universe_m_d_ref_2$exp_ret_score <- NULL
  signal_eligibility_results_list[[2]] <- signal_universe_m_d_ref_2

  eligible_signals_2 <- signal_eligibility_results_list[[2]]$tickers[
    which(signal_eligibility_results_list[[2]]$is_eligible == 1)
  ]
  eligible_signals_list[[4]] <- data.frame(tickers = eligible_signals_2)
  eligible_signals_list[[5]] <- data.frame(tickers = eligible_signals_2)
  eligible_signals_list[[6]] <- data.frame(tickers = eligible_signals_2)
  eligible_signals_list[[7]] <- data.frame(tickers = eligible_signals_2)


  names(eligible_signals_list) <- dates_m_vector[initial_sample_size:length(dates_m_vector)]

  expected_signal_universe <- rbind(signal_eligibility_results_list[[1]], signal_eligibility_results_list[[2]])
  expected_signal_universe <- expected_signal_universe[order(expected_signal_universe$id),]

  rownames(results@signal_universe_m_df@data) <- NULL
  rownames(expected_signal_universe) <- NULL

  expect_equal(results@signal_universe_m_df@data, expected_signal_universe)

  expect_true(
    all.equal(results@final_signal_universe_m_d_ref@data, signal_universe_m_d_ref_2[order(signal_universe_m_d_ref_2$id),],
              check.attributes = FALSE)
  )

})

test_that("run_ss_backtest works for vanilla bayesian setting", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_signal_selection_obj.RData", sep =""))

  set.seed(123)
  #Backtest Returns
  mocked_backtest_returns_xts <- xts::as.xts(data.frame(
    book_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.035),
    dy_med_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.025),
    eps_yield = rnorm(length(unique(signals_m_df$dates)), mean = 0.005, sd = 0.03),
    mom_res_12m = rnorm(length(unique(signals_m_df$dates)), mean = 0.015, sd = 0.035),
    roe_3m = rnorm(length(unique(signals_m_df$dates)), mean = 0.01, sd = 0.02),
    sharpe_6m = rnorm(length(unique(signals_m_df$dates)), mean = 0.025, sd = 0.035),
    low_vol_36m = rnorm(length(unique(signals_m_df$dates)), mean = 0.0075, sd = 0.0075)
  ), order.by = unique(signals_m_df$dates))

  #Priors
  #DGP 1
  ##############################
  set.seed(123)  # For reproducibility

  # Define themes and tickers
  themes <- c("value", "growth", "momentum", "defensive", "size")
  theme_ticker_map <- list(
    value = c("book_yield", "sales_yield", "dividend_yield", "asset_yield", "eps_yield",
              "ev_ebitda", "ev_ebit", "fcf_yield", "fcfe_yield", "ev_fcff"),
    defensive = c("g_eps", "g_sps", "g_dps", "sur", "g_roe", "g_roic", "g_fcfe", "g_fcf", "g_fcff",
                  "g_eps_36m", "g_sps_36m", "g_dps_36m", "sur_36m", "g_roe_36m", "g_roic_36m",
                  "g_fcfe_36m", "g_fcf_36m", "g_fcff_36m"),
    momentum = c("sharpe_6m", "sharpe_12m", "alpha_6m", "alpha_12m", "sharpe_ewma", "alpha_ewma",
                 "return_6m", "return_12m", "sharpe_3m", "alpha_3m", "return_3m")
  )

  # Expand theme-signal combinations into a data frame
  # Each row represents a theme and its corresponding signal
  theme_signal_combinations <- do.call(
    rbind,
    lapply(names(theme_ticker_map), function(theme) {
      data.frame(theme = theme, signal = theme_ticker_map[[theme]])
    })
  )

  # Define priors based on the model specification
  theme_effects_means <- c(0.0125, 0.005, 0.01, 0.003, -0.003)  # Fixed effects for themes
  names(theme_effects_means) <- themes
  theme_effects_sds <- c(0.003, 0.002, 0.002, 0.002, 0.003)  # SD for random effects at theme level
  names(theme_effects_sds) <- themes

  fixed_slope_mean <- 0.000      # Mean of the slope for market_factor_proxy (fixed effect)
  fixed_slope_sd <- 0.005        # SD of the slope for market_factor_proxy (fixed effect)

  random_intercept_tickers_sd  <- 0.01    # SD for random intercept at theme:tickers level
  random_slope_tickers_sd  <- 0.003        # SD for random slope at theme:tickers level
  residual_sd <- 0.0450                   # SD for residual error

  # Correlation between random intercept and slope for theme:tickers level
  correlation <- 0.2  # Approximate correlation from LKJ prior

  # Covariance matrix for random effects (intercept and slope) for theme:tickers
  cov_matrix <- matrix(
    c(random_intercept_tickers_sd^2,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      random_slope_tickers_sd^2),
    nrow = 2
  )

  # Generate data
  n_obs_per_ticker <- 3000

  # Expand the theme-ticker combinations
  theme_ticker_combinations <- do.call(rbind, lapply(names(theme_ticker_map), function(theme) {
    data.frame(theme = theme, ticker = theme_ticker_map[[theme]])
  }))

  # Generate random effects
  n_tickers <- nrow(theme_ticker_combinations)
  random_effects_tickers <- MASS::mvrnorm(n_tickers, mu = c(0, 0), Sigma = cov_matrix)

  # Predictor: market_factor_proxy
  market_factor_proxy <- rnorm(n_obs_per_ticker * n_tickers, mean = 0, sd = 1)

  # Generate monthly dates for the observations
  dates <- rep(
    seq.Date(as.Date("1980-01-01"), by = "month", length.out = n_obs_per_ticker),
    times = n_tickers
  )

  # Initialize the response variable (active_return)
  return <- numeric(length(market_factor_proxy))

  # Loop to calculate active_return for each observation
  for (i in seq_along(return)) {
    ticker_idx <- ((i - 1) %/% n_obs_per_ticker) + 1  # Identify signal index
    theme <- theme_ticker_combinations$theme[ticker_idx]
    ticker <- theme_ticker_combinations$ticker[ticker_idx]

    # Combine fixed effects, random effects, and residual noise
    return[i] <- rnorm(1, mean = theme_effects_means[theme], sd = theme_effects_sds[theme]) +   # Fixed intercept
      rnorm(1, mean = fixed_slope_mean, sd = fixed_slope_sd) * market_factor_proxy[i] +  # Fixed slope
      random_effects_tickers[ticker_idx, 1] +                                                    # Random intercept for theme:tickers
      random_effects_tickers[ticker_idx, 2] * market_factor_proxy[i] +                           # Random slope for theme:tickers
      rnorm(1, mean = 0, sd = residual_sd)                                               # Residual noise
  }

  # Assign signal names to each observation
  signal_names <- rep(theme_signal_combinations$signal, each = n_obs_per_ticker)
  theme_names <- rep(theme_signal_combinations$theme, each = n_obs_per_ticker)

  # Create the final data frame
  simulated_data <- data.frame(
    id = paste0(signal_names, "-", dates),  # Unique ID combining signal and date
    dates = dates,                          # Monthly dates
    theme = theme_names,                    # Theme names
    tickers = signal_names,                 # Signal names
    return = return,          # Response variable
    market_factor_proxy = market_factor_proxy  # Predictor variable
  )

  # Reorder columns as requested
  mocked_priors_m_df <- simulated_data[, c("id", "tickers", "dates", "return", "market_factor_proxy", "theme")]

  ##############################
  chosen_signals_and_positions <- c(book_yield = "long", eps_yield = "long", roe_3m = "long", sharpe_6m = "long", vol_36m = "short")
  bayesian_ss_config <- create_ss_backtest_config(initial_sample_size = 6, rebalancing_months = 6,
                                                  split_method = "expanding", config_name = "bayesian_ss", chosen_signals_and_positions = chosen_signals_and_positions) %>%
    add_alpha_test_strategy(model_structure = "partial_pooled", theme_level_intercept = "theme_specific", theme_level_slope = "theme_specific",
                            signal_significance_threshold = 0.10, p_correction_method = "bayesian",
                            market_factor_proxy = "IBOV", enable_theme_representativeness = TRUE,
                            lmer_control = list(lmer_optimizer = "Nelder_Mead")) %>%
    add_bayesian_model_parameters(brms_control = list(adapt_delta = 0.7, iter = 1000, warmup = 500))


  signals_m_df <- create_meta_dataframe(signals_m_df, "signals_123")
  signal_themes_m_df <- create_meta_dataframe(signal_themes_m_df, "st_11")
  priors_m_df <- create_meta_dataframe(mocked_priors_m_df[order(mocked_priors_m_df$id),], "priors_123")



  future::plan("multisession")
  results <- run_ss_backtest(bayesian_ss_config, priors_m_df = priors_m_df,
                             signals_m_df = signals_m_df, backtest_returns_xts = mocked_backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts,
                             signal_themes_m_df = signal_themes_m_df,
                             verbose = TRUE
  )


  ####Expected Results
  ####################
  ####################
  signals_m_df <- signals_m_df@data
  signal_themes_m_df <- signal_themes_m_df@data
  priors_m_df <- priors_m_df@data
  initial_sample_size <- bayesian_ss_config@initial_sample_size
  rebalancing_months <- bayesian_ss_config@rebalancing_months
  initial_sample_size <- bayesian_ss_config@initial_sample_size
  market_factor_proxy <- bayesian_ss_config@alpha_test_strategy@market_factor_proxy
  model_structure <- bayesian_ss_config@alpha_test_strategy@model_structure
  theme_level_intercept <- bayesian_ss_config@alpha_test_strategy@theme_level_intercept
  theme_level_slope <- bayesian_ss_config@alpha_test_strategy@theme_level_slope
  active_returns <- bayesian_ss_config@active_returns
  lmer_control <- bayesian_ss_config@alpha_test_strategy@lmer_control


  signal_universe_m_d_ref_list <- list()
  bayesian_fit_nested_list <- list()
  eligible_signals_list <- list()
  dates_m_vector <- unique(signals_m_df$dates)

  check_inputs_ss_backtest(rebalancing_months = rebalancing_months,
                           signals_m_df = signals_m_df, initial_sample_size = initial_sample_size,
                           chosen_signals_and_positions = chosen_signals_and_positions,
                           custom_signal_universe_metrics_m_df = NULL,
                           theme_level_intercept = theme_level_intercept, theme_level_slope = theme_level_slope,
                           active_returns = active_returns, model_structure = model_structure, forced_signals = NULL,
                           backtest_returns_xts = mocked_backtest_returns_xts, benchmark_returns_xts = benchmark_returns_xts,
                           p_correction_method = signal_selection_policy$p_correction_method, lmer_control = lmer_control,
                           enable_theme_representativeness = TRUE, market_factor_proxy = market_factor_proxy,
                           signal_significance_threshold = signal_selection_policy$signal_significance_threshold,
                           priors_m_df = priors_m_df, signal_themes_m_df = signal_themes_m_df
  )

  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    signal_themes_m_df = signal_themes_m_df,
    chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_xts = mocked_backtest_returns_xts
  )


  expect_equal(selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts,
               mocked_backtest_returns_xts[, c("book_yield", "eps_yield", "roe_3m", "sharpe_6m", "low_vol_36m")])

  expect_equal(selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df[,c("id", "tickers", "dates", "book_yield", "eps_yield", "roe_3m", "sharpe_6m")],
               signals_m_df[,c("id", "tickers", "dates", "book_yield", "eps_yield", "roe_3m", "sharpe_6m")])

  expect_equal(selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df$low_vol_36m,
               signals_m_df$vol_36m * -1)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, market_factor_proxy]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  #Second Rebalancing Month
  ##########################
  current_date <- dates_m_vector[9]

  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:9),]
  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:9), market_factor_proxy]
  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]
  priors_m_upd_ref <- priors_m_df[which(priors_m_df$dates <= current_date),]


  model_spec_theme_level <- "theme_specific_intercept_theme_specific_slope"
  summarize_performance_results <- summarize_performance(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_structure = model_structure,
    lmer_control = lmer_control,
    active_returns = active_returns,
    model_spec_theme_level = model_spec_theme_level
  )

  #Create signal universe
  signal_universe_m_d_ref_2 <- summarize_performance_results$signal_universe_m_d_ref

  #Get first priors
  priors_2 <- derive_informative_priors_from_data(priors_m_upd_ref = priors_m_upd_ref, lmer_optimizer = "Nelder_Mead",
                                                  model_spec_theme_level = model_spec_theme_level)

  expect_equal(priors_2$priors, results@bayesian_results$elected_priors)
  expect_equal(coef(priors_2$model), coef(results@bayesian_results$elected_priors_frequentist_model))

  selected_backtest_returns_corrected_positions_df <- as.data.frame(selected_backtest_returns_corrected_positions_xts_upd_ref) %>%
    tibble::rownames_to_column(var = "dates")
  selected_backtest_returns_corrected_positions_df$market_factor_proxy <- as.vector(selected_market_factor_proxy_xts_upd_ref)

  long_data <- tidyr::pivot_longer(selected_backtest_returns_corrected_positions_df, cols = c(-dates, -market_factor_proxy), names_to = "tickers", values_to = "return")
  long_data <- dplyr::left_join(long_data, dplyr::select(selected_signal_themes_m_d_ref, tickers, theme), by = "tickers")
  long_data$`theme:tickers` <- paste0(long_data$theme, "_", long_data$tickers)
  long_data <- long_data %>% as.data.frame()

  expect_equal(results@bayesian_results$brm_model$data$return, long_data$return)

  expect_equal(results@bayesian_results$brm_model$data$market_factor_proxy,
               long_data$market_factor_proxy)

  expect_equal(results@bayesian_results$brm_model$data$`theme:tickers`,
               long_data$`theme:tickers`)

  expect_equal(results@bayesian_results$brm_model$prior$prior[2],
               priors_2$priors$prior[1])

  expect_equal(as.character(results@bayesian_results$brm_model$formula$formula),
               c("~", "return",
               "0 + theme + theme:market_factor_proxy + (1 + market_factor_proxy | theme:tickers)"))

  set.seed(123)
  posterior_draws_2 <- summarize_posteriors_draws(brm_model = results@bayesian_results$brm_model,
                                                  signal_universe_m_d_ref = signal_universe_m_d_ref_2,
                                                  model_spec_theme_level = model_spec_theme_level,
                                                  selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref)


  signal_universe_m_d_ref_2 <- posterior_draws_2$signal_universe_m_d_ref

  signal_universe_m_d_ref_2[, "exp_ret_score"] <- signal_transform(
    signal_universe_m_d_ref_2$posterior_alpha_t_stat,
    upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization
  )


  #Create benchmarks
  top_assets <- rep(0, length(signal_universe_m_d_ref_2$pd_alpha))
  top_assets[which((1-signal_universe_m_d_ref_2$pd_alpha) <= bayesian_ss_config@alpha_test_strategy@signal_significance_threshold)] <- 1
  signal_universe_m_d_ref_2$top_assets <- top_assets
  se_benchmarks <- create_se_benchmarks(signal_universe_m_d_ref_2, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref)


  benchmark_weights_m_d_ref <- create_se_benchmarks(
    signal_universe_m_d_ref_2, selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref
  )

  expect_equal(benchmark_weights_m_d_ref$theme_ss, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  expect_equal(benchmark_weights_m_d_ref$theme_sb, c(1/3/2, 1/3/2, 1/3/2, 1/3, 1/3/2))
  signal_universe_m_d_ref_2$theme_ss_bench_weights <- benchmark_weights_m_d_ref$theme_ss
  signal_universe_m_d_ref_2$theme_sb_bench_weights <- benchmark_weights_m_d_ref$theme_sb
  signal_universe_m_d_ref_2$theme <- c("value", "value", "defensive", "momentum", "defensive")
  signal_universe_m_d_ref_2$is_eligible <- c(1,1,1,1,1)



  expect_equal(signal_universe_m_d_ref_2,
               classify_investment_universe(signals_m_d_ref = signal_universe_m_d_ref_2[,c(1:60)],
                                            signal_significance_threshold = bayesian_ss_config@alpha_test_strategy@signal_significance_threshold,
                                            groups_m_d_ref = selected_signal_themes_m_d_ref,
                                            concentration_constraint_policy = list(
                                              benchmark = c("theme_ss", "theme_sb"),
                                              max_abs_active_group_weight = 0.1
                                            ),
                                            asset_object = "signals"
               )
  )

  signal_universe_m_d_ref_2$exp_ret_score <- NULL


  ordered_final_signal_universe <- results@final_signal_universe_m_d_ref@data[order(results@final_signal_universe_m_d_ref@data$id),]
  ordered_signal_universe_m_d_ref_2 <- signal_universe_m_d_ref_2[order(signal_universe_m_d_ref_2$id),]
  expect_equal(ordered_final_signal_universe[,c(1:11, 15:19, 21:27)], #Exlude those that are based on random preds (not epreds)
               ordered_signal_universe_m_d_ref_2[,c(1:11, 15:19, 21:27)])

})


