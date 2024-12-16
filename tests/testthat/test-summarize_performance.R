test_that("summarize_performance works for no_pooled model_structure", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"
  data_availability_cutoff <- 3

  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions,
    signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, "IBOV"]

  current_date <- "2001-06-15"

  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:4), ]

  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:4),]

  signal_themes_m_d_ref <- signal_themes_m_df[which(signal_themes_m_df$dates == current_date),]


  #Create base_signal_universe_m_d_ref
  base_signal_universe_m_d_ref <- create_performance_m_df(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    active_returns = TRUE
  )

  #CAPM list
  alpha <- vector()
  alpha_se <- vector()
  beta <- vector()
  specific_risk <- vector()
  alpha_t_stat <- vector()
  model_list <- list()
  for(j in 1:3){
    model_list[[j]] <- lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)
    alpha[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,1]
    alpha_se[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,2]
    beta[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[2,1]
    specific_risk[j] <- sigma(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref))
    alpha_t_stat[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,3]
  }
  treynor_ratio <- apply(selected_backtest_returns_corrected_positions_xts_upd_ref, 2, function(x){
   PerformanceAnalytics::mean.geometric(x/100)*100
  })/beta
  app_ratio <- alpha/specific_risk

  #Result
  result <- summarize_performance(selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
                                  selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
                                  model_structure = "no_pooled", model_spec_theme_level = NULL, lmer_control = NULL,
                                  signal_themes_m_d_ref = signal_themes_m_d_ref
                                  )

  expect_equal(result$signal_universe_m_d_ref$alpha, alpha)
  expect_equal(result$signal_universe_m_d_ref$alpha_se, alpha_se)
  expect_equal(result$signal_universe_m_d_ref$beta, beta)
  expect_equal(result$signal_universe_m_d_ref$specific_risk, specific_risk)
  expect_equal(result$signal_universe_m_d_ref$alpha_t_stat, alpha_t_stat)
  expect_equal(result$signal_universe_m_d_ref$treynor_ratio, as.numeric(treynor_ratio))
  expect_equal(result$signal_universe_m_d_ref$appraisal_ratio, as.numeric(app_ratio))
  expect_equal(result$frequentist_fit_results_list[[1]]$coefficients, model_list[[1]]$coefficients)
  expect_equal(result$frequentist_fit_results_list[[2]]$coefficients, model_list[[2]]$coefficients)
  expect_equal(result$frequentist_fit_results_list[[3]]$coefficients, model_list[[3]]$coefficients)



})

test_that("summarize_performance works for no_pooled model_structure and NAs", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"
  data_availability_cutoff <- 3

  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions,
    signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, "IBOV"]

  current_date <- "2001-06-15"

  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:4), ]

  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:4),]

  signal_themes_m_d_ref <- signal_themes_m_df[which(signal_themes_m_df$dates == current_date),]


  selected_backtest_returns_corrected_positions_xts_upd_ref$Alpha[1:2] <- NA
  #Create base_signal_universe_m_d_ref
  base_signal_universe_m_d_ref <- create_performance_m_df(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    active_returns = TRUE
  )

  #CAPM list
  alpha <- vector()
  alpha_se <- vector()
  beta <- vector()
  specific_risk <- vector()
  alpha_t_stat <- vector()
  model_list <- list()

  alpha[1] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[3:4,1] ~ selected_market_factor_proxy_xts_upd_ref[3:4])))[1,1]
  alpha_se[1] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[3:4,1] ~ selected_market_factor_proxy_xts_upd_ref[3:4])))[1,2]
  beta[1] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[3:4,1] ~ selected_market_factor_proxy_xts_upd_ref[3:4])))[2,1]
  specific_risk[1] <- sigma(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[3:4,1] ~ selected_market_factor_proxy_xts_upd_ref[3:4]))
  alpha_t_stat[1] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[3:4,1] ~ selected_market_factor_proxy_xts_upd_ref[3:4])))[1,3]
  model_list[[1]] <- lm(selected_backtest_returns_corrected_positions_xts_upd_ref[3:4,1] ~ selected_market_factor_proxy_xts_upd_ref[3:4])
  for(j in 2:3){
    alpha[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,1]
    alpha_se[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,2]
    beta[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[2,1]
    specific_risk[j] <- sigma(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref))
    alpha_t_stat[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,3]
    model_list[[j]] <- lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)
  }
  treynor_ratio <- apply(selected_backtest_returns_corrected_positions_xts_upd_ref, 2, function(x){
    PerformanceAnalytics::mean.geometric(x/100)*100
  })/beta
  app_ratio <- alpha/specific_risk



  #Result
  result <- summarize_performance(selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
                                  selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
                                  model_structure = "no_pooled", model_spec_theme_level = NULL, lmer_control = NULL,
                                  signal_themes_m_d_ref = signal_themes_m_d_ref
  )

  expect_equal(result$signal_universe_m_d_ref$alpha, alpha)
  expect_equal(result$signal_universe_m_d_ref$alpha_se, alpha_se)
  expect_equal(result$signal_universe_m_d_ref$beta, beta)
  expect_equal(result$signal_universe_m_d_ref$specific_risk, specific_risk)
  expect_equal(result$signal_universe_m_d_ref$alpha_t_stat, alpha_t_stat)
  expect_equal(result$signal_universe_m_d_ref$treynor_ratio, as.numeric(treynor_ratio))
  expect_equal(result$signal_universe_m_d_ref$appraisal_ratio, as.numeric(app_ratio))

  expect_equal(as.numeric(result$frequentist_fit_results_list[[1]]$coefficients), as.numeric(model_list[[1]]$coefficients))
  expect_equal(result$frequentist_fit_results_list[[2]]$coefficients, model_list[[2]]$coefficients)
  expect_equal(result$frequentist_fit_results_list[[3]]$coefficients, model_list[[3]]$coefficients)

})
