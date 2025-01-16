test_that("summarize_performance works for no_pooled model_structure", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"


  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions, signal_themes_m_df = signal_themes_m_df,
    signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, "IBOV"]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df


  current_date <- "2001-06-15"

  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:4), ]

  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:4),]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]


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
  p_value <- vector()
  model_list <- list()
  for(j in 1:3){
    model_list[[j]] <- lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)
    alpha[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,1]
    alpha_se[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,2]
    beta[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[2,1]
    specific_risk[j] <- sigma(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref))
    alpha_t_stat[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,3]
    p_value[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,4]/2
  }
  treynor_ratio <- apply(selected_backtest_returns_corrected_positions_xts_upd_ref, 2, function(x){
   PerformanceAnalytics::mean.geometric(x/100)*100
  })/beta
  app_ratio <- alpha/specific_risk

  #Result
  result <- summarize_performance(selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
                                  selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
                                  model_structure = "no_pooled", model_spec_theme_level = NULL, lmer_control = NULL,
                                  selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref
                                  )

  expect_equal(result$signal_universe_m_d_ref$alpha, alpha)
  expect_equal(result$signal_universe_m_d_ref$alpha_se, alpha_se)
  expect_equal(result$signal_universe_m_d_ref$beta, beta)
  expect_equal(result$signal_universe_m_d_ref$specific_risk, specific_risk)
  expect_equal(result$signal_universe_m_d_ref$alpha_t_stat, alpha_t_stat)
  expect_equal(result$signal_universe_m_d_ref$treynor_ratio, as.numeric(treynor_ratio))
  expect_equal(result$signal_universe_m_d_ref$appraisal_ratio, as.numeric(app_ratio))
  expect_equal(result$signal_universe_m_d_ref$p_value, as.numeric(p_value))
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


  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions,
    signal_themes_m_df = signal_themes_m_df,
    signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, "IBOV"]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  current_date <- "2001-06-15"

  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:4), ]

  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:4),]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]


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
  p_value <- vector()
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
    p_value[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,4]/2
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
                                  selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref
  )

  expect_equal(result$signal_universe_m_d_ref$alpha, alpha)
  expect_equal(result$signal_universe_m_d_ref$alpha_se, alpha_se)
  expect_equal(result$signal_universe_m_d_ref$beta, beta)
  expect_equal(result$signal_universe_m_d_ref$specific_risk, specific_risk)
  expect_equal(result$signal_universe_m_d_ref$alpha_t_stat, alpha_t_stat)
  expect_equal(result$signal_universe_m_d_ref$treynor_ratio, as.numeric(treynor_ratio))
  expect_equal(result$signal_universe_m_d_ref$appraisal_ratio, as.numeric(app_ratio))
  expect_equal(result$signal_universe_m_d_ref$p_value, as.numeric(p_value))

  expect_equal(as.numeric(result$frequentist_fit_results_list[[1]]$coefficients), as.numeric(model_list[[1]]$coefficients))
  expect_equal(result$frequentist_fit_results_list[[2]]$coefficients, model_list[[2]]$coefficients)
  expect_equal(result$frequentist_fit_results_list[[3]]$coefficients, model_list[[3]]$coefficients)

})

test_that("summarize_performance works for no_pooled model_structure and custom_signal_metrics", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  signals <- c("Alpha", "Gamma", "low_Beta", "Delta")
  dates <- seq.Date(from = as.Date("2001-03-15"), to = as.Date("2001-05-15"), by = "months")

  mocked_custom_signal_universe_metrics_m_df <- expand.grid(tickers = signals, dates = dates) %>%
    dplyr::mutate(id = paste0(tickers, "-", dates), .before = tickers) %>%
    dplyr::arrange(id)

  set.seed(123)
  mocked_custom_signal_universe_metrics_m_df$pe <- rnorm(nrow(mocked_custom_signal_universe_metrics_m_df), mean = 10, sd = 2)
  mocked_custom_signal_universe_metrics_m_df$pb <- rnorm(nrow(mocked_custom_signal_universe_metrics_m_df), mean = 1.5, sd = 0.5)
  mocked_custom_signal_universe_metrics_m_df$div_yield <- rnorm(nrow(mocked_custom_signal_universe_metrics_m_df), mean = 0.03, sd = 0.01)
  mocked_custom_signal_universe_metrics_m_df$hr <- rnorm(nrow(mocked_custom_signal_universe_metrics_m_df), mean = 0.03, sd = 0.01)
  mocked_custom_signal_universe_metrics_m_df$hr <- rnorm(nrow(mocked_custom_signal_universe_metrics_m_df), mean = 0.03, sd = 0.01)


  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"


  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions, signal_themes_m_df = signal_themes_m_df,
    signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, "IBOV"]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df


  current_date <- "2001-06-15"

  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:4), ]

  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:4),]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]

  custom_signal_universe_metrics_m_upd_ref <- mocked_custom_signal_universe_metrics_m_df[which(mocked_custom_signal_universe_metrics_m_df$dates <= current_date),]

  #Create base_signal_universe_m_d_ref
  base_signal_universe_m_d_ref <- create_performance_m_df(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    active_returns = TRUE
  )

  #Add most recent custom signal metrics
  custom_signal_universe_metrics_m_d_ref <- custom_signal_universe_metrics_m_upd_ref %>% dplyr::filter(dates == "2001-05-15") %>% dplyr::select(-id, -dates)

  #Join
  base_signal_universe_m_d_ref <- base_signal_universe_m_d_ref %>% dplyr::left_join(custom_signal_universe_metrics_m_d_ref, by = c("tickers" = "tickers"))




  #CAPM list
  alpha <- vector()
  alpha_se <- vector()
  beta <- vector()
  specific_risk <- vector()
  alpha_t_stat <- vector()
  p_value <- vector()
  model_list <- list()
  for(j in 1:3){
    model_list[[j]] <- lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)
    alpha[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,1]
    alpha_se[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,2]
    beta[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[2,1]
    specific_risk[j] <- sigma(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref))
    alpha_t_stat[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,3]
    p_value[j] <- coef(summary(lm(selected_backtest_returns_corrected_positions_xts_upd_ref[,j] ~ selected_market_factor_proxy_xts_upd_ref)))[1,4]/2
  }
  treynor_ratio <- apply(selected_backtest_returns_corrected_positions_xts_upd_ref, 2, function(x){
    PerformanceAnalytics::mean.geometric(x/100)*100
  })/beta
  app_ratio <- alpha/specific_risk

  #Result
  suppressWarnings(
  result <- summarize_performance(selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
                                  custom_signal_universe_metrics_m_upd_ref = custom_signal_universe_metrics_m_upd_ref,
                                  selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
                                  model_structure = "no_pooled", model_spec_theme_level = NULL, lmer_control = NULL,
                                  selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref
  )
  )

  expect_equal(result$signal_universe_m_d_ref$alpha, alpha)
  expect_equal(result$signal_universe_m_d_ref$alpha_se, alpha_se)
  expect_equal(result$signal_universe_m_d_ref$beta, beta)
  expect_equal(result$signal_universe_m_d_ref$specific_risk, specific_risk)
  expect_equal(result$signal_universe_m_d_ref$alpha_t_stat, alpha_t_stat)
  expect_equal(result$signal_universe_m_d_ref$treynor_ratio, as.numeric(treynor_ratio))
  expect_equal(result$signal_universe_m_d_ref$appraisal_ratio, as.numeric(app_ratio))
  expect_equal(result$signal_universe_m_d_ref$p_value, as.numeric(p_value))
  expected_custom_signal_universe_metrics_m_d_ref <- custom_signal_universe_metrics_m_d_ref %>% dplyr::filter(!tickers %in% "Delta") %>%
    dplyr::select(pe, pb, div_yield, hr) %>% as.data.frame()
  attr(expected_custom_signal_universe_metrics_m_d_ref, "out.attrs") <- NULL

  expect_equal(result$signal_universe_m_d_ref %>% dplyr::select(pe, pb, div_yield, hr) %>% as.data.frame(), expected_custom_signal_universe_metrics_m_d_ref)
  expect_equal(result$frequentist_fit_results_list[[1]]$coefficients, model_list[[1]]$coefficients)
  expect_equal(result$frequentist_fit_results_list[[2]]$coefficients, model_list[[2]]$coefficients)
  expect_equal(result$frequentist_fit_results_list[[3]]$coefficients, model_list[[3]]$coefficients)



})

test_that("summarize_performance works for pooled model_structure", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"


  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions,
    signal_themes_m_df = signal_themes_m_df,
    signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, "IBOV"]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  current_date <- "2001-06-15"

  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:4), ]

  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:4),]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]


  #Create base_signal_universe_m_d_ref
  base_signal_universe_m_d_ref <- create_performance_m_df(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    active_returns = TRUE
  )

  #Fit frequentist hierarchical model
  frequentist_fit_results_list <- fit_frequentist_hierarchical_model(
    signal_universe_m_d_ref = base_signal_universe_m_d_ref,
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_spec_theme_level = "theme_specific_intercept_fixed_slope",
    lmer_optimizer = "Nelder_Mead", lmer_optimization_objective = TRUE, hierarchical_p_value_method = "Satterthwaite"
  )

  expected_result <- dplyr::left_join(base_signal_universe_m_d_ref,
                                      dplyr::select(frequentist_fit_results_list$pooled_CAPM_metrics_m_d_ref, -tickers, -dates), by = "id")

  result <- summarize_performance(selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
                                 selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
                                 model_structure = "partial_pooled", model_spec_theme_level = "theme_specific_intercept_fixed_slope",
                                 lmer_control = list(lmer_optimizer = "Nelder_Mead", lmer_optimization_objective  = "REML", hierarchical_p_value_method = "Satterthwaite"),
                                 selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref
  )

  expect_equal(result$signal_universe_m_d_ref, expected_result)
  expect_equal(coef(result$frequentist_fit_results_list), coef(frequentist_fit_results_list$lmer_model))

})

test_that("summarize_performance works for pooled model_structure and NAs", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"



  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions,
    signal_themes_m_df = signal_themes_m_df,
    signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_backtest_returns_corrected_positions_xts$Gamma[c(1:4)] <- NA
  selected_backtest_returns_corrected_positions_xts$Alpha[c(1:2)] <- NA

  selected_market_factor_proxy_xts <- benchmark_returns_xts[, "IBOV"]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  current_date <- "2001-06-15"

  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:4), ]

  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:4),]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]



  #Create base_signal_universe_m_d_ref
  base_signal_universe_m_d_ref <- suppressWarnings(create_performance_m_df(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    active_returns = TRUE
  ))

  #Fit frequentist hierarchical model
  frequentist_fit_results_list <- fit_frequentist_hierarchical_model(
    signal_universe_m_d_ref = base_signal_universe_m_d_ref,
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_spec_theme_level = "theme_specific_intercept_fixed_slope",
    lmer_optimizer = "Nelder_Mead", lmer_optimization_objective = TRUE, hierarchical_p_value_method = "Satterthwaite"
  )

  expected_result <- dplyr::left_join(base_signal_universe_m_d_ref,
                                      dplyr::select(frequentist_fit_results_list$pooled_CAPM_metrics_m_d_ref, -tickers, -dates), by = "id")

  result <- suppressWarnings(summarize_performance(selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
                                  selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
                                  model_structure = "partial_pooled", model_spec_theme_level = "theme_specific_intercept_fixed_slope",
                                  lmer_control = list(lmer_optimizer = "Nelder_Mead", lmer_optimization_objective  = "REML", hierarchical_p_value_method = "Satterthwaite"),
                                  selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref
  ))

  expect_equal(result$signal_universe_m_d_ref, expected_result)
  expect_equal(coef(result$frequentist_fit_results_list), coef(frequentist_fit_results_list$lmer_model))

})
