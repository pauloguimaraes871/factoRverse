test_that("define_signal_elibility works for frequentist setting", {


  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals <- signal_selection_policy$chosen_signals
  signal_positions <- signal_selection_policy$signal_positions
  signal_significance_threshold <- signal_selection_policy$signal_significance_threshold
  p_correction_method <- signal_selection_policy$p_correction_method
  data_availability_cutoff <- signal_selection_policy$data_availability_cutoff

  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(chosen_signals = chosen_signals, signal_positions = signal_positions,
                                                                   signals_m_df = signals_m_df, backtest_returns_df = backtest_returns_df)

  selected_backtest_returns_corrected_positions_df <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_df
  selected_market_factor_proxy_df <- benchmark_returns_df[, c("dates", "IBOV")]

  current_date <- "2001-06-15"

  selected_backtest_returns_corrected_positions_upd_ref <-
    selected_backtest_returns_corrected_positions_df[which(selected_backtest_returns_corrected_positions_df$dates <= current_date), ]

  selected_market_factor_proxy_upd_ref <-
    selected_market_factor_proxy_df[which(selected_market_factor_proxy_df$dates <= current_date),]

  signal_themes_m_d_ref <- signal_themes_m_df[which(signal_themes_m_df$dates == current_date),]

  #expected results
  expected_result <- data.frame(id = paste0(colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1],"-",current_date),
                                tickers = colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1], dates = current_date)
  expected_result$dates <- as.Date(expected_result$dates, format = "%Y-%m-%d")
  expected_result$mean_active_return <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) mean(x))
  expected_result$tracking_error <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) sd(x))
  expected_result$IR <- expected_result$mean_active_return/expected_result$tracking_error

  lm_model_summary_list <- purrr::map(lapply(selected_backtest_returns_corrected_positions_upd_ref[,-1], as.vector),
                                      ~ summary(lm(.x ~ selected_market_factor_proxy_upd_ref$IBOV)))

  expected_result$alpha <- sapply(lm_model_summary_list, function(x) x$coefficients[1])
  expected_result$alpha_t_stat <- sapply(lm_model_summary_list, function(x) x$coefficients[5])
  expected_result$beta <- sapply(lm_model_summary_list, function(x) x$coefficients[2])
  expected_result$treynor <- expected_result$mean_active_return/expected_result$beta
  expected_result$p_value <- sapply(lm_model_summary_list, function(x) x$coefficients[7])
  expected_result$adjusted_p_value <- p.adjust(expected_result$p_value, p_correction_method)

  #final signal
  expected_result$final_signal <-
    signal_transform(expected_result[, "alpha_t_stat"],
                     upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization)


  #Classify
  concentration_constraint_policy_test <-
    list(benchmark = c("theme_ss", "theme_sb"), max_abs_active_group_weight = 0.1)

  expected_result <- list(
    signal_universe_m_d_ref = classify_investment_universe(expected_result, signal_significance_threshold = signal_significance_threshold,
                                                           groups_m_d_ref = signal_themes_m_d_ref,
                                                           concentration_constraint_policy = concentration_constraint_policy_test,
                                                           asset_object = "signals")
  )

  expected_result$signal_universe_m_d_ref$final_signal <- NULL
  #results
  results <- define_signal_eligibility(selected_backtest_returns_corrected_positions_upd_ref = selected_backtest_returns_corrected_positions_upd_ref,
                                       selected_market_factor_proxy_upd_ref = selected_market_factor_proxy_upd_ref,
                                       data_availability_cutoff = data_availability_cutoff,
                                       p_correction_method = p_correction_method,
                                       signal_significance_threshold = signal_significance_threshold,
                                       enable_theme_representativeness = TRUE,
                                       priors_m_upd_ref = NULL, user_priors = NULL,
                                       signal_themes_m_d_ref = signal_themes_m_d_ref)

  expect_equal(results,expected_result)


})

test_that("define_signal_elibility works for frequentist setting when there is a backtest with short length", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals <- signal_selection_policy$chosen_signals
  signal_positions <- signal_selection_policy$signal_positions
  signal_significance_threshold <- signal_selection_policy$signal_significance_threshold
  p_correction_method <- signal_selection_policy$p_correction_method
  data_availability_cutoff <- signal_selection_policy$data_availability_cutoff

  #repalce with NA
  backtest_returns_df$low_Beta[1:3] <- NA

  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(chosen_signals = chosen_signals, signal_positions = signal_positions,
                                                                   signals_m_df = signals_m_df, backtest_returns_df = backtest_returns_df)

  selected_backtest_returns_corrected_positions_df <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_df
  selected_market_factor_proxy_df <- benchmark_returns_df[, c("dates", "IBOV")]

  current_date <- "2001-06-15"

  selected_backtest_returns_corrected_positions_upd_ref <-
    selected_backtest_returns_corrected_positions_df[which(selected_backtest_returns_corrected_positions_df$dates <= current_date), ]

  selected_market_factor_proxy_upd_ref <-
    selected_market_factor_proxy_df[which(selected_market_factor_proxy_df$dates <= current_date),]

  signal_themes_m_d_ref <- signal_themes_m_df[which(signal_themes_m_df$dates == current_date),]

  #expected results
  expected_result <- data.frame(id = paste0(colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1],"-",current_date), tickers = colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1], dates = current_date)
  expected_result$dates <- as.Date(expected_result$dates, format = "%Y-%m-%d")
  expected_result$mean_active_return <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) mean(x))
  expected_result$tracking_error <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) sd(x))
  expected_result$IR <- expected_result$mean_active_return/expected_result$tracking_error

  lm_model_summary_list <- purrr::map(lapply(selected_backtest_returns_corrected_positions_upd_ref[,-1], as.vector),
                                      ~ summary(lm(.x ~ selected_market_factor_proxy_upd_ref$IBOV)))

  expected_result$alpha <- sapply(lm_model_summary_list, function(x) x$coefficients[1])
  expected_result$alpha_t_stat <- sapply(lm_model_summary_list, function(x) x$coefficients[5])
  expected_result$beta <- sapply(lm_model_summary_list, function(x) x$coefficients[2])
  expected_result$treynor <- expected_result$mean_active_return/expected_result$beta
  expected_result$p_value <- sapply(lm_model_summary_list, function(x) x$coefficients[7])
  expected_result$adjusted_p_value <- p.adjust(expected_result$p_value, p_correction_method)

  #adjust for backtset with inadequate length
  expected_result[2,-c(1:3)] <- NA

  #final signal
  expected_result$final_signal <- signal_transform(expected_result[, "alpha_t_stat"], upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization)


  #Classify
  concentration_constraint_policy_test <-
    list(benchmark = c("theme_ss", "theme_sb"), max_abs_active_group_weight = 0.1)

  expected_result <- list(
    signal_universe_m_d_ref = classify_investment_universe(expected_result, signal_significance_threshold = signal_significance_threshold,
                                                           groups_m_d_ref = signal_themes_m_d_ref, concentration_constraint_policy = concentration_constraint_policy_test,
                                                           asset_object = "signals")
  )
  expected_result$signal_universe_m_d_ref$final_signal <- NULL

  expect_equal(
    suppressWarnings(define_signal_eligibility(selected_backtest_returns_corrected_positions_upd_ref = selected_backtest_returns_corrected_positions_upd_ref,
                                               selected_market_factor_proxy_upd_ref = selected_market_factor_proxy_upd_ref,
                                               data_availability_cutoff = data_availability_cutoff,
                                               p_correction_method = p_correction_method,
                                               signal_significance_threshold = signal_significance_threshold,
                                               enable_theme_representativeness = TRUE,
                                               priors_m_upd_ref = NULL,
                                               signal_themes_m_d_ref = signal_themes_m_d_ref)),
    expected_result)


})

test_that("define_signal_elibility works for bayesian setting", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals <- signal_selection_policy$chosen_signals
  signal_positions <- signal_selection_policy$signal_positions
  signal_significance_threshold <- signal_selection_policy$signal_significance_threshold
  max_abs_active_group_weight <- signal_selection_policy$max_abs_active_group_weight
  p_correction_method <- "bayesian"

  current_date <- "2001-06-15"

  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(chosen_signals = chosen_signals, signal_positions = signal_positions,
                                                                   signals_m_df = signals_m_df, backtest_returns_df = backtest_returns_df)

  selected_backtest_returns_corrected_positions_df <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_df
  selected_market_factor_proxy_df <- benchmark_returns_df[, c("dates", "IBOV")]

  selected_backtest_returns_corrected_positions_upd_ref <-
    selected_backtest_returns_corrected_positions_df[which(selected_backtest_returns_corrected_positions_df$dates <= current_date), ]

  selected_market_factor_proxy_upd_ref <-
    selected_market_factor_proxy_df[which(selected_market_factor_proxy_df$dates <= current_date),]

  signal_themes_m_d_ref <- signal_themes_m_df[which(signal_themes_m_df$dates == current_date),]

  #Select priors
  priors_m_upd_ref <- priors_m_df[which(priors_m_df$dates <= current_date),]


  #expected results
  expected_result <- data.frame(id = paste0(colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1],"-",current_date),
                                tickers = colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1], dates = current_date)
  expected_result$dates <- as.Date(expected_result$dates, format = "%Y-%m-%d")
  expected_result$mean_active_return <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) mean(x))
  expected_result$tracking_error <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) sd(x))
  expected_result$IR <- expected_result$mean_active_return/expected_result$tracking_error

  lm_model_summary_list <- purrr::map(lapply(selected_backtest_returns_corrected_positions_upd_ref[,-1], as.vector),
                                      ~ summary(lm(.x ~ selected_market_factor_proxy_upd_ref$IBOV)))

  expected_result$alpha <- sapply(lm_model_summary_list, function(x) x$coefficients[1])
  expected_result$alpha_t_stat <- sapply(lm_model_summary_list, function(x) x$coefficients[5])
  expected_result$beta <- sapply(lm_model_summary_list, function(x) x$coefficients[2])
  expected_result$treynor <- expected_result$mean_active_return/expected_result$beta
  expected_result$p_value <- sapply(lm_model_summary_list, function(x) x$coefficients[7])

  #Bayesian adjustment
  user_priors <- c(
    brms::set_prior("normal(0, 0.5)", class = "Intercept"),
    brms::set_prior("normal(0, 0.5)", class = "b", coef = "market_factor_proxy"),
    brms::set_prior("student_t(30,0,0.2)", class = "sd", coef = "Intercept", group = "theme:tickers"),
    brms::set_prior("student_t(30,0,0.2)", class = "sd", coef = "market_factor_proxy", group = "theme:tickers"),
    brms::set_prior("student_t(30,0,0.2)", class = "sd", coef = "Intercept", group = "theme")
  )

  set.seed(123)
  bayesian_results <- suppressWarnings(
    bayesian_adjustment(
    signal_universe_m_d_ref = expected_result,
    selected_backtest_returns_corrected_positions_upd_ref = selected_backtest_returns_corrected_positions_upd_ref,
    selected_market_factor_proxy_vector_upd_ref = selected_market_factor_proxy_upd_ref$IBOV,
    user_priors = user_priors,
    signal_themes_m_d_ref = signal_themes_m_d_ref
  )
  )

  expected_result <- bayesian_results$posterior_signal_universe_m_d_ref
  #final signal
  expected_result$final_signal <- signal_transform(expected_result[, paste0("posterior_", "alpha_t_stat")],
                                                   lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization)

  #Classify
  concentration_constraint_policy_test <- list(benchmarks = c("theme_sb", "theme_ss"), max_abs_active_group_weight = NULL)
  #Adjust significance threshold to have at least one significant signal
  signal_significance_threshold <- 1.30 #this makes no sense
  p_correction_method <- "bayesian" #this makes no sense

  expected_result <- list(
    signal_universe_m_d_ref = classify_investment_universe(expected_result, signal_significance_threshold = signal_significance_threshold,
                                                           groups_m_d_ref = signal_themes_m_d_ref, concentration_constraint_policy = concentration_constraint_policy_test,
                                                           asset_object = "signals"),
    bayesian_fit_list = bayesian_results[-1]
  )

  #results
  set.seed(123)
  results <- suppressWarnings(define_signal_eligibility(
    selected_backtest_returns_corrected_positions_upd_ref = selected_backtest_returns_corrected_positions_upd_ref,
    selected_market_factor_proxy_upd_ref = selected_market_factor_proxy_upd_ref,
    data_availability_cutoff = signal_selection_policy$data_availability_cutoff,
    p_correction_method = "bayesian",
    signal_significance_threshold = signal_significance_threshold, enable_theme_representativeness = FALSE,
    user_priors = user_priors,
    signal_themes_m_d_ref = signal_themes_m_d_ref
  ))

  expect_equal(results$signal_universe_m_d_ref, expected_result$signal_universe_m_d_ref)


})
