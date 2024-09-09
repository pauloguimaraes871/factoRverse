test_that("set_final_signal correctly works for multiple signals", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-06-15"

  #Subset
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  target_m_upd_ref <- target_m_df[which(target_m_df$dates <= current_date),]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]
  selected_benchmark_returns_upd_ref <- benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)]
  priors_m_upd_ref_list <- list(jkp_emerging = priors_m_df_list$jkp_emerging[which(priors_m_df_list$jkp_emerging$dates <= current_date), ])
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]


  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(signal_selection_policy = signal_selection_policy, signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref)
  selected_signals_backtest_returns_upd_ref <- selected_signals_and_backtest_list$selected_signals_backtest_returns_upd_ref

  #Define signal eligibilirt
  signal_eligibility_results_list <- define_signal_eligibility(
    selected_signals_backtest_returns_upd_ref = selected_signals_backtest_returns_upd_ref,
    selected_benchmark_returns_upd_ref = selected_benchmark_returns_upd_ref,
    signal_selection_policy = signal_selection_policy,
    signals_groups_m_d_ref = signals_groups_m_d_ref
  )

  #EW Portfolio for eligible
  signal_universe_m_d_ref <- signal_eligibility_results_list$signal_universe_m_d_ref
  signal_universe_m_d_ref <-
    set_portfolio_weights(universe_m_d_ref = signal_universe_m_d_ref,
                          portfolio_construction_method = signal_selection_policy$signal_blending_method)

  #Expected final result
  expected_results <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_upd_ref
  expected_results$final_signal <- expected_results$Alpha*0.5+ expected_results$low_Beta*0.5
  expected_results$final_signal <- expected_results$final_signal %>%
    signal_transform(upper_quantile_winsorization = upper_quantile_winsorization,
                     lower_quantile_winsorization = lower_quantile_winsorization)

  results <- set_final_signal(
    selected_signals_corrected_positions_m_d_ref = selected_signals_and_backtest_list$selected_signals_corrected_positions_m_upd_ref,
    signal_weights = signal_universe_m_d_ref$weights,
    ml_walk_forward_validation_results = NULL,
    eligible_signals = NULL,
    upper_quantile_winsorization = upper_quantile_winsorization,
    lower_quantile_winsorization = lower_quantile_winsorization
  )



  expect_equal(expected_results, results$selected_signals_corrected_positions_m_d_ref)


})

test_that("set_final_signal correctly works for single signal", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-06-15"

  #Subset
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  target_m_upd_ref <- target_m_df[which(target_m_df$dates <= current_date),]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]
  selected_benchmark_returns_upd_ref <- benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)]
  priors_m_upd_ref_list <- list(jkp_emerging = priors_m_df_list$jkp_emerging[which(priors_m_df_list$jkp_emerging$dates <= current_date), ])
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]


  #Select signals based on user choice
  signal_selection_policy$chosen_signals <- "Alpha"
  signal_selection_policy$signal_positions <- c(Alpha = "long")
  selected_signals_and_backtest_list <- select_and_correct_signals(signal_selection_policy = signal_selection_policy, signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref)
  selected_signals_backtest_returns_upd_ref <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_upd_ref

  #EW Portfolio
  signal_universe_m_d_ref <- data.frame(tickers = "Alpha", is_eligible = 1)
  signal_universe_m_d_ref <- set_portfolio_weights(universe_m_d_ref = signal_universe_m_d_ref,
                                                   portfolio_construction_method = signal_selection_policy$signal_blending_method)

  #Expected final result
  expected_results <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_upd_ref
  expected_results$final_signal <- expected_results$Alpha
  expected_results$final_signal <- expected_results$final_signal %>%
    signal_transform(upper_quantile_winsorization = upper_quantile_winsorization,
                     lower_quantile_winsorization = lower_quantile_winsorization)

  results <- set_final_signal(
    selected_signals_corrected_positions_m_d_ref = selected_signals_and_backtest_list$selected_signals_corrected_positions_m_upd_ref,
    signal_weights = signal_universe_m_d_ref$weights,
    ml_walk_forward_validation_results = NULL,
    eligible_signals = NULL,
    upper_quantile_winsorization = upper_quantile_winsorization,
    lower_quantile_winsorization = lower_quantile_winsorization
  )

  expect_equal(expected_results, results$selected_signals_corrected_positions_m_d_ref)

})

test_that("set_final_signal works for ML model when eligible signals are less than total options", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Change Default
  signal_selection_policy$signal_blending_method <- "ML"
  signal_selection_policy$p_correction_method <- "none"
  signal_selection_policy$ml_parameters <- list(
    target_fwd = 1,
    target_fwd_name = "fwd_return_1m",
    training_sample_rate = 0.5,
    split_method = "expanding",
    ml_algorithm = "glmnet",
    chosen_eval_metric = "rmse",
    hyper_grid_domain_list = list(
      alpha = c(0,0.5, 1),
      lambda.min.ratio = c(0, 0.5, 0.9)
    ),
    tuning_method = "grid_search"
  )
  signal_selection_policy$signal_significance_threshold <- 0.85 #Hack eligibility


  #Current date
  current_date <- "2001-08-15"

  #Initial preparations
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  target_m_upd_ref <- target_m_df[which(target_m_df$dates <= current_date),]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]
  selected_benchmark_returns_upd_ref <- benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)]
  priors_m_upd_ref_list <- list(jkp_emerging = priors_m_df_list$jkp_emerging[which(priors_m_df_list$jkp_emerging$dates <= current_date), ])
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]

  #Select signals based on user choice
  signal_selection_policy$chosen_signals <- c("Alpha", "Beta", "Gamma")
  signal_selection_policy$signal_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  selected_signals_and_backtest_list <- select_and_correct_signals(signal_selection_policy = signal_selection_policy, signals_m_upd_ref = signals_m_upd_ref, backtest_returns_upd_ref = backtest_returns_upd_ref)
  selected_signals_backtest_returns_upd_ref <- selected_signals_and_backtest_list$selected_signals_backtest_returns_upd_ref
  selected_signals_corrected_positions_m_upd_ref <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_upd_ref
  selected_signals_corrected_positions_m_d_ref <- selected_signals_corrected_positions_m_upd_ref[which(selected_signals_corrected_positions_m_upd_ref$dates == current_date),]

  #Select priors
  selected_priors_informative_data_m_upd_ref <- priors_m_upd_ref_list[[signal_selection_policy$chosen_informative_data]]

  #Define signal eligibilith
  signal_eligibility_results <- define_signal_eligibility(
    selected_signals_backtest_returns_upd_ref = selected_signals_backtest_returns_upd_ref,
    selected_benchmark_returns_upd_ref = selected_benchmark_returns_upd_ref,
    signal_selection_policy = signal_selection_policy,
    signals_groups_m_d_ref = signals_groups_m_d_ref,
    selected_priors_informative_data_m_upd_ref = selected_priors_informative_data_m_upd_ref
  )
  eligible_signals <- signal_eligibility_results$signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::select(tickers)

  #Fit ML model
  target_m_df <- target_m_upd_ref
  features_m_df <- selected_signals_corrected_positions_m_upd_ref %>% dplyr::select(id, tickers, dates, low_Beta, Gamma)
  dates_m_vector <- unique(features_m_df$dates)[order(unique(features_m_df$dates))]
  #adjust order
  target_m_df <- target_m_df[order(target_m_df$dates),]
  features_m_df <- features_m_df[order(features_m_df$dates),]

  training_sample_size <- round(length(dates_m_vector)*signal_selection_policy$ml_parameters$training_sample_rate, 0)
  validation_sample_size <- length(dates_m_vector) - training_sample_size
  rebalancing_month <- 8

  #Fit model
  ml_fit <- suppressWarnings(ml_walk_forward_validation(
    features_m_df = features_m_df,
    target_m_df = target_m_df,
    dates_m_vector = dates_m_vector,
    training_sample_size = training_sample_size,
    target_fwd = signal_selection_policy$ml_parameters$target_fwd,
    target_fwd_name = signal_selection_policy$ml_parameters$target_fwd_name,
    validation_sample_size = validation_sample_size,
    tuning_method = signal_selection_policy$ml_parameters$tuning_method,
    hyper_grid_domain_list = signal_selection_policy$ml_parameters$hyper_grid_domain_list,
    ml_algorithm = signal_selection_policy$ml_parameters$ml_algorithm,
    rebalancing_month = 8
  ))

  #Get features new
  new_features_m_d_ref <- features_m_df[which(features_m_df$dates == current_date),]
  ml_predictions <- predict_ml_model(ml_walk_forward_validation_results = ml_fit, new_features_m_d_ref = new_features_m_d_ref)

  #Get expected_results
  expected_results <- list()
  expected_results$selected_signals_corrected_positions_m_d_ref <- selected_signals_corrected_positions_m_d_ref
  expected_results$selected_signals_corrected_positions_m_d_ref[, "final_signal"] <- ml_predictions %>%
    signal_transform(upper_quantile_winsorization = 0.975, lower_quantile_winsorization = 0.025)
  expected_results$ml_predictions <- ml_predictions
  expected_results$new_features_m_d_ref <- new_features_m_d_ref

  results <- set_final_signal(selected_signals_corrected_positions_m_d_ref = selected_signals_corrected_positions_m_d_ref,
                              eligible_signals = eligible_signals,
                              signal_weights = NULL,
                              ml_walk_forward_validation_results = ml_fit
  )

  expect_equal(results, expected_results)


})

