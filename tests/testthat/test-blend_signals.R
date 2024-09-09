test_that("blend signal works for a single signal", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Current date
  current_date <- "2001-06-15"

  #Initial preparations
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  target_m_upd_ref <- target_m_df[which(target_m_df$dates <= current_date),]
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]

  #Select signals based on user choice
  signal_selection_policy$chosen_signals <- c("Beta")
  signal_selection_policy$signal_positions <- c(Beta = "short")

  selected_signals_and_backtest_list <- select_and_correct_signals(signal_selection_policy = signal_selection_policy, signals_m_upd_ref = signals_m_upd_ref)
  selected_signals_corrected_positions_m_upd_ref <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_upd_ref
  selected_signals_corrected_positions_m_d_ref <- selected_signals_corrected_positions_m_upd_ref[which(selected_signals_corrected_positions_m_upd_ref$dates == current_date),]

  #Create signal universe
  signal_universe_m_d_ref <- data.frame(tickers = "low_Beta", is_eligible = 1)

  #Set weights
  signal_universe_m_d_ref <- set_portfolio_weights(
    universe_m_d_ref = signal_universe_m_d_ref,
    portfolio_construction_method = signal_selection_policy$signal_blending_method
  )

  #Set final signal
  stock_universe_m_d_ref <- set_final_signal(
    selected_signals_corrected_positions_m_d_ref = selected_signals_corrected_positions_m_d_ref,
    eligible_signals = NULL, signal_weights = signal_universe_m_d_ref$weights, ml_walk_forward_validation_results = NULL
  )$selected_signals_corrected_positions_m_d_ref

  #Expect
  expected_results <- list(
    signal_universe_m_d_ref = signal_universe_m_d_ref,
    stock_universe_m_d_ref = stock_universe_m_d_ref,
    eligible_signals = data.frame(tickers = signal_universe_m_d_ref$tickers),
    signal_weights = signal_universe_m_d_ref$weights
  )

  #Results
  results <- blend_signals(current_date = current_date,
                           signals_m_df = signals_m_df,
                           signal_selection_policy = signal_selection_policy
  )

  expect_equal(results, expected_results)


})

test_that("blend signal works for EW", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Current date
  current_date <- "2001-06-15"

  #Initial preparations
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  target_m_upd_ref <- target_m_df[which(target_m_df$dates <= current_date),]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]
  selected_benchmark_returns_upd_ref <- benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)]
  priors_m_upd_ref_list <- list(jkp_emerging = priors_m_df_list$jkp_emerging[which(priors_m_df_list$jkp_emerging$dates <= current_date), ])
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]

  #Select signals based on user choice
  signal_selection_policy$chosen_signals <- c("Alpha", "Beta")
  signal_selection_policy$signal_positions <- c(Alpha = "long", Beta = "short")

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

  #Set weights
  signal_universe_m_d_ref <- set_portfolio_weights(
    universe_m_d_ref = signal_eligibility_results$signal_universe_m_d_ref,
    portfolio_construction_method = signal_selection_policy$signal_blending_method,
    groups_m_d_ref = signals_groups_m_d_ref
  )

  #Set final signal
  stock_universe_m_d_ref <- set_final_signal(
    selected_signals_corrected_positions_m_d_ref = selected_signals_corrected_positions_m_d_ref,
    eligible_signals = NULL, signal_weights = signal_universe_m_d_ref$weights, ml_walk_forward_validation_results = NULL
  )$selected_signals_corrected_positions_m_d_ref

  #Expect
  expected_results <- list(
    signal_universe_m_d_ref = signal_universe_m_d_ref,
    stock_universe_m_d_ref = stock_universe_m_d_ref,
    eligible_signals = data.frame(tickers = signal_universe_m_d_ref$tickers),
    signal_weights = signal_universe_m_d_ref$weights
  )

  #Results
  results <- blend_signals(current_date = current_date,
                           signals_m_df = signals_m_df,
                           target_m_df = target_m_df,
                           signal_selection_policy = signal_selection_policy,
                           backtest_returns_df = backtest_returns_df,
                           selected_benchmark_returns_df = benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)],
                           signals_groups_m_d_ref = signals_groups_m_d_ref
  )

  expect_equal(results, expected_results)


})

test_that("blend signal works for SW (IR)", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Change Default
  signal_selection_policy$signal_blending_method <- "SW"
  signal_selection_policy$chosen_sb_metric <- "IR"

  #Current date
  current_date <- "2001-06-15"

  #Initial preparations
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  target_m_upd_ref <- target_m_df[which(target_m_df$dates <= current_date),]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]
  selected_benchmark_returns_upd_ref <- benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)]
  priors_m_upd_ref_list <- list(jkp_emerging = priors_m_df_list$jkp_emerging[which(priors_m_df_list$jkp_emerging$dates <= current_date), ])
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]

  #Select signals based on user choice
  signal_selection_policy$chosen_signals <- c("Alpha", "Beta")
  signal_selection_policy$signal_positions <- c(Alpha = "long", Beta = "short")

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

  #Set weights
  signal_universe_m_d_ref <- set_portfolio_weights(
    universe_m_d_ref = signal_eligibility_results$signal_universe_m_d_ref,
    portfolio_construction_method = signal_selection_policy$signal_blending_method
  )

  #Set final signal
  stock_universe_m_d_ref <- set_final_signal(
    selected_signals_corrected_positions_m_d_ref = selected_signals_corrected_positions_m_d_ref,
    eligible_signals = NULL, signal_weights = signal_universe_m_d_ref$weights, ml_walk_forward_validation_results = NULL
  )$selected_signals_corrected_positions_m_d_ref

  #Expect
  expected_results <- list(
    signal_universe_m_d_ref = signal_universe_m_d_ref,
    stock_universe_m_d_ref = stock_universe_m_d_ref,
    eligible_signals = data.frame(tickers = signal_universe_m_d_ref$tickers),
    signal_weights = signal_universe_m_d_ref$weights
  )

  #Results
  results <- blend_signals(current_date = current_date,
                           signals_m_df = signals_m_df,
                           signal_selection_policy = signal_selection_policy,
                           backtest_returns_df = backtest_returns_df,
                           selected_benchmark_returns_df = benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)],
                           signals_groups_m_d_ref = signals_groups_m_d_ref
  )

  expect_equal(results, expected_results)


})

test_that("blend signal works for RP", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Change Default
  signal_selection_policy$signal_blending_method <- "RP"
  covariance_estimation_method <- "PCA2"
  signal_selection_policy$p_correction_method <- "BY"

  #Current date
  current_date <- "2001-06-15"

  #Initial preparations
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  target_m_upd_ref <- target_m_df[which(target_m_df$dates <= current_date),]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]
  selected_benchmark_returns_upd_ref <- benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)]
  priors_m_upd_ref_list <- list(jkp_emerging = priors_m_df_list$jkp_emerging[which(priors_m_df_list$jkp_emerging$dates <= current_date), ])
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]

  #Select signals based on user choice
  signal_selection_policy$chosen_signals <- c("Alpha", "Beta")
  signal_selection_policy$signal_positions <- c(Alpha = "long", Beta = "short")

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

  #Set weights
  signal_universe_m_d_ref <- set_portfolio_weights(
    universe_m_d_ref = signal_eligibility_results$signal_universe_m_d_ref,
    portfolio_construction_method = signal_selection_policy$signal_blending_method,
    covariance_estimation_method = covariance_estimation_method,
    returns_upd_ref = selected_signals_backtest_returns_upd_ref
  )

  #Set final signal
  stock_universe_m_d_ref <- set_final_signal(
    selected_signals_corrected_positions_m_d_ref = selected_signals_corrected_positions_m_d_ref,
    eligible_signals = NULL, signal_weights = signal_universe_m_d_ref$weights, ml_walk_forward_validation_results = NULL
  )$selected_signals_corrected_positions_m_d_ref

  #Expect
  expected_results <- list(
    signal_universe_m_d_ref = signal_universe_m_d_ref,
    stock_universe_m_d_ref = stock_universe_m_d_ref,
    eligible_signals = data.frame(tickers = signal_universe_m_d_ref$tickers),
    signal_weights = signal_universe_m_d_ref$weights
  )

  #Results
  results <- blend_signals(current_date = current_date,
                           signals_m_df = signals_m_df,
                           signal_selection_policy = signal_selection_policy,
                           backtest_returns_df = backtest_returns_df,
                           covariance_estimation_method = covariance_estimation_method,
                           selected_benchmark_returns_df = benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)],
                           signals_groups_m_d_ref = signals_groups_m_d_ref
  )

  expect_equal(results, expected_results)


})

test_that("blend signal works for MTO", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Change Default
  signal_selection_policy$signal_blending_method <- "MTO"
  covariance_estimation_method <- "PCA1"
  signal_selection_policy$p_correction_method <- "BH"

  #Current date
  current_date <- "2001-06-15"

  #Initial preparations
  signals_m_upd_ref <- signals_m_df[which(signals_m_df$dates <= current_date), ]
  target_m_upd_ref <- target_m_df[which(target_m_df$dates <= current_date),]
  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]
  selected_benchmark_returns_upd_ref <- benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)]
  priors_m_upd_ref_list <- list(jkp_emerging = priors_m_df_list$jkp_emerging[which(priors_m_df_list$jkp_emerging$dates <= current_date), ])
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]

  #Select signals based on user choice
  signal_selection_policy$chosen_signals <- c("Alpha", "Beta")
  signal_selection_policy$signal_positions <- c(Alpha = "long", Beta = "short")

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

  #Set weights
  signal_universe_m_d_ref <- set_portfolio_weights(
    universe_m_d_ref = signal_eligibility_results$signal_universe_m_d_ref,
    portfolio_construction_method = signal_selection_policy$signal_blending_method,
    concentration_constraint_policy = list(
      benchmark = signal_selection_policy$sb_benchmark_weighting,
      max_abs_active_individual_weight = signal_selection_policy$max_abs_active_individual_weight,
      max_abs_active_group_weight = signal_selection_policy$max_abs_active_group_weight
    ),
    covariance_estimation_method = covariance_estimation_method,
    returns_upd_ref = selected_signals_backtest_returns_upd_ref,
    groups_m_d_ref = signals_groups_m_d_ref
  )

  #Set final signal
  stock_universe_m_d_ref <- set_final_signal(
    selected_signals_corrected_positions_m_d_ref = selected_signals_corrected_positions_m_d_ref,
    eligible_signals = NULL, signal_weights = signal_universe_m_d_ref$weights, ml_walk_forward_validation_results = NULL
  )$selected_signals_corrected_positions_m_d_ref

  #Expect
  expected_results <- list(
    signal_universe_m_d_ref = signal_universe_m_d_ref,
    stock_universe_m_d_ref = stock_universe_m_d_ref,
    eligible_signals = data.frame(tickers = signal_universe_m_d_ref$tickers),
    signal_weights = signal_universe_m_d_ref$weights
  )

  #Results
  results <- blend_signals(current_date = current_date,
                           signals_m_df = signals_m_df,
                           signal_selection_policy = signal_selection_policy,
                           backtest_returns_df = backtest_returns_df,
                           covariance_estimation_method = covariance_estimation_method,
                           selected_benchmark_returns_df = benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)],
                           signals_groups_m_d_ref = signals_groups_m_d_ref
  )

  expect_equal(results, expected_results)

  #Test also that it works under no rebalancing
  results_no_rebalancing <- blend_signals(
    current_date = current_date,
    eligible_signals = results$eligible_signals,
    signal_weights = results$signal_universe_m_d_ref$weights,
    ml_walk_forward_validation_results = NULL,
    signals_m_df = signals_m_df,
    signal_selection_policy = signal_selection_policy
  )



  expect_equal(results_no_rebalancing$stock_universe_m_d_ref, results$stock_universe_m_d_ref)


})

test_that("blend signal works for ML", {

  #Create signals_m_d_ref_test
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
  signal_selection_policy$signal_significance_threshold <- 1 #Hack eligibility


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

  #Fit ML model
  target_m_df <- target_m_upd_ref
  features_m_df <- selected_signals_corrected_positions_m_upd_ref
  dates_m_vector <- unique(features_m_df$dates)[order(unique(features_m_df$dates))]
  #adjust order
  target_m_df <- target_m_df[order(target_m_df$dates),]
  features_m_df <- features_m_df[order(features_m_df$dates),]

  training_sample_size <- round(length(dates_m_vector)*signal_selection_policy$ml_parameters$training_sample_rate, 0)
  validation_sample_size <- length(dates_m_vector) - training_sample_size
  rebalancing_month <- 8

  ml_fit <- suppressWarnings(
    ml_walk_forward_validation(
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
    )
  )

  #Set final signal
  signal_universe_m_d_ref <- signal_eligibility_results$signal_universe_m_d_ref
  eligible_signals <- signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::select(tickers)
  final_signal_results_list <- set_final_signal(
    selected_signals_corrected_positions_m_d_ref = selected_signals_corrected_positions_m_d_ref,
    eligible_signals = eligible_signals, signal_weights = NULL, ml_walk_forward_validation_results = ml_fit
  )
  stock_universe_m_d_ref <- final_signal_results_list$selected_signals_corrected_positions_m_d_ref

  #Expect
  expected_results <- list(
    signal_universe_m_d_ref = signal_universe_m_d_ref,
    stock_universe_m_d_ref = stock_universe_m_d_ref,
    eligible_signals = data.frame(tickers = signal_universe_m_d_ref$tickers),
    ml_walk_forward_validation_results = ml_fit,
    ml_predictions = final_signal_results_list$ml_predictions,
    new_features_m_d_ref = final_signal_results_list$new_features_m_d_ref
  )

  #Results
  results <- blend_signals(current_date = current_date,
                           signals_m_df = signals_m_df,
                           target_m_df = target_m_df,
                           signal_selection_policy = signal_selection_policy,
                           backtest_returns_df = backtest_returns_df,
                           selected_benchmark_returns_df = benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", concentration_constraint_policy$benchmark)],
                           signals_groups_m_d_ref = signals_groups_m_d_ref
  )


  #Test also that it works under no rebalancing
  results_no_rebalancing <- blend_signals(
    current_date = current_date,
    target_m_df = NULL,
    eligible_signals = results$eligible_signals,
    signal_weights = NULL,
    ml_walk_forward_validation_results = results$ml_walk_forward_validation_results,
    signals_m_df = signals_m_df,
    signal_selection_policy = signal_selection_policy
  )

  #First compare no rebalancing to preserve metadata
  expect_equal(results_no_rebalancing$stock_universe_m_d_ref, results$stock_universe_m_d_ref)

  #Erase plot and metadata and compare
  results$ml_walk_forward_validation_results$plots <- NULL
  results$ml_walk_forward_validation_results$metadata <- NULL
  expected_results$ml_walk_forward_validation_results$plots <- NULL
  expected_results$ml_walk_forward_validation_results$metadata <- NULL
  expect_equal(results, expected_results)


})

