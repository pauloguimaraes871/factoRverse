test_that("generate_group_constraints works for signals", {

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
  #adjust backtest to include two assets in sb benchmark
  selected_signals_backtest_returns_upd_ref$low_Beta <- selected_signals_backtest_returns_upd_ref$low_Beta + 5

  signal_eligibility_results_list <- define_signal_eligibility(
    selected_signals_backtest_returns_upd_ref = selected_signals_backtest_returns_upd_ref,
    selected_benchmark_returns_upd_ref = selected_benchmark_returns_upd_ref,
    signal_selection_policy = signal_selection_policy,
    signals_groups_m_d_ref = signals_groups_m_d_ref
  )

  #MTO Portfolio for eligible
  concentration_constraint_policy_signal <- list(
    benchmark = signal_selection_policy$sb_benchmark_weighting,
    max_abs_active_individual_weight = signal_selection_policy$max_abs_active_individual_weight,
    max_abs_active_group_weight = signal_selection_policy$max_abs_active_group_weight
  )

  expected_results <- signal_eligibility_results_list$signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)
  expected_results <- list(
    eligible_assets_group_membership_list <- list(theme.momentum = c(2), theme.value = c(1)),
    group_constraint_max = c(0.7, 0.7),
    group_constraint_min = c(0.3, 0.3)
  )
  names(expected_results)[1] <- "eligible_assets_group_membership_list"


  #get results
  signal_universe_m_d_ref <- signal_eligibility_results_list$signal_universe_m_d_ref
  results <- generate_group_constraints(universe_m_d_ref = signal_universe_m_d_ref,
                                      concentration_constraint_policy = concentration_constraint_policy_signal,
                                      groups_m_d_ref = signals_groups_m_d_ref)

  #expect
  expect_equal(results, expected_results)

})

test_that("generate_group_constraints works for stocks (2 groups)", {
  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Change Default
  signal_selection_policy$signal_blending_method <- "MTO"
  covariance_estimation_method <- "PCA1"
  signal_selection_policy$p_correction_method <- "BH"
  top_assets_quantile <- 0.67

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  selected_benchmark_returns_df <- benchmark_returns_df[, c("dates", concentration_constraint_policy$benchmark)]
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]
  stocks_groups_m_d_ref <- groups_m_df_list$stocks[which(groups_m_df_list$stocks$dates == current_date),]
  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date),]
  benchmark_weights_m_d_ref <- benchmark_weights_m_df[which(benchmark_weights_m_df$dates == current_date),]
  portfolio_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  portfolio_weights_m_lstd_ref$old_portfolio_weights <- c(0.20, 0.20, 0.20, 0.20, 0.20)

  #Blend Signals
  signal_results_list <- blend_signals(current_date = current_date,
                                       signals_m_df = signals_m_df,
                                       target_m_df = target_m_df,
                                       signal_selection_policy = signal_selection_policy,
                                       backtest_returns_df = backtest_returns_df,
                                       covariance_estimation_method = covariance_estimation_method,
                                       selected_benchmark_returns_df = selected_benchmark_returns_df,
                                       priors_m_df_list = priors_m_df_list,
                                       signals_groups_m_d_ref = signals_groups_m_d_ref
  )

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    signals_m_d_ref = signal_results_list$stock_universe_m_d_ref,
    top_assets_quantile = top_assets_quantile,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stocks_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Generate box constraints
  eligible_universe_m_d_ref <- generate_box_constraints(universe_m_d_ref = stock_universe_m_d_ref,
                                                        liquidity_constraint_policy = liquidity_constraint_policy,
                                                        turnover_constraint_policy = turnover_constraint_policy,
                                                        concentration_constraint_policy = concentration_constraint_policy
                                                        )

  eligible_universe_m_d_ref %>% dplyr::group_by(Subsector) %>% dplyr::summarize(sector_sum = sum(IBOV_bench_weights))
  #Generate group constraints
  eligible_assets_group_membership_list <- list(
    Sector.Cyclical = c(3,4), Sector.Financials = c(2), Sector.Oil = c(1),
    Subsector.Education = c(4), Subsector.Insurance = c(2), Subsector.Oil = c(1),  Subsector.Retail = c(3)
  )

  group_constraint_max = c(0.535, 0.259, 0.507, 0.774, 0.659, 0.907, 0.661)
  group_constraint_min = c(0.335, 0.059, 0.307, 0, 0, 0, 0)

  expected_results <- list(
    eligible_assets_group_membership_list = eligible_assets_group_membership_list,
    group_constraint_max = group_constraint_max,
    group_constraint_min = group_constraint_min
  )

  results <- generate_group_constraints(universe_m_d_ref = stock_universe_m_d_ref,
                             concentration_constraint_policy = concentration_constraint_policy,
                             groups_m_d_ref = stocks_groups_m_d_ref)

  expect_equal(results, expected_results, tolerance = 1e-2)

})

