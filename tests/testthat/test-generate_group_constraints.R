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

