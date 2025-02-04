test_that("generate_box_constraints works for signals", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  current_date <- "2001-06-15"

  #Subset
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  backtest_returns_m_xts_upd_ref <- backtest_returns_m_xts[which(zoo::index(backtest_returns_m_xts) <= current_date), ]
  selected_benchmark_returns_m_xts_upd_ref <- benchmark_returns_m_xts[which(zoo::index(benchmark_returns_m_xts) <= current_date), concentration_constraint_policy$benchmark]
  signal_themes_m_d_ref <- signal_themes_m_df %>% dplyr::filter(dates == current_date)

  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(signals_m_df = signals_m_d_ref, chosen_signals_and_positions = c(Alpha = "long", Beta = "short", Gamma = "long"),
                                                                   signal_themes_m_df = signal_themes_m_d_ref, backtest_returns_m_xts = backtest_returns_m_xts_upd_ref)

  selected_signals_backtest_returns_m_xts_upd_ref <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts

  #Define signal eligibility
  #adjust backtest to include two assets in sb benchmark
  selected_signals_backtest_returns_m_xts_upd_ref$low_Beta <- selected_signals_backtest_returns_m_xts_upd_ref$low_Beta + 5

  suppressWarnings(
  signal_eligibility_results_list <- define_signal_eligibility(
    selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_signals_backtest_returns_m_xts_upd_ref,
    selected_market_factor_proxy_m_xts_upd_ref = selected_benchmark_returns_m_xts_upd_ref,
    selected_signal_themes_m_d_ref = signal_themes_m_d_ref
  )
  )

  #MVO Portfolio for eligible
  concentration_constraint_policy_signal <- list(
    benchmark = "theme_sb",
    max_abs_active_individual_weight = 0.2,
    max_abs_active_group_weight = 0.1
  )

  expected_results <- signal_eligibility_results_list$signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)

  expected_results$max_weight <- expected_results$theme_sb_bench_weights + concentration_constraint_policy_signal$max_abs_active_individual_weight

  expected_results$min_weight <- expected_results$theme_sb_bench_weights - concentration_constraint_policy_signal$max_abs_active_individual_weight

  #get results
  signal_universe_m_d_ref <- signal_eligibility_results_list$signal_universe_m_d_ref

  results <- generate_box_constraints(universe_m_d_ref = signal_universe_m_d_ref,
                                      concentration_constraint_policy = concentration_constraint_policy_signal)

  #expect
  expect_equal(results, expected_results)

})

test_that("generate_box_constraints works for stocks", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Change Default
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df[which(benchmark_weights_m_df$dates == current_date),]
  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date),]
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.20, 0.20, 0.20, 0.20, 0.20)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(Gamma = "long"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Generate box constraints
  expected_results <- stock_universe_m_d_ref
  #Benchmark-relative simple constraints
  expected_results$max_weight <- expected_results$IBOV_bench_weights + concentration_constraint_policy$max_abs_active_individual_weight
  expected_results$min_weight <- expected_results$IBOV_bench_weights - concentration_constraint_policy$max_abs_active_individual_weight

  #Adjust Liq Caps
  liquidity_max <- expected_results$IBOV_bench_weights[2] + liquidity_constraint_policy$liquidity_cap_rules[1]

  #Adjust turnover cap
  turnover_max <- expected_results$bop_port_weights[2] + turnover_constraint_policy$turnover_cap_rules[1]
  turnover_min <- expected_results$bop_port_weights[2] - turnover_constraint_policy$turnover_cap_rules[1]

  #Adjust bench-weight relative
  expected_results$max_weight[2] <- liquidity_max
  expected_results$min_weight[2] <- expected_results$IBOV_bench_weights[2]

  results <- generate_box_constraints(universe_m_d_ref = stock_universe_m_d_ref, liquidity_constraint_policy = liquidity_constraint_policy,
                                      turnover_constraint_policy = turnover_constraint_policy, concentration_constraint_policy = concentration_constraint_policy)

  expect_equal(results, expected_results)


})

test_that("generate_box_constraints works for stocks when max and min are the same", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Change Default
  eligibility_quantile_range <- c(0.67, 1)
  liquidity_constraint_policy$liquidity_cap_rules[1] <- 0 #Hack to make both equal

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df[which(benchmark_weights_m_df$dates == current_date),]
  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date),]
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.20, 0.20, 0.20, 0.20, 0.20)

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(Gamma = "long"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Generate box constraints
  expected_results <- stock_universe_m_d_ref

  #Benchmark-relative simple constraints
  expected_results$max_weight <- expected_results$IBOV_bench_weights + concentration_constraint_policy$max_abs_active_individual_weight
  expected_results$min_weight <- expected_results$IBOV_bench_weights - concentration_constraint_policy$max_abs_active_individual_weight

  #Adjust Liq Caps
  liquidity_max <- expected_results$IBOV_bench_weights[2] + liquidity_constraint_policy$liquidity_cap_rules[1]

  #Adjust turnover cap
  turnover_max <- expected_results$bop_port_weights[2] + turnover_constraint_policy$turnover_cap_rules[1]
  turnover_min <- expected_results$bop_port_weights[2] - turnover_constraint_policy$turnover_cap_rules[1]

  #Adjust bench-weight relative
  expected_results$max_weight[2] <- liquidity_max
  expected_results$min_weight[2] <- expected_results$IBOV_bench_weights[2]

  #Create wiggle room
  expected_results$max_weight[2] <- expected_results$max_weight[2] + 0.002
  expected_results$min_weight[2] <- expected_results$min_weight[2] - 0.002


  results <- generate_box_constraints(universe_m_d_ref = stock_universe_m_d_ref, liquidity_constraint_policy = liquidity_constraint_policy,
                                      turnover_constraint_policy = turnover_constraint_policy, concentration_constraint_policy = concentration_constraint_policy)

  expect_equal(results, expected_results)


})
