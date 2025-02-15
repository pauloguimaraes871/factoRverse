test_that("calculate transaction costs works for constant lambda at new rebalancing", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  selected_benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date) %>% dplyr::select(-smll)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- 0

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
    benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Set Portfolio Weights
  sw_port <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "sw")

  #merge_and_rescale
  merged_port_results_list <- merge_and_rescale_weights(port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
                                                        updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                        stock_universe_m_d_ref = sw_port@universe_m_d_ref@data,
                                                        selected_benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref
                                                        )

  #Get transactions
  transactions_m_d_ref <- calculate_trade_orders(merged_port_results = merged_port_results_list,
                                                 updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                 liquidity_m_d_ref = liquidity_m_d_ref,
                                                 volatility_m_d_ref = volatility_m_d_ref,
                                                 strategy_aum = 1,
                                                 main_liquidity_metric = "mean_volfin_3m"
  )

  #Get transaction costs
  results <- calculate_transaction_costs(
    transactions_m_d_ref = transactions_m_d_ref,
    alpha = 0.5, lambda = .5,
    direct_transaction_cost = 0.07,
    strategy_aum = 1,
    verbose = FALSE
  )

  #Compare with hand-calculated (transaction_cost_calc)
  expect_equal(results$port_costs_d_ref$direct_cost , 0.07)
  expect_equal(results$port_costs_d_ref$market_impact_cost, 0.2139, tolerance = 1e-1)
  expect_equal(results$transactions_and_costs_m_d_ref$direct_cost, c(0.038360417, 0.011398279, 0.008843025, 0.011398279, 0))
  expect_equal(results$transactions_and_costs_m_d_ref$market_impact_cost, c(0.1758, 0.0021, 0.0255, 0.0104, 0), tolerance = 1e-1)
  expect_equal(results$port_costs_d_ref$turnover, 0.5)

})


test_that("calculate transaction costs works for dynamic lambda and associated warning for too high costs at new rebalancing", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  selected_benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date) %>% dplyr::select(-smll)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- 0

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
    benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Set Portfolio Weights
  sw_port <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "sw")

  #merge_and_rescale
  merged_port_results_list <- merge_and_rescale_weights(port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
                                                        updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                        stock_universe_m_d_ref = sw_port@universe_m_d_ref@data,
                                                        selected_benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref
                                                        )

  #Get transactions
  transactions_m_d_ref <- calculate_trade_orders(merged_port_results = merged_port_results_list,
                                                 updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                 liquidity_m_d_ref = liquidity_m_d_ref,
                                                 volatility_m_d_ref = volatility_m_d_ref,
                                                 strategy_aum = 10,
                                                 main_liquidity_metric = "mean_volfin_3m"
  )

  #Get transaction costs
  expect_warning(
  results <- calculate_transaction_costs(
    transactions_m_d_ref = transactions_m_d_ref,
    alpha = 0.5, lambda = "dynamic",
    direct_transaction_cost = 0.07,
    strategy_aum = 10,
    verbose = FALSE
  ),
  "Total cost higher than 1.0%. Consider changing backtest parameters or implementing a stricter liquidity_floor_rule constraint."
  )

  #Compare with hand-calculated (transaction_cost_calc)
  expect_equal(results$port_costs_d_ref$direct_cost, 0.07)
  expect_equal(results$port_costs_d_ref$market_impact_cost , 1.4511, tolerance = 1e-02)
  expect_equal(results$transactions_and_costs_m_d_ref$direct_cost, c(0.038360417, 0.011398279, 0.008843025, 0.011398279, 0))
  expect_equal(results$transactions_and_costs_m_d_ref$market_impact_cost, c(1.2175, 0.01648, 0.151402, 0.065687, 0), tolerance = 1e-2)
  expect_equal(results$port_costs_d_ref$turnover, 0.5)


})

test_that("calculate transaction costs works for constant lambda at non-rebalancing", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  selected_benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date) %>% dplyr::select(-smll)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- 0.20

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
    benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    turnover_constraint_policy = turnover_constraint_policy
  )

  #Set Portfolio Weights
  sw_port <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "sw")

  #merge_and_rescale
  merged_port_results_list <- merge_and_rescale_weights(port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
                                                        updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                        stock_universe_m_d_ref = sw_port@universe_m_d_ref@data,
                                                        selected_benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref
  )

  #Get transactions
  transactions_m_d_ref <- calculate_trade_orders(merged_port_results = merged_port_results_list,
                                                 updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                 liquidity_m_d_ref = liquidity_m_d_ref,
                                                 volatility_m_d_ref = volatility_m_d_ref,
                                                 strategy_aum = 1,
                                                 main_liquidity_metric = "mean_volfin_3m"
  )

  #Get transaction costs
  results <- calculate_transaction_costs(
    transactions_m_d_ref = transactions_m_d_ref,
    alpha = 0.5, lambda = .5,
    direct_transaction_cost = 0.07,
    strategy_aum = 1,
    verbose = FALSE
  )

  #Compare with hand-calculated (transaction_cost_calc)
  expect_equal(results$port_costs_d_ref$direct_cost , 0.0487, tolerance = 1e-2)
  expect_equal(results$port_costs_d_ref$market_impact_cost, 0.1363, tolerance = 1e-02)
  expect_equal(results$transactions_and_costs_m_d_ref$direct_cost, c(0.02436, 0.002602, 0.005157, 0.002602, 0.014), tolerance = 1e-2)
  expect_equal(results$transactions_and_costs_m_d_ref$market_impact_cost, c(0.0888, 0.00023, 0.0113757, 0.001132941, 0.034532901), tolerance = 1e-2)
  expect_equal(results$port_costs_d_ref$turnover, 0.348006, tolerance = 1e-02)


})


test_that("calculate_transaction_costs works for toypreprocessed ", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  selected_benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2023-03-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- 0

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #cw_port
  cw_port <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "cw",
                                   liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = "mean_volfin_3m")

  #merge_and_rescale
  merged_port_results_list <- merge_and_rescale_weights(port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
                                                        updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                        stock_universe_m_d_ref = cw_port@universe_m_d_ref@data,
                                                        selected_benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
  )

  #Result
  transactions_m_d_ref <- calculate_trade_orders(merged_port_results_list = merged_port_results_list,
                                                 updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                 liquidity_m_d_ref = liquidity_m_d_ref,
                                                 volatility_m_d_ref = volatility_m_d_ref,
                                                 strategy_aum = 1,
                                                 main_liquidity_metric = "mean_volfin_3m")

  #Get transaction costs
  results <- calculate_transaction_costs(
    transactions_m_d_ref = transactions_m_d_ref,
    alpha = 1, lambda = "dynamic",
    direct_transaction_cost = 0.07,
    strategy_aum = 1,
    verbose = FALSE
  )

  #Expect costs to be higher for high rel order size
  expect_gt(
    results$transactions_and_costs_m_d_ref %>% dplyr::filter(relative_order_size > 0) %>%
      dplyr::filter(relative_order_size > median(relative_order_size)) %>%
      dplyr::pull(market_impact_cost) %>%
      mean(),
    results$transactions_and_costs_m_d_ref %>% dplyr::filter(relative_order_size > 0) %>%
      dplyr::filter(relative_order_size < median(relative_order_size)) %>%
      dplyr::pull(market_impact_cost) %>%
      mean()
  )

  #Expect costs to be higher for high daily_vol
  expect_gt(
    results$transactions_and_costs_m_d_ref %>% dplyr::filter(relative_order_size > 0) %>%
      dplyr::filter(daily_vol > median(daily_vol)) %>%
      dplyr::pull(market_impact_cost) %>%
      mean(),
    results$transactions_and_costs_m_d_ref %>% dplyr::filter(relative_order_size > 0) %>%
      dplyr::filter(daily_vol < median(daily_vol)) %>%
      dplyr::pull(market_impact_cost) %>%
      mean()
  )

  #Expect direct cost
  expect_equal(results$port_costs_d_ref$direct_cost, 0.07)




})
