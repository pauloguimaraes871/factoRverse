test_that("calculate trade order works for a first rebalancing scenario with delistings and IPOs ", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0, 0, 0, 0, 0)

  #Exclude a stock
  updated_port_weights_m_lstd_ref <- updated_port_weights_m_lstd_ref %>% dplyr::filter(!tickers == "Stock A")

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

  #Set Portfolio Weights
  sw_port <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "sw")

  #merge_and_rescale
  merged_port_results_list <- merge_and_rescale_weights(port_weights_m_d_ref = port_weights_m_d_ref,
                                                        updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                        stock_universe_m_d_ref = sw_port@universe_m_d_ref@data)

  #Result
  results <- calculate_trade_orders(merged_port_results = merged_port_results_list,
                                    liquidity_m_d_ref = liquidity_m_d_ref,
                                    volatility_m_d_ref = volatility_m_d_ref,
                                    strategy_aum = 100,
                                    main_liquidity_metric = "mean_volfin_3m"
  )


  #Check that delisted and new stocks are present
  expect_true(all(c(port_weights_m_d_ref$tickers, updated_port_weights_m_lstd_ref$tickers) %in% results$tickers))
  expect_true(all(merged_port_results_list$delisted_tickers_old_universe %in% results$tickers))
  expect_true(all(merged_port_results_list$ipo_tickers %in% results$tickers))
  expect_equal(nrow(results), length(unique(c(port_weights_m_d_ref$tickers, updated_port_weights_m_lstd_ref$tickers))))

  #Check that weights sum to 1 and are all 0
  expect_equal(results$eop_port_weights %>% sum(), 1)
  expect_equal(results$bop_port_weights %>% sum(), 0)

  #Check that weights correspond to expectations
  expect_equal(results %>% dplyr::filter(!tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::select(eop_port_weights) %>% sum(), 1)
  current_tickers <- port_weights_m_d_ref %>% dplyr::pull(tickers)
  #Rebalanced  weights match sw object
  expect_equal(results %>% dplyr::filter(tickers %in% current_tickers) %>% dplyr::pull(eop_port_weights), sw_port@universe_m_d_ref@data %>% dplyr::pull(weights))
  #Delisted tickers have a weight of zero
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(eop_port_weights) %>% unique(), 0)
  #bop is 0 for everybody
  expect_equal(results %>% dplyr::pull(bop_port_weights) %>% unique(), 0)
  #mean_volfin_3m is q25 for delisted tickers
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(mean_volfin_3m) %>% unique(),
               liquidity_m_d_ref$mean_volfin_3m %>% quantile(0.25) %>% unname())
  #daily_vol is q50 for delisted tickers
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(daily_vol) %>% unique(),
               volatility_m_d_ref$daily_vol %>% quantile(0.5) %>% unname())
  #Observations are right
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(obs) %>% unique(),
               "delisted")
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$ipo_tickers) %>% dplyr::pull(obs) %>% unique(),
               "IPO")

  #delta is as expected
  expect_equal(results %>% dplyr::pull(delta) %>% sum(), 1)
  #order is as expected
  expect_equal(results %>% dplyr::pull(order) %>% sum(), 100)

  #Check that NA fills are right
  results[5, "id"] = c("Stock B-2001-05-15")
  results[5, "dates"] = c("2001-05-15") %>% as.Date()
  results[5, "presence"] = 0


})


test_that("calculate trade order works for non-rebalancing scenario with delistings and IPOs ", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.25, 0.15, 0.35, 0, 0.50)

  #Exclude a stock
  updated_port_weights_m_lstd_ref <- updated_port_weights_m_lstd_ref %>% dplyr::filter(!tickers == "Stock A")

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

  #merge_and_rescale
  merged_port_results_list <- merge_and_rescale_weights(port_weights_m_d_ref = port_weights_m_d_ref,
                                                        updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                        stock_universe_m_d_ref = NULL)

  #Result
  results <- calculate_trade_orders(merged_port_results = merged_port_results_list,
                                    liquidity_m_d_ref = liquidity_m_d_ref,
                                    volatility_m_d_ref = volatility_m_d_ref,
                                    strategy_aum = 100,
                                    main_liquidity_metric = "mean_volfin_3m"
  )


  #Check that delisted and new stocks are present
  expect_true(all(c(port_weights_m_d_ref$tickers, updated_port_weights_m_lstd_ref$tickers) %in% results$tickers))
  expect_true(all(merged_port_results_list$delisted_tickers_old_universe %in% results$tickers))
  expect_true(all(merged_port_results_list$ipo_tickers %in% results$tickers))
  expect_equal(nrow(results), length(unique(c(port_weights_m_d_ref$tickers, updated_port_weights_m_lstd_ref$tickers))))

  #Check that weights sum to 1 and are all 0
  expect_equal(results$eop_port_weights %>% sum(), 1)
  expect_equal(results$bop_port_weights %>% sum(), 1)

  #Check that weights correspond to expectations
  expect_equal(results %>% dplyr::filter(!tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::select(eop_port_weights) %>% sum(), 1)
  current_tickers <- port_weights_m_d_ref %>% dplyr::pull(tickers)
  #Rebalanced weights match rescaled weights
  expect_equal(results %>% dplyr::filter(tickers %in% current_tickers) %>% dplyr::pull(eop_port_weights), merged_port_results_list$port_weights_m_d_ref$eop_port_weights)
  #Delisted tickers have a weight of zero
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(eop_port_weights) %>% unique(), 0)
  #ipo tickers have a weight of zero
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$ipo_tickers) %>% dplyr::pull(eop_port_weights) %>% unique(), 0)
  #mean_volfin_3m is q25 for delisted tickers
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(mean_volfin_3m) %>% unique(),
               liquidity_m_d_ref$mean_volfin_3m %>% quantile(0.25) %>% unname())
  #daily_vol is q50 for delisted tickers
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(daily_vol) %>% unique(),
               volatility_m_d_ref$daily_vol %>% quantile(0.5) %>% unname())
  #Observations are right
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(obs) %>% unique(),
               "delisted")
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$ipo_tickers) %>% dplyr::pull(obs) %>% unique(),
               "IPO")
  #order is as expected
  expect_equal(results %>% dplyr::pull(order) %>% sum(), (results %>% dplyr::pull(delta) * 100) %>% sum())

  #Check that NA fills are right
  results[5, "id"] = c("Stock B-2001-05-15")
  results[5, "dates"] = c("2001-05-15") %>% as.Date()
  results[5, "presence"] = 0


})


test_that("calculate trade order works for a first rebalancing scenario with delistings -toypreprocessed ", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2023-09-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2023-08-15"), c(1:3)]
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
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  #cw_port
  cw_port <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "cw",
                                   liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = "mean_volfin_3m")

  #merge_and_rescale
  merged_port_results_list <- merge_and_rescale_weights(port_weights_m_d_ref = port_weights_m_d_ref,
                                                           updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                           stock_universe_m_d_ref = cw_port@universe_m_d_ref@data)

  #Result
  results <- calculate_trade_orders(merged_port_results = merged_port_results_list,
                                    liquidity_m_d_ref = liquidity_m_d_ref,
                                    volatility_m_d_ref = volatility_m_d_ref,
                                    strategy_aum = 100,
                                    main_liquidity_metric = "mean_volfin_3m"
  )


  #Check that delisted and new stocks are present
  expect_true(all(c(port_weights_m_d_ref$tickers, updated_port_weights_m_lstd_ref$tickers) %in% results$tickers))
  expect_true(all(merged_port_results_list$delisted_tickers_old_universe %in% results$tickers))
  expect_true(all(merged_port_results_list$ipo_tickers %in% results$tickers))
  expect_equal(nrow(results), length(unique(c(port_weights_m_d_ref$tickers, updated_port_weights_m_lstd_ref$tickers))))

  #Check that weights sum to 1 and are all 0
  expect_equal(results$eop_port_weights %>% sum(), 1)
  expect_equal(results$bop_port_weights %>% sum(), 0)

  #Check that weights correspond to expectations
  expect_equal(results %>% dplyr::filter(!tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::select(eop_port_weights) %>% sum(), 1)
  current_tickers <- port_weights_m_d_ref %>% dplyr::pull(tickers)
  #Rebalanced  weights match cw object
  expect_equal(results %>% dplyr::filter(tickers %in% current_tickers) %>% dplyr::pull(eop_port_weights), cw_port@universe_m_d_ref@data %>% dplyr::pull(weights))
  #Delisted tickers have a weight of zero
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(eop_port_weights) %>% unique(), 0)
  #bop is 0 for everybody
  expect_equal(results %>% dplyr::pull(bop_port_weights) %>% unique(), 0)
  #mean_volfin_3m is q25 for delisted tickers
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(mean_volfin_3m) %>% unique(),
               liquidity_m_d_ref$mean_volfin_3m %>% quantile(0.25) %>% unname())
  #daily_vol is q50 for delisted tickers
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(daily_vol) %>% unique(),
               volatility_m_d_ref$daily_vol %>% quantile(0.5) %>% unname())
  #Observations are right
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(obs) %>% unique(),
               "delisted")

  #Check that NA fills are right
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(dates) %>% unique(),
               "2023-08-15" %>% as.Date())
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(vol_12m) %>% unique(),
               0)
  expect_equal(results %>% dplyr::filter(tickers %in% merged_port_results_list$delisted_tickers_old_universe) %>% dplyr::pull(presence) %>% unique(),
               0)



  #delta is as expected
  expect_equal(results %>% dplyr::pull(delta) %>% sum(), 1)
  #order is as expected
  expect_equal(results %>% dplyr::pull(order) %>% sum(), 100)

})
