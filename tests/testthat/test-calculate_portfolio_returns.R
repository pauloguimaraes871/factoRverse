test_that("calculate port returns works for rebalancing and no rebalancing, considering delisting and IPOs", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

 #Change Default
  signal_selection_policy$signal_blending_method <- "RP"
  covariance_estimation_method <- "PCA1"
  signal_selection_policy$p_correction_method <- "BH"
  top_assets_quantile <- 0.67
  transaction_costs_list$strategy_aum <- 1 #hack
  transaction_costs_list$alpha <- 0.5

  #Current date
  current_date <- as.Date("2001-06-15")

  #portfolio_returns_df
  dates_returns <- as.Date(c(dates_m_vector, "2001-09-15"))[-1]
  portfolio_returns_df <- data.frame(dates = dates_returns,
                                     raw_return = NA, raw_active_return = NA, net_return = NA, net_active_return = NA,
                                     direct_cost = NA, market_impact_cost = NA, total_cost = NA, turnover = NA,
                                     IBOV = NA
  )

  #Initial Preps
  selected_benchmark_returns_df <- benchmark_returns_df[, c("dates", concentration_constraint_policy$benchmark)]
  signals_groups_m_d_ref <- groups_m_df_list$signals[which(groups_m_df_list$signals$dates == current_date),]
  stocks_groups_m_d_ref <- groups_m_df_list$stocks[which(groups_m_df_list$stocks$dates == current_date),]
  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date),]
  volatility_m_d_ref <- volatility_m_df[which(volatility_m_df$dates == current_date),]
  target_m_d_ref <- target_m_df[which(target_m_df$dates == current_date),]
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

  #Set portfolio weights
  stock_universe_m_d_ref <- set_portfolio_weights(
    universe_m_d_ref = stock_universe_m_d_ref,
    portfolio_construction_method = "CS",
    groups_m_d_ref = stocks_groups_m_d_ref,
    cap_weighting_metric = "mean_volfin_3m",
    liquidity_m_d_ref = liquidity_m_d_ref
  )

  #Set transaction df
  portfolio_weights_m_d_ref <- stock_universe_m_d_ref %>% dplyr::select(id, tickers, dates, weights)
  colnames(portfolio_weights_m_d_ref)[4] <- "portfolio_weights"
  transactions_m_d_ref <- portfolio_weights_m_d_ref %>% dplyr::left_join(dplyr::select(target_m_d_ref, tickers, fwd_return_1m), by = "tickers") %>%
    dplyr::left_join(dplyr::select(liquidity_m_d_ref, -id, -dates), by = "tickers") %>%
    dplyr::left_join(dplyr::select(volatility_m_d_ref, -id, -dates), by = "tickers") %>%
    dplyr::full_join(dplyr::select(portfolio_weights_m_lstd_ref, tickers, old_portfolio_weights), by = "tickers")
  transactions_m_d_ref$portfolio_weights[5] <- 0
  transactions_m_d_ref$mean_volfin_3m[5] <- quantile(transactions_m_d_ref$mean_volfin_3m, 0.25, na.rm = TRUE)
  transactions_m_d_ref$daily_vol[5] <- quantile(transactions_m_d_ref$daily_vol, 0.5, na.rm = TRUE)

  #Estimate costs
  transactions_m_d_ref <- transactions_m_d_ref %>% dplyr::mutate(delta = portfolio_weights - old_portfolio_weights) %>%
    dplyr::mutate(order = delta * transaction_costs_list$strategy_aum) %>%
    dplyr::mutate(relative_order_size = abs(order)/mean_volfin_3m)

  transactions_m_d_ref$lambda <- 0.5  #dynamic lambda
  transactions_m_d_ref <- transactions_m_d_ref %>%
    dplyr::mutate(direct_cost = (transaction_costs_list$direct_transaction_cost * abs(order))/transaction_costs_list$strategy_aum) %>%
    dplyr::mutate(market_impact_cost = transaction_costs_list$alpha*(relative_order_size^transactions_m_d_ref$lambda)*daily_vol)

  costs <- sum(transactions_m_d_ref$direct_cost) + sum(transactions_m_d_ref$market_impact_cost)
  turnover <- mean(abs(transactions_m_d_ref$delta))

  #add obs
  transactions_m_d_ref$obs <- c(rep("none", 4), "delisted")

  #returns
  portfolio_returns_df[4,10] <- selected_benchmark_returns_df[5,2]
  portfolio_returns_df[4,2] <- sum(transactions_m_d_ref$portfolio_weights * transactions_m_d_ref$fwd_return_1m, na.rm = TRUE)
  portfolio_returns_df[4,3] <- portfolio_returns_df[4,2] - portfolio_returns_df[4,10]
  portfolio_returns_df[4,4] <- portfolio_returns_df[4,2] - costs
  portfolio_returns_df[4,5] <- portfolio_returns_df[4,3] - costs
  portfolio_returns_df[4,6] <- sum(transactions_m_d_ref$direct_cost)
  portfolio_returns_df[4,7] <- sum(transactions_m_d_ref$market_impact_cost)
  portfolio_returns_df[4,8] <- costs
  portfolio_returns_df[4,9] <- turnover


  #update port
  updated_portfolio_weights <- portfolio_weights_m_d_ref
  updated_portfolio_weights$portfolio_weights <- (portfolio_weights_m_d_ref$portfolio_weights * (1+target_m_d_ref$fwd_return_1m/100))
  updated_portfolio_weights$portfolio_weights <- updated_portfolio_weights$portfolio_weights/sum(updated_portfolio_weights$portfolio_weights)

  #expected (validated in excel)
  expected_results <- list(
    portfolio_weights_m_d_ref = portfolio_weights_m_d_ref,
    transactions_m_d_ref = transactions_m_d_ref,
    portfolio_returns_df = portfolio_returns_df,
    updated_portfolio_weights = updated_portfolio_weights
  )

  #results
  #reset
  portfolio_weights_m_d_ref$portfolio_weights <- 0
  portfolio_returns_df <- data.frame(dates = dates_returns,
                                     raw_return = NA, raw_active_return = NA, net_return = NA, net_active_return = NA,
                                     direct_cost = NA, market_impact_cost = NA, total_cost = NA, turnover = NA,
                                     IBOV = NA
  )

  results <- calculate_portfolio_returns(current_date = current_date,
                                         is_rebalancing_month = TRUE,
                                         stock_universe_m_d_ref = stock_universe_m_d_ref,
                                         target_m_d_ref = target_m_d_ref,
                                         portfolio_weights_m_d_ref = portfolio_weights_m_d_ref,
                                         portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref,
                                         portfolio_returns_df = portfolio_returns_df,
                                         selected_benchmark_returns_df = selected_benchmark_returns_df,
                                         liquidity_m_d_ref = liquidity_m_d_ref,
                                         volatility_m_d_ref = volatility_m_d_ref,
                                         transaction_costs_list = transaction_costs_list
                                         )

  expect_equal(results, expected_results)
  #check that sum of detlta close to 0
  expect_equal(sum(results$transactions_m_d_ref$delta), 0, tolerance = 1e-03)

  ###Next Period (no rebalancing)
  portfolio_weights_m_lstd_ref <- expected_results$updated_portfolio_weights
  colnames(portfolio_weights_m_lstd_ref)[4] <- "old_portfolio_weights"

  current_date <- as.Date("2001-07-15")
  #Get info
  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date),]
  volatility_m_d_ref <- volatility_m_df[which(volatility_m_df$dates == current_date),]
  target_m_d_ref <- target_m_df[which(target_m_df$dates == current_date),]
  benchmark_weights_m_d_ref <- benchmark_weights_m_df[which(benchmark_weights_m_df$dates == current_date),]

  #Get weights
  portfolio_weights_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), c("id", "tickers", "dates")]
  portfolio_weights_m_d_ref$portfolio_weights <- c(0, portfolio_weights_m_lstd_ref$old_portfolio_weights[-1])
  portfolio_weights_m_d_ref$portfolio_weights <-  portfolio_weights_m_d_ref$portfolio_weights/sum(portfolio_weights_m_d_ref$portfolio_weights) #rescale

  #Transaction costs df
  transactions_m_d_ref <- portfolio_weights_m_d_ref %>% dplyr::left_join(dplyr::select(target_m_d_ref, tickers, fwd_return_1m), by = "tickers") %>%
    dplyr::left_join(dplyr::select(liquidity_m_d_ref, -id, -dates), by = "tickers") %>%
    dplyr::left_join(dplyr::select(volatility_m_d_ref, -id, -dates), by = "tickers") %>%
    dplyr::full_join(dplyr::select(portfolio_weights_m_lstd_ref, tickers, old_portfolio_weights), by = "tickers")
  transactions_m_d_ref$portfolio_weights[5] <- 0
  transactions_m_d_ref$mean_volfin_3m[5] <- quantile(transactions_m_d_ref$mean_volfin_3m, 0.25, na.rm = TRUE)
  transactions_m_d_ref$daily_vol[5] <- quantile(transactions_m_d_ref$daily_vol, 0.5, na.rm = TRUE)
  transactions_m_d_ref$old_portfolio_weights[1] <- 0


  #Estimate costs
  transactions_m_d_ref <- transactions_m_d_ref %>% dplyr::mutate(delta = portfolio_weights - old_portfolio_weights) %>%
    dplyr::mutate(order = delta * transaction_costs_list$strategy_aum) %>%
    dplyr::mutate(relative_order_size = abs(order)/mean_volfin_3m)

  transactions_m_d_ref$lambda <- c(1,1,0.5,0.5,0.5)  #dynamic lambda
  transactions_m_d_ref <- transactions_m_d_ref %>%
    dplyr::mutate(direct_cost = (transaction_costs_list$direct_transaction_cost * abs(order))/transaction_costs_list$strategy_aum) %>%
    dplyr::mutate(market_impact_cost = transaction_costs_list$alpha*(relative_order_size^transactions_m_d_ref$lambda)*daily_vol)

  costs <- sum(transactions_m_d_ref$direct_cost) + sum(transactions_m_d_ref$market_impact_cost)
  turnover <- mean(abs(transactions_m_d_ref$delta))

  #get obs
  transactions_m_d_ref$obs <- c("IPO", rep("none",3), "delisted")

  #returns
  portfolio_returns_df <- expected_results$portfolio_returns_df
  portfolio_returns_df[5,10] <- selected_benchmark_returns_df[6,2]
  portfolio_returns_df[5,2] <- sum(transactions_m_d_ref$portfolio_weights * transactions_m_d_ref$fwd_return_1m, na.rm = TRUE)
  portfolio_returns_df[5,3] <- portfolio_returns_df[5,2] - portfolio_returns_df[5,10]
  portfolio_returns_df[5,4] <- portfolio_returns_df[5,2] - costs
  portfolio_returns_df[5,5] <- portfolio_returns_df[5,3] - costs
  portfolio_returns_df[5,6] <- sum(transactions_m_d_ref$direct_cost)
  portfolio_returns_df[5,7] <- sum(transactions_m_d_ref$market_impact_cost)
  portfolio_returns_df[5,8] <- costs
  portfolio_returns_df[5,9] <- turnover


  #update port
  updated_portfolio_weights <- portfolio_weights_m_d_ref
  updated_portfolio_weights$portfolio_weights <- (portfolio_weights_m_d_ref$portfolio_weights * (1+target_m_d_ref$fwd_return_1m/100))
  updated_portfolio_weights$portfolio_weights <- updated_portfolio_weights$portfolio_weights/sum(updated_portfolio_weights$portfolio_weights)

  #expected (validated in excel)
  expected_results <- list(
    portfolio_weights_m_d_ref = portfolio_weights_m_d_ref,
    transactions_m_d_ref = transactions_m_d_ref,
    portfolio_returns_df = portfolio_returns_df,
    updated_portfolio_weights = updated_portfolio_weights
  )

  #reset
  portfolio_weights_m_d_ref$portfolio_weights <- 0
  portfolio_returns_df[5, -1] <- NA

  results <- calculate_portfolio_returns(current_date = current_date,
                                         is_rebalancing_month = FALSE,
                                         stock_universe_m_d_ref = stock_universe_m_d_ref,
                                         target_m_d_ref = target_m_d_ref,
                                         portfolio_weights_m_d_ref = portfolio_weights_m_d_ref,
                                         portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref,
                                         portfolio_returns_df = portfolio_returns_df,
                                         selected_benchmark_returns_df = selected_benchmark_returns_df,
                                         liquidity_m_d_ref = liquidity_m_d_ref,
                                         volatility_m_d_ref = volatility_m_d_ref,
                                         transaction_costs_list = transaction_costs_list
  )


  expect_equal(results, expected_results)




})


