test_that("merge_and_rescale weight works for first rebalancing - 1 delisting in stock universe", {

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
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0, 0, 0, 0, 0)

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

  #Results
  #Expect delisting message
  expect_message(
    results <- merge_and_rescale_weights(port_weights_m_d_ref = port_weights_m_d_ref,
                                         updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                         stock_universe_m_d_ref = sw_port@universe_m_d_ref@data
    ),
    "Delisted tickers: Stock B. Of those, the following were in the portfolio: "
  )

  #Checks that only new tickers are contemplated in results
  expect_equal(results$port_weights_m_d_ref$tickers, sw_port@universe_m_d_ref@data$tickers)

  #Check that weights sum to 1
  expect_equal(sum(results$port_weights_m_d_ref$eop_port_weights), 1)

  #Check that weights match sw_port
  expect_equal(results$port_weights_m_d_ref$eop_port_weights, sw_port@universe_m_d_ref@data$weights)

  #Check that delisted_tickers are correct
  expect_equal(results$delisted_tickers_old_universe, "Stock B")
  expect_equal(results$delisted_tickers_old_portfolio, character(0))
  expect_equal(results$tickers_both_universes, dplyr::intersect(port_weights_m_d_ref$tickers, updated_port_weights_m_lstd_ref$tickers))

  #Check that ipos tickers are right
  expect_equal(results$ipo_tickers, character(0))

})


test_that("merge_and_rescale weight works for first rebalancing - 1 IPO", {

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

  #Results
  #Expect ipo message
  expect_message(
    results <- merge_and_rescale_weights(port_weights_m_d_ref = port_weights_m_d_ref,
                                         updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                         stock_universe_m_d_ref = sw_port@universe_m_d_ref@data
    ),
    "IPOs: Stock A"
  )

  #Checks that only new tickers are contemplated in results
  expect_equal(results$port_weights_m_d_ref$tickers, sw_port@universe_m_d_ref@data$tickers)

  #Check that weights sum to 1
  expect_equal(sum(results$port_weights_m_d_ref$eop_port_weights), 1)

  #Check that weights match sw_port
  expect_equal(results$port_weights_m_d_ref$eop_port_weights, sw_port@universe_m_d_ref@data$weights)

  #Check that delisted_tickers are correct
  expect_equal(results$delisted_tickers_old_universe, "Stock B")
  expect_equal(results$delisted_tickers_old_portfolio, character(0))
  expect_equal(results$tickers_both_universes, dplyr::intersect(port_weights_m_d_ref$tickers, updated_port_weights_m_lstd_ref$tickers))

  #Check that ipos tickers are right
  expect_equal(results$ipo_tickers, "Stock A")


})

test_that("merge_and_rescale weight works for non-rebalancing - 1 delisting", {

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
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.25, 0.15, 0.35, 0, 0.50)

  #Results
  #Expect delisting message
  expect_message(
    results <- merge_and_rescale_weights(port_weights_m_d_ref = port_weights_m_d_ref,
                                         updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref
    )
  )

  #Checks that only new tickers are contemplated in results
  expect_equal(results$port_weights_m_d_ref$tickers, port_weights_m_d_ref$tickers)
  expect_false("Stock B" %in% results$port_weights_m_d_ref$tickers)

  #Check that weights sum to 1
  expect_equal(sum(results$port_weights_m_d_ref$eop_port_weights), 1)

  #Check that weights are correctly re-scaled
  expect_equal(results$port_weights_m_d_ref$eop_port_weights, c(0.25/(0.25 + 0.35 + 0.50), 0.35/(0.25 + 0.35 + 0.50), 0, 0.50/(0.25 + 0.35 + 0.50)))

  #Check that delisted_tickers are correct
  expect_equal(results$delisted_tickers_old_universe, "Stock B")
  expect_equal(results$delisted_tickers_old_portfolio, "Stock B")
  expect_equal(results$tickers_both_universes, dplyr::intersect(port_weights_m_d_ref$tickers, updated_port_weights_m_lstd_ref$tickers))

  #Check that ipos tickers are right
  expect_equal(results$ipo_tickers, character(0))



})

test_that("merge_and_rescale weight works for non-rebalancing - 1 IPO", {

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
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.25, 0.15, 0.35, 0, 0.50)

  #Exclude a stock
  updated_port_weights_m_lstd_ref <- updated_port_weights_m_lstd_ref %>% dplyr::filter(!tickers == "Stock D")


  #Results
  #Expect delisting message
  expect_message(
    results <- merge_and_rescale_weights(port_weights_m_d_ref = port_weights_m_d_ref,
                                         updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref
    )
  )

  #Checks that only new tickers are contemplated in results
  expect_equal(results$port_weights_m_d_ref$tickers, port_weights_m_d_ref$tickers)
  expect_false("Stock B" %in% results$port_weights_m_d_ref$tickers)
  expect_true("Stock D" %in% results$port_weights_m_d_ref$tickers)

  #Check that weights sum to 1
  expect_equal(sum(results$port_weights_m_d_ref$eop_port_weights), 1)

  #Check that weights are correctly re-scaled
  expect_equal(results$port_weights_m_d_ref$eop_port_weights, c(0.25/(0.25 + 0.35 + 0.50), 0.35/(0.25 + 0.35 + 0.50), 0, 0.50/(0.25 + 0.35 + 0.50)))

  #Check that delisted_tickers are correct
  expect_equal(results$delisted_tickers_old_universe, "Stock B")
  expect_equal(results$delisted_tickers_old_portfolio, "Stock B")
  expect_equal(results$tickers_both_universes, dplyr::intersect(port_weights_m_d_ref$tickers, updated_port_weights_m_lstd_ref$tickers))

  #Check that ipos tickers are right
  expect_equal(results$ipo_tickers, "Stock D")



})


test_that("merge_and_rescale weights works for toy_preprocessed_data in a new rebalancing", {

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
  results <- merge_and_rescale_weights(port_weights_m_d_ref = port_weights_m_d_ref,
                                       updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                       stock_universe_m_d_ref = cw_port@universe_m_d_ref@data)

  #Check that weights sum to 1
  expect_equal(sum(results$port_weights_m_d_ref$eop_port_weights), 1)

  #Check that delisted stocks are not present
  delisted_tickers <- updated_port_weights_m_lstd_ref$tickers[which(!updated_port_weights_m_lstd_ref$tickers %in% port_weights_m_d_ref$tickers)]
  expect_false(any(delisted_tickers %in% results$port_weights_m_d_ref$tickers))

  #Check that weight match
  expect_equal(results$port_weights_m_d_ref$eop_port_weights,
               cw_port@universe_m_d_ref@data$weights)

  #Check that delisted_tickers are correct
  expect_equal(results$delisted_tickers_old_universe, delisted_tickers)
  expect_equal(results$delisted_tickers_old_portfolio, character(0))
  expect_equal(results$tickers_both_universes, dplyr::intersect(port_weights_m_d_ref$tickers, updated_port_weights_m_lstd_ref$tickers))

  #Check that ipos tickers are right
  expect_equal(results$ipo_tickers, character(0))


})

