test_that("roll_port works for artificial data, considering 3 subsequent periods", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range and others
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(Gamma = "long")
  fwd_return_m_df <- target_m_df %>% dplyr::select(id, tickers, dates, fwd_return_1m)
  transaction_costs_parameters$strategy_aum <- 1

  #Check
  check_inputs_port_backtest(signals_m_df = signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = chosen_score_metric_and_position,
                             rebalancing_months = 6, initial_buffer_period = 3, port_construction_method = "sw",
                             eligibility_quantile_range = eligibility_quantile_range, selected_benchmark = "ibov",
                             rp_method = NULL, n_random_ports = NULL, random_ports_method = NULL, opt_objective = NULL, opt_method = NULL,
                             cov_estimation_method = NULL, cov_matrix_sample_size = NULL, active_returns = FALSE, cov_matrix_benchmark = NULL,
                             daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = benchmark_returns_m_xts,
                             liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL,
                             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, main_liquidity_metric = "mean_volfin_3m",
                             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, volatility_m_df = volatility_m_df,
                             fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_parameters,
                             custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
                             upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05, verbose = TRUE
  )

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  selected_benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date) %>% dplyr::select(-smll)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2001-05-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- 0

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = chosen_score_metric_and_position,
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

  #Allocate Port
  port_alloc_results <- allocate_port(port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
                                      updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                      stock_universe_m_d_ref = sw_port@universe_m_d_ref@data,
                                      liquidity_m_d_ref = liquidity_m_d_ref,
                                      volatility_m_d_ref = volatility_m_d_ref,
                                      main_liquidity_metric = "mean_volfin_3m",
                                      transaction_costs_parameters = transaction_costs_parameters,
                                      selected_benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
                                      verbose = FALSE)

  ##Clean fwd 1m
  clean_fwd_return_1m_m_d_ref <- fwd_return_m_d_ref
  fwd_selected_benchmark_return <- benchmark_returns_m_xts$ibov[5] %>% as.numeric()

  ##Returns
  returns <- calculate_port_returns(
    clean_fwd_return_1m_m_d_ref = clean_fwd_return_1m_m_d_ref,
    fwd_selected_benchmark_return = fwd_selected_benchmark_return,
    port_weights_m_d_ref = port_alloc_results$port_weights_m_d_ref,
    total_cost = port_alloc_results$port_costs_d_ref$total_cost
  )

  ##Rolled port
  rolled_port <- roll_fwd_port_weights(
    port_weights_m_d_ref = port_alloc_results$port_weights_m_d_ref,
    clean_fwd_return_1m_m_d_ref = clean_fwd_return_1m_m_d_ref
  )

  ##Expected results
  expected_results <- list(
    rolled_fwd_port_weights_m_d_ref = rolled_port,
    fwd_port_returns_d_ref = returns
  )

  #Results
  results <- roll_port(fwd_return_m_d_ref = fwd_return_m_d_ref,
                       fwd_selected_benchmark_return = fwd_selected_benchmark_return,
                       port_weights_m_d_ref = port_alloc_results$port_weights_m_d_ref,
                       total_cost = port_alloc_results$port_costs_d_ref$total_cost,
                       verbose = FALSE
  )

  #Expectation
  expect_equal(expected_results, results)

  #Roll to next period

  #Current date
  current_date <- "2001-07-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  selected_benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date) %>% dplyr::select(-smll)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- results$rolled_fwd_port_weights_m_d_ref
  colnames(updated_port_weights_m_lstd_ref)[4] <- "bop_port_weights"

  #Allocate Port
  port_alloc_results <- allocate_port(port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
                                      updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                      stock_universe_m_d_ref = NULL,
                                      liquidity_m_d_ref = liquidity_m_d_ref,
                                      volatility_m_d_ref = volatility_m_d_ref,
                                      main_liquidity_metric = "mean_volfin_3m",
                                      transaction_costs_parameters = transaction_costs_parameters,
                                      selected_benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
                                      verbose = FALSE)

  ##Clean fwd 1m
  clean_fwd_return_1m_m_d_ref <- fwd_return_m_d_ref
  fwd_selected_benchmark_return <- benchmark_returns_m_xts$ibov[6] %>% as.numeric()

  ##Roll Port
  results <- roll_port(
    fwd_return_m_d_ref = fwd_return_m_d_ref,
    fwd_selected_benchmark_return = fwd_selected_benchmark_return,
    port_weights_m_d_ref = port_alloc_results$port_weights_m_d_ref,
    total_cost = port_alloc_results$port_costs_d_ref$total_cost,
    verbose = TRUE
  )


  ##Check that rolled_port contains only tickers of current period
  expect_equal(results$rolled_fwd_port_weights_m_d_ref$tickers, signals_m_d_ref$tickers)
  expect_false("Stock A" %in% results$rolled_fwd_port_weights_m_d_ref$tickers)

  ##Check that IPO tickers have weight of zero
  expect_true(results$rolled_fwd_port_weights_m_d_ref %>% dplyr::filter(tickers == "Stock B") %>% dplyr::pull(updated_port_weights) == 0)

  #Check that weights match the expected values (excel calc)
  expect_equal(results$rolled_fwd_port_weights_m_d_ref$updated_port_weights, c(0, 0.3625, 0.2724, 0.3651), tolerance = 1e-2)

  #Check that return match
  expect_equal(results$fwd_port_returns_d_ref$fwd_raw_return, -0.5192, tolerance = 1e-2)
  expect_equal(results$fwd_port_returns_d_ref$fwd_raw_active_return, -0.5192 - fwd_selected_benchmark_return, tolerance = 1e-2)

  #End-of-line
  #Current date
  current_date <- "2001-08-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  selected_benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date) %>% dplyr::select(-smll)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- results$rolled_fwd_port_weights_m_d_ref
  colnames(updated_port_weights_m_lstd_ref)[4] <- "bop_port_weights"

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = chosen_score_metric_and_position,
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

  #Allocate Port
  port_alloc_results <- allocate_port(port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
                                      updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                      stock_universe_m_d_ref = sw_port@universe_m_d_ref@data,
                                      liquidity_m_d_ref = liquidity_m_d_ref,
                                      volatility_m_d_ref = volatility_m_d_ref,
                                      main_liquidity_metric = "mean_volfin_3m",
                                      transaction_costs_parameters = transaction_costs_parameters,
                                      selected_benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
                                      verbose = FALSE)

  ##Clean fwd 1m
  clean_fwd_return_1m_m_d_ref <- fwd_return_m_d_ref
  fwd_selected_benchmark_return <-  benchmark_returns_m_xts[which(zoo::index(benchmark_returns_m_xts) == as.Date("2001-09-15")), "ibov"] %>% as.numeric()


  ##Roll Port
  expect_message(
  results <- roll_port(
    fwd_return_m_d_ref = fwd_return_m_d_ref,
    fwd_selected_benchmark_return = fwd_selected_benchmark_return,
    port_weights_m_d_ref = port_alloc_results$port_weights_m_d_ref,
    total_cost = port_alloc_results$port_costs_d_ref$total_cost,
    verbose = TRUE
  ),
  "End of backtest. No more dates to roll port"
  )

  #Last port will be port_alloc_results
  expect_equal(port_alloc_results$port_weights_m_d_ref$eop_port_weights, sw_port@universe_m_d_ref@data$weights)

})


test_that("roll_port works for toypreprocessed data, emulating benchmark weights", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)
  chosen_score_metric_and_position <- c(vol_36m = "short")
  transaction_costs_parameters$strategy_aum <- 1


  #Current date
  current_date <- "2023-03-15"

  #Custom Stock Weights
  ibov_weights_m_d_ref <- data.frame(
    tickers = c("VALE3", "ITUB4", "PETR4", "PETR3", "BBDC4", "B3SA3", "ELET3", "ABEV3", "BBAS3", "WEGE3",
                "ITSA4", "RENT3", "SUZB3", "EQTL3", "RADL3", "GGBR4", "RDOR3", "PRIO3", "BPAC11", "RAIL3",
                "BBSE3", "JBSS3", "ENEV3", "SBSP3", "BBDC3", "LREN3", "VIVT3", "CSAN3", "VBBR3", "HYPE3",
                "CMIG4", "ASAI3", "TOTS3", "EMBR3", "UGPA3", "KLBN11", "CCRO3", "HAPV3", "CPLE6", "MGLU3",
                "NTCO3", "EGIE3", "TIMS3", "ELET6", "ENGI11", "SANB11", "CSNA3", "TAEE11", "GOAU4", "BRFS3",
                "MULT3", "BRAP4", "CRFB3", "CPFE3", "ALOS3", "CIEL3", "ENBR3", "RRRP3", "CMIN3", "FLRY3",
                "BRKM5", "SLCE3", "SOMA3", "CYRE3", "AZUL4", "IGTI11", "COGN3", "ARZZ3", "SMTO3", "USIM5",
                "BHIA3", "RAIZ4", "BEEF3", "LWSA3", "MRVE3", "MRFG3", "PCAR3", "YDUQ3", "PETZ3", "ALPA4",
                "BPAN4", "DXCO3", "ECOR3", "EZTC3", "GOLL4", "QUAL3", "CVCB3", "CASH3"),
    weights = c(15.327, 6.292, 5.908, 5.09, 3.867, 3.775, 3.512, 3.409, 2.957, 2.929,
                2.315, 2.268, 1.782, 1.65, 1.624, 1.551, 1.515, 1.449, 1.33, 1.246,
                1.223, 1.211, 1.025, 1.006, 0.992, 0.992, 0.94, 0.931, 0.921, 0.914,
                0.873, 0.868, 0.832, 0.795, 0.791, 0.697, 0.653, 0.622, 0.611, 0.607,
                0.604, 0.595, 0.581, 0.558, 0.555, 0.52, 0.483, 0.451, 0.424, 0.399,
                0.378, 0.373, 0.368, 0.335, 0.324, 0.31, 0.302, 0.296, 0.288, 0.284,
                0.27, 0.248, 0.244, 0.238, 0.235, 0.232, 0.23, 0.229, 0.204, 0.197,
                0.185, 0.181, 0.16, 0.139, 0.126, 0.121, 0.121, 0.12, 0.117, 0.104,
                0.094, 0.094, 0.078, 0.076, 0.076, 0.068, 0.047, 0.038)
  )
  ## Add dates
  ibov_weights_m_d_ref <- ibov_weights_m_d_ref %>% dplyr::mutate(dates = as.Date("2023-03-15"), .after = tickers) %>% dplyr::mutate(id = paste0(tickers, "-", dates), .before = tickers)


  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  ibov_weights_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>%
    dplyr::left_join(ibov_weights_m_d_ref %>% dplyr::select(id, weights), by = "id") %>%
    dplyr::mutate(weights = dplyr::if_else(is.na(weights), 0, weights/100))

  port_weights_placeholder_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates) %>% dplyr::mutate(eop_port_weights = 0)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  fwd_return_m_d_ref <- fwd_return_m_df %>% dplyr::filter(dates == current_date)
  volatility_m_d_ref <- volatility_m_df %>% dplyr::filter(dates == current_date)
  selected_benchmark_weights_m_d_ref <- ibov_weights_m_d_ref %>% dplyr::rename(ibov = weights)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == "2023-02-15"), c(1:3)]
  updated_port_weights_m_lstd_ref$bop_port_weights <- 0

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = chosen_score_metric_and_position,
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

  #custom_port
  custom_port <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, port_construction_method = "custom_weights",
                                   custom_weights_m_d_ref = ibov_weights_m_d_ref)

  #Allocate Port
  port_alloc_results <- allocate_port(port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref,
                                      updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                      stock_universe_m_d_ref = custom_port@universe_m_d_ref@data,
                                      liquidity_m_d_ref = liquidity_m_d_ref,
                                      volatility_m_d_ref = volatility_m_d_ref,
                                      main_liquidity_metric = "mean_volfin_3m",
                                      transaction_costs_parameters = transaction_costs_parameters,
                                      selected_benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
                                      verbose = FALSE)



  ##Clean fwd 1m
  clean_fwd_return_1m_m_d_ref <- fwd_return_m_d_ref
  fwd_selected_benchmark_return <- benchmark_returns_m_xts$ibov[7] %>% as.numeric()

  ##Results
  results <- roll_port(
    fwd_return_m_d_ref = fwd_return_m_d_ref,
    fwd_selected_benchmark_return = fwd_selected_benchmark_return,
    port_weights_m_d_ref = port_alloc_results$port_weights_m_d_ref,
    total_cost = port_alloc_results$port_costs_d_ref$total_cost,
    verbose = FALSE
  )

  ##Check that port weights and bench match
  expect_equal(port_alloc_results$port_weights_m_d_ref$eop_port_weights, ibov_weights_m_d_ref$weights)
  ##Check that raw return is close enough
  expect_lt(results$fwd_port_returns_d_ref$fwd_raw_active_return, 0.1)
  ##Check that weights are close enough
  expect_equal(results$rolled_fwd_port_weights_m_d_ref %>% dplyr::filter(tickers == "VALE3") %>% dplyr::pull(updated_port_weights),
               0.14324, tolerance = 1e-1)
  expect_equal(results$rolled_fwd_port_weights_m_d_ref %>% dplyr::filter(tickers == "ALOS3") %>% dplyr::pull(updated_port_weights),
               0.00324, tolerance = 1e-1)
  expect_equal(results$rolled_fwd_port_weights_m_d_ref %>% dplyr::filter(tickers == "B3SA3") %>% dplyr::pull(updated_port_weights),
               0.03775, tolerance = 1e-1)

})
