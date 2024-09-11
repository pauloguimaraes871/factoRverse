test_that("set portfolio weights work for EW (signals)", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-06-15"

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
  expected_results <- signal_universe_m_d_ref
  expected_results$weights <- c(0.5, 0.5, 0)

  results <- set_portfolio_weights(universe_m_d_ref = signal_universe_m_d_ref, portfolio_construction_method = "EW")

  expect_equal(expected_results, results)

})

test_that("set portfolio weights work for SW (signals) ", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-06-15"

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

  #SW Portfolio for eligible
  signal_universe_m_d_ref <- signal_eligibility_results_list$signal_universe_m_d_ref
  expected_results <- signal_universe_m_d_ref
  expected_results$weights <- c(expected_results$final_signal[1]/sum(expected_results$final_signal[1:2]),
                                expected_results$final_signal[2]/sum(expected_results$final_signal[1:2]),
                                0)

  results <- set_portfolio_weights(universe_m_d_ref = signal_universe_m_d_ref, portfolio_construction_method = "SW")

  expect_equal(expected_results, results)

})

test_that("set portfolio weights work for RP (signals) ", {

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

  #RP Portfolio for eligible
  signal_universe_m_d_ref <- signal_eligibility_results_list$signal_universe_m_d_ref
  eligible_signals <- signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)

  cov_matrix <- estimate_covariance_matrix(tickers = eligible_signals$tickers, returns_upd_ref = selected_signals_backtest_returns_upd_ref,
                                           covariance_estimation_method = "SAM", covariance_matrix_sample_size = NULL, groups_m_d_ref = NULL)

  rp <- riskParityPortfolio::riskParityPortfolio(cov_matrix)

  expected_results <- signal_universe_m_d_ref
  expected_results$risk_contribution <- c(rp$relative_risk_contribution, NA)
  expected_results$weights <- c(rp$w, 0)


  results <- set_portfolio_weights(universe_m_d_ref = signal_universe_m_d_ref,
                                   returns_upd_ref = selected_signals_backtest_returns_upd_ref,
                                   portfolio_construction_method = "RP", covariance_matrix_sample_size = NULL,
                                   covariance_estimation_method = "SAM", groups_m_d_ref = NULL)

  expect_equal(expected_results, results)

})

test_that("set portfolio weights work for MTO (signals) - unconstrained ", {

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

  #MTO Portfolio for eligible
  signal_universe_m_d_ref <- signal_eligibility_results_list$signal_universe_m_d_ref
  eligible_signals <- signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)

  cov_matrix <- estimate_covariance_matrix(tickers = eligible_signals$tickers, returns_upd_ref = selected_signals_backtest_returns_upd_ref,
                                           covariance_estimation_method = "SAM", covariance_matrix_sample_size = NULL)


  expected_results <- signal_universe_m_d_ref
  eligible_signals <- expected_results %>% dplyr::filter(is_eligible == 1)

  port_spec <- PortfolioAnalytics::portfolio.spec(assets = eligible_signals$tickers)
  port_spec <- PortfolioAnalytics::add.constraint(port_spec, type = "full_investment")
  port_spec <- PortfolioAnalytics::add.constraint(port_spec, type = "box")

  set.seed(123)
  random_weights <- PortfolioAnalytics::random_portfolios(
    portfolio = port_spec,
    permutations = 2000,
    "sample"
  )

  #Expected returns
  returns <- random_weights %>% apply(1, function(row){
    sum(row * expected_results$final_signal[1:2])
  })

  #Expected risk
  risk <- random_weights %>% apply(1, function(row){
    sqrt(t(as.matrix(row)) %*% cov_matrix %*% as.matrix(row))
  })

  #IR
  ir = returns/risk
  random_weights[which.max(ir),]
  expected_results$weights <- c(0.75, 0.25, 0)

  set.seed(123)
  #get optimal port
  results <- set_portfolio_weights(universe_m_d_ref = signal_universe_m_d_ref,
                                   returns_upd_ref = selected_signals_backtest_returns_upd_ref,
                                   portfolio_construction_method = "MTO", covariance_matrix_sample_size = NULL,
                                   covariance_estimation_method = "SAM", groups_m_d_ref = NULL)

  expect_equal(expected_results, results)

})

test_that("set portfolio weights work for MTO (signals) - constrained ", {

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

  selected_signals_backtest_returns_upd_ref$low_Beta <- selected_signals_backtest_returns_upd_ref$low_Beta + 5
  #Define signal eligibilirt
  signal_eligibility_results_list <- define_signal_eligibility(
    selected_signals_backtest_returns_upd_ref = selected_signals_backtest_returns_upd_ref,
    selected_benchmark_returns_upd_ref = selected_benchmark_returns_upd_ref,
    signal_selection_policy = signal_selection_policy,
    signals_groups_m_d_ref = signals_groups_m_d_ref
  )

  #MTO Portfolio for eligible
  signal_universe_m_d_ref <- signal_eligibility_results_list$signal_universe_m_d_ref
  eligible_signals <- signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)

  cov_matrix <- estimate_covariance_matrix(tickers = eligible_signals$tickers, returns_upd_ref = selected_signals_backtest_returns_upd_ref,
                                           covariance_estimation_method = "PCA1", covariance_matrix_sample_size = NULL)


  expected_results <- signal_universe_m_d_ref
  eligible_signals <- expected_results %>% dplyr::filter(is_eligible == 1)

  port_spec <- PortfolioAnalytics::portfolio.spec(assets = eligible_signals$tickers)
  port_spec <- PortfolioAnalytics::add.constraint(port_spec, type = "full_investment")
  port_spec <- PortfolioAnalytics::add.constraint(port_spec, type = "box", min = c(0.4, 0.4),
                                                  max = c(0.6, 0.6))
  port_spec <- PortfolioAnalytics::add.constraint(port_spec, type = "group",
                                                  groups = list(theme.momentum = 2,
                                                                theme.value = 1),
                                                  group_min = c(0.3, 0.3),
                                                  group_max = c(0.7, 0.7))

  expected_results$max_weight <- c(0.6, 0.6, 0)
  expected_results$min_weight <- c(0.4, 0.4, 0)



  set.seed(123)
  random_weights <- PortfolioAnalytics::random_portfolios(
    portfolio = port_spec,
    permutations = 2000,
    "sample"
  )

  #Expected returns
  returns <- random_weights %>% apply(1, function(row){
    sum(row * expected_results$final_signal[1:2])
  })

  #Expected risk
  risk <- random_weights %>% apply(1, function(row){
    sqrt(t(as.matrix(row)) %*% cov_matrix %*% as.matrix(row))
  })

  #IR
  ir = returns/risk
  random_weights[which.max(ir),]
  expected_results$weights <- c(0.4, 0.6, 0)

  set.seed(123)
  #get optimal port
  concentration_constraint_policy_signal <- list(
    benchmark = signal_selection_policy$sb_benchmark_weighting,
    max_abs_active_individual_weight = signal_selection_policy$max_abs_active_individual_weight,
    max_abs_active_group_weight = signal_selection_policy$max_abs_active_group_weight
  )

  results <- set_portfolio_weights(universe_m_d_ref = signal_universe_m_d_ref,
                                   returns_upd_ref = selected_signals_backtest_returns_upd_ref,
                                   portfolio_construction_method = "MTO", covariance_matrix_sample_size = NULL,
                                   covariance_estimation_method = "PCA1", groups_m_d_ref = signals_groups_m_d_ref,
                                   concentration_constraint_policy = concentration_constraint_policy_signal
                                   )

  expect_equal(expected_results, results)

})

test_that("set portfolio weights works for stocks (all formats) ", {

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

  #Test EW
  expected_results <- stock_universe_m_d_ref
  expected_results$weights <- rep(0.25, 4)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, portfolio_construction_method = "EW")

  expect_equal(results, expected_results)

  #Test CW
  expected_results <- stock_universe_m_d_ref
  expected_results$cap_score <- signal_transform(expected_results$mean_volfin_3m, upper_quantile_winsorization, lower_quantile_winsorization)
  expected_results$weights <- expected_results$cap_score/sum(expected_results$cap_score)

  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, portfolio_construction_method = "CW",
                                   liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = "mean_volfin_3m")

  expect_equal(results, expected_results)

  #Test CS
  expected_results <- stock_universe_m_d_ref
  expected_results$cap_score <- signal_transform(expected_results$mean_volfin_3m, upper_quantile_winsorization, lower_quantile_winsorization)
  expected_results$weights <- (expected_results$cap_score * expected_results$final_signal)/sum((expected_results$cap_score * expected_results$final_signal))

  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref, portfolio_construction_method = "CS",
                                   liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = "mean_volfin_3m")

  expect_equal(results, expected_results)

  #Test RP
  expected_results <- stock_universe_m_d_ref

  daily_active_returns_upd_ref <- daily_returns_df[which(daily_returns_df$dates <= current_date),]
  adapted_tickers <- c("Stock_A", "Stock_C", "Stock_D", "Stock_E")
  stocks_groups_m_d_ref_adapted <- stocks_groups_m_d_ref
  stocks_groups_m_d_ref_adapted$tickers <- adapted_tickers

  covariance_matrix <- estimate_covariance_matrix(tickers = adapted_tickers, returns_upd_ref = daily_active_returns_upd_ref,
                                                  covariance_matrix_sample_size = 252, covariance_estimation_method = covariance_estimation_method,
                                                  groups_m_d_ref = stocks_groups_m_d_ref_adapted
                                                  )

  rp_results <- riskParityPortfolio::riskParityPortfolio(Sigma = covariance_matrix)
  expected_results$risk_contribution <- rp_results$relative_risk_contribution
  expected_results$weights <- rp_results$w

  stock_universe_m_d_ref_adapted <- stock_universe_m_d_ref
  stock_universe_m_d_ref_adapted$tickers <- adapted_tickers
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref_adapted, portfolio_construction_method = "RP",
                                   returns_upd_ref = daily_active_returns_upd_ref, groups_m_d_ref = stocks_groups_m_d_ref_adapted,
                                   covariance_matrix_sample_size = 252, covariance_estimation_method = covariance_estimation_method
                                   )
  results$tickers <- c("Stock A", "Stock C", "Stock D", "Stock E")

  expect_equal(results, expected_results)

  #Test MTO Unconstrained
  expected_results <- stock_universe_m_d_ref
  daily_active_returns_upd_ref <- daily_returns_df[which(daily_returns_df$dates <= current_date),]
  adapted_tickers <- c("Stock_A", "Stock_C", "Stock_D", "Stock_E")
  stocks_groups_m_d_ref_adapted <- stocks_groups_m_d_ref
  stocks_groups_m_d_ref_adapted$tickers <- adapted_tickers

  stock_universe_m_d_ref_adapted <- stock_universe_m_d_ref
  stock_universe_m_d_ref_adapted$tickers <- adapted_tickers

  covariance_matrix <- estimate_covariance_matrix(tickers = adapted_tickers, returns_upd_ref = daily_active_returns_upd_ref,
                                                  covariance_matrix_sample_size = 252, covariance_estimation_method = covariance_estimation_method,
                                                  groups_m_d_ref = stocks_groups_m_d_ref_adapted
  )

  #Portfolio
  port_spec <- PortfolioAnalytics::portfolio.spec(assets = adapted_tickers)
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "full_investment")
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "box")

  set.seed(123)
  rp_weights <- PortfolioAnalytics::random_portfolios(portfolio = port_spec_constrained,
                                                      permutations = 2000,
                                                      rp_method = "sample")

  #Best Portfolio for IR
  expected_results$weights <- c(0.588,0,0.166,0.2460) #Calculated manually

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref_adapted, portfolio_construction_method = "MTO",
                                   returns_upd_ref = daily_active_returns_upd_ref, groups_m_d_ref = stocks_groups_m_d_ref_adapted,
                                   covariance_matrix_sample_size = 252, covariance_estimation_method = covariance_estimation_method
  )
  results$tickers <- c("Stock A", "Stock C", "Stock D", "Stock E")
  expect_equal(results, expected_results)

  #Best Portfolio for Return
  expected_results$weights <- c(0.944,0.02,0.012,0.024) #Calculated manually

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref_adapted, portfolio_construction_method = "MTO",
                                   returns_upd_ref = daily_active_returns_upd_ref, groups_m_d_ref = stocks_groups_m_d_ref_adapted,
                                   covariance_matrix_sample_size = 252, covariance_estimation_method = covariance_estimation_method,
                                   mto_port_objective = "AR"
  )
  results$tickers <- c("Stock A", "Stock C", "Stock D", "Stock E")
  expect_equal(results, expected_results)

  #Best Portfolio for Risk
  expected_results$weights <- c(0.340,0.072,0.256,0.332) #Calculated manually

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref_adapted, portfolio_construction_method = "MTO",
                                   returns_upd_ref = daily_active_returns_upd_ref, groups_m_d_ref = stocks_groups_m_d_ref_adapted,
                                   covariance_matrix_sample_size = 252, covariance_estimation_method = covariance_estimation_method,
                                   mto_port_objective = "TE"
  )
  results$tickers <- c("Stock A", "Stock C", "Stock D", "Stock E")
  expect_equal(results, expected_results)

  #Test MTO Constrained
  expected_results <- stock_universe_m_d_ref
  daily_active_returns_upd_ref <- daily_returns_df[which(daily_returns_df$dates <= current_date),]
  adapted_tickers <- c("Stock_A", "Stock_C", "Stock_D", "Stock_E")
  stocks_groups_m_d_ref_adapted <- stocks_groups_m_d_ref
  stocks_groups_m_d_ref_adapted$tickers <- adapted_tickers

  stock_universe_m_d_ref_adapted <- stock_universe_m_d_ref
  stock_universe_m_d_ref_adapted$tickers <- adapted_tickers

  covariance_matrix <- estimate_covariance_matrix(tickers = adapted_tickers, returns_upd_ref = daily_active_returns_upd_ref,
                                                  covariance_matrix_sample_size = 252, covariance_estimation_method = covariance_estimation_method,
                                                  groups_m_d_ref = stocks_groups_m_d_ref_adapted
  )

  #Portfolio
  port_spec <- PortfolioAnalytics::portfolio.spec(assets = stock_universe_m_d_ref$tickers)
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "full_investment")
  #Box constraints
  eligible_universe_m_d_ref <- generate_box_constraints(universe_m_d_ref = stock_universe_m_d_ref,
                                                        liquidity_constraint_policy = liquidity_constraint_policy,
                                                        turnover_constraint_policy = turnover_constraint_policy,
                                                        concentration_constraint_policy = concentration_constraint_policy)

  port_spec_constrained <- PortfolioAnalytics::add.constraint(type = "box", portfolio = port_spec_constrained,
                                                              min = eligible_universe_m_d_ref$min_weight,
                                                              max = eligible_universe_m_d_ref$max_weight)
  #Group constraints
  group_constraints_helper <- generate_group_constraints(universe_m_d_ref = stock_universe_m_d_ref, concentration_constraint_policy = concentration_constraint_policy,
                                                         groups_m_d_ref = stocks_groups_m_d_ref)

  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec_constrained,
                                                              type = "group",
                                                              groups = group_constraints_helper$eligible_assets_group_membership_list,
                                                              group_min = group_constraints_helper$group_constraint_min,
                                                              group_max = group_constraints_helper$group_constraint_max
  )
  expected_results$max_weight <- eligible_universe_m_d_ref$max_weight
  expected_results$min_weight <- eligible_universe_m_d_ref$min_weight

  #Generate random ports
  set.seed(123)
  rp_weights <- PortfolioAnalytics::random_portfolios(portfolio = port_spec_constrained,
                                                      permutations = 2000,
                                                      rp_method = "sample")

  #Best Portfolio for IR
  expected_results$weights <- c(0.445,0.159,0.131,0.265) #Calculated manually

  set.seed(123)
  results <- set_portfolio_weights(universe_m_d_ref = stock_universe_m_d_ref_adapted, portfolio_construction_method = "MTO",
                                   returns_upd_ref = daily_active_returns_upd_ref, groups_m_d_ref = stocks_groups_m_d_ref_adapted,
                                   covariance_matrix_sample_size = 252, covariance_estimation_method = covariance_estimation_method,
                                   liquidity_constraint_policy = liquidity_constraint_policy,
                                   turnover_constraint_policy = turnover_constraint_policy,
                                   concentration_constraint_policy = concentration_constraint_policy
  )

  results$tickers <- expected_results$tickers
  expect_equal(results, expected_results)

  #Check that constraints match expectations
  #Upper box
  expect_true(all(
    all(rp_weights[,1] <= eligible_universe_m_d_ref$max_weight[1]),
    all(rp_weights[,2] <= eligible_universe_m_d_ref$max_weight[2]),
    all(rp_weights[,3] <= eligible_universe_m_d_ref$max_weight[3]),
    all(rp_weights[,4] <= eligible_universe_m_d_ref$max_weight[4])))

  #Lower box
  expect_true(all(
    all(rp_weights[,1] >= eligible_universe_m_d_ref$min_weight[1]),
    all(rp_weights[,2] >= eligible_universe_m_d_ref$min_weight[2]),
    all(rp_weights[,3] >= eligible_universe_m_d_ref$min_weight[3]),
    all(rp_weights[,4] >= eligible_universe_m_d_ref$min_weight[4])))

  #Group
  sector_cyclical <- rp_weights[,3] + rp_weights[,4]
  sector_financial <- rp_weights[,2]
  sector_oil <- rp_weights[,1]
  subsector_education <- rp_weights[,4]
  subsector_insurance <- rp_weights[,2]
  subsector_oil <- rp_weights[,1]
  subsector_retail <- rp_weights[,3]

  #Lower Group
  expect_true(all(
    all(sector_cyclical >= group_constraints_helper$group_constraint_min[1]),
    all(sector_financial >= group_constraints_helper$group_constraint_min[2]),
    all(sector_oil >= group_constraints_helper$group_constraint_min[3]),
    all(subsector_education >= group_constraints_helper$group_constraint_min[4]),
    all(subsector_insurance >= group_constraints_helper$group_constraint_min[5]),
    all(subsector_oil >= group_constraints_helper$group_constraint_min[6]),
    all(subsector_retail >= group_constraints_helper$group_constraint_min[7])
  ))

  #Upper group
  expect_true(all(
    all(sector_cyclical <= group_constraints_helper$group_constraint_max[1]),
    all(sector_financial <= group_constraints_helper$group_constraint_max[2]),
    all(sector_oil <= group_constraints_helper$group_constraint_max[3]),
    all(subsector_education <= group_constraints_helper$group_constraint_max[4]),
    all(subsector_insurance <= group_constraints_helper$group_constraint_max[5]),
    all(subsector_oil <= group_constraints_helper$group_constraint_max[6]),
    all(subsector_retail <= group_constraints_helper$group_constraint_max[7])
  ))

  })



