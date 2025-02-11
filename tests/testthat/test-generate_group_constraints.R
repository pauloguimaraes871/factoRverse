test_that("generate_group_constraints works for signals", {

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
  selected_signal_themes_m_d_ref <- selected_signals_and_backtest_list$selected_signal_themes_m_df

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
    max_abs_active_group_weight = 0.2
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
                                        groups_m_d_ref = selected_signal_themes_m_d_ref)

  #expect
  expect_equal(results, expected_results)

})

test_that("generate_group_constraints correctly maps stocks into groups and sets max and min weights for stocks (2 groups)", {

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
  eligible_universe_m_d_ref <- generate_box_constraints(universe_m_d_ref = stock_universe_m_d_ref,
                                                        liquidity_constraint_policy = liquidity_constraint_policy,
                                                        turnover_constraint_policy = turnover_constraint_policy,
                                                        concentration_constraint_policy = concentration_constraint_policy
                                                        )

  eligible_universe_m_d_ref %>% dplyr::group_by(Subsector) %>% dplyr::summarize(sector_sum = sum(ibov_bench_weights))
  eligible_universe_m_d_ref %>% dplyr::group_by(Sector) %>% dplyr::summarize(sector_sum = sum(ibov_bench_weights))

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
                             groups_m_d_ref = stock_groups_m_d_ref)

  expect_equal(results, expected_results, tolerance = 1e-2)

})

test_that("generate_group_constraints correctly maps stocks into groups and sets max and min weights, considering multiple groups - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

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

  #Test MVO Constrained
  expected_results <- stock_universe_m_d_ref
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  eligible_tickers <- expected_results %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  covariance_matrix <- estimate_covariance_matrix(tickers = eligible_tickers, returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 60, cov_estimation_method = "cc",
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )


  #Portfolio
  port_spec <- PortfolioAnalytics::portfolio.spec(assets = eligible_tickers)
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "full_investment")
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "box")

  #Box constraints
  eligible_universe_m_d_ref <- generate_box_constraints(universe_m_d_ref = stock_universe_m_d_ref,
                                                        liquidity_constraint_policy = liquidity_constraint_policy,
                                                        concentration_constraint_policy = concentration_constraint_policy)

  port_spec_constrained <- PortfolioAnalytics::add.constraint(type = "box", portfolio = port_spec_constrained,
                                                              min = eligible_universe_m_d_ref$min_weight,
                                                              max = eligible_universe_m_d_ref$max_weight)

  #Group constraints
  group_constraints_helper <- generate_group_constraints(universe_m_d_ref = stock_universe_m_d_ref, concentration_constraint_policy = concentration_constraint_policy,
                                                         groups_m_d_ref = stock_groups_m_d_ref)

  #Check that numbers inside each element of eligible_assets_group_membership_list is unique for a given sector classification
  expect_equal(group_constraints_helper$eligible_assets_group_membership_list[c(1:8)] %>% unlist() %>% unname() %>% sort(),
               seq_len(nrow(stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)))
  )

  expect_equal(group_constraints_helper$eligible_assets_group_membership_list[c(9:12)] %>% unlist() %>% unname() %>% sort(),
               seq_len(nrow(stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)))
  )

  #Check that stocks are correctly classified
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>%  dplyr::slice(group_constraints_helper$eligible_assets_group_membership_list$sectors.Agro)
    %>% dplyr::pull(sectors) %>% unique(), "Agro"
  )
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>%  dplyr::slice(group_constraints_helper$eligible_assets_group_membership_list$`sectors.Bancos e Serviços Financeiros`)
    %>% dplyr::pull(sectors) %>% unique(), "Bancos e Serviços Financeiros"
  )
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>%  dplyr::slice(group_constraints_helper$eligible_assets_group_membership_list$`sectors.Consumo Cíclico`)
    %>% dplyr::pull(sectors) %>% unique(), "Consumo Cíclico"
  )
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>%  dplyr::slice(group_constraints_helper$eligible_assets_group_membership_list$`sectors.Consumo Não-Cíclico`)
    %>% dplyr::pull(sectors) %>% unique(), "Consumo Não-Cíclico"
  )
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>%  dplyr::slice(group_constraints_helper$eligible_assets_group_membership_list$sectors.Indústria)
    %>% dplyr::pull(sectors) %>% unique(), "Indústria"
  )
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>%  dplyr::slice(group_constraints_helper$eligible_assets_group_membership_list$`sectors.Materiais Básicos`)
    %>% dplyr::pull(sectors) %>% unique(), "Materiais Básicos"
  )
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>%  dplyr::slice(group_constraints_helper$eligible_assets_group_membership_list$`sectors.Petróleo gás e biocombustíveis`)
    %>% dplyr::pull(sectors) %>% unique(), "Petróleo gás e biocombustíveis"
  )
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>%  dplyr::slice(group_constraints_helper$eligible_assets_group_membership_list$`sectors.Utilidade Pública`)
    %>% dplyr::pull(sectors) %>% unique(), "Utilidade Pública"
  )
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>%  dplyr::slice(group_constraints_helper$eligible_assets_group_membership_list$`macro_sector.Doméstico Cíclico`)
    %>% dplyr::pull(macro_sector) %>% unique(), "Doméstico Cíclico"
  )
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>%  dplyr::slice(group_constraints_helper$eligible_assets_group_membership_list$`macro_sector.Doméstico Defensivo`)
    %>% dplyr::pull(macro_sector) %>% unique(), "Doméstico Defensivo"
  )
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>%  dplyr::slice(group_constraints_helper$eligible_assets_group_membership_list$macro_sector.Exportador)
    %>% dplyr::pull(macro_sector) %>% unique(), "Exportador"
  )
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>%  dplyr::slice(group_constraints_helper$eligible_assets_group_membership_list$macro_sector.Indústria)
    %>% dplyr::pull(macro_sector) %>% unique(), "Indústria"
  )
  #Check that weights are as expected
  ##Max
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::group_by(sectors) %>% dplyr::summarize(w = sum(ibov_bench_weights)) %>% dplyr::pull(w) +
      concentration_constraint_policy$max_abs_active_group_weight[1],
    group_constraints_helper$group_constraint_max[1:8] #Sectors
  )
  expect_equal(
    stock_universe_m_d_ref %>% dplyr::group_by(macro_sector) %>% dplyr::summarize(w = sum(ibov_bench_weights)) %>% dplyr::pull(w) +
      concentration_constraint_policy$max_abs_active_group_weight[2],
    group_constraints_helper$group_constraint_max[9:12] #Macro
  )

  ##Min
  expect_equal(
    pmax(stock_universe_m_d_ref %>% dplyr::group_by(sectors) %>% dplyr::summarize(w = sum(ibov_bench_weights)) %>% dplyr::pull(w) -
           concentration_constraint_policy$max_abs_active_group_weight[1], 0),
    group_constraints_helper$group_constraint_min[1:8] #Sectors
  )
  expect_equal(
    pmax(stock_universe_m_d_ref %>% dplyr::group_by(macro_sector) %>% dplyr::summarize(w = sum(ibov_bench_weights)) %>% dplyr::pull(w) -
           concentration_constraint_policy$max_abs_active_group_weight[2], 0),
    group_constraints_helper$group_constraint_min[9:12] #Macro
  )
})

test_that("generate_group_constraints stops when there are groups with no eligible stocks - toy_preprocessed", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.99, 1)

  #Current date
  current_date <- "2023-04-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)

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

  #Test MVO Constrained
  #Makes 'Industria' without components
  stock_universe_m_d_ref[stock_universe_m_d_ref$tickers == "WHRL3", "is_eligible"] <- 0

  expected_results <- stock_universe_m_d_ref
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  eligible_tickers <- expected_results %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  covariance_matrix <- estimate_covariance_matrix(tickers = eligible_tickers, returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 60, cov_estimation_method = "cc",
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )



  #Portfolio
  port_spec <- PortfolioAnalytics::portfolio.spec(assets = eligible_tickers)
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "full_investment")
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "box")

  #Box constraints
  eligible_universe_m_d_ref <- generate_box_constraints(universe_m_d_ref = stock_universe_m_d_ref,
                                                        liquidity_constraint_policy = liquidity_constraint_policy,
                                                        concentration_constraint_policy = concentration_constraint_policy)

  port_spec_constrained <- PortfolioAnalytics::add.constraint(type = "box", portfolio = port_spec_constrained,
                                                              min = eligible_universe_m_d_ref$min_weight,
                                                              max = eligible_universe_m_d_ref$max_weight)

  #Group constraints
  expect_error(
    generate_group_constraints(universe_m_d_ref = stock_universe_m_d_ref, concentration_constraint_policy = concentration_constraint_policy,
                               groups_m_d_ref = stock_groups_m_d_ref),
    "Some groups have no eligible assets. Please enable group representativeness to force inclusion of stocks into every group."
  )


})
