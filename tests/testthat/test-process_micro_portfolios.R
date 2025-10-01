test_that("set top down micro weights works without group weights for simple RP case in a top_down_proxy context", {

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
  ridge_pen <- 1
  n_resamples <- 3
  exp_ret_score_jitter <- 0.02
  cov_jitter <- 0.01
  concentration_constraint_policy$max_abs_active_group_weight <- NULL

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                                          chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Set ibov_bench_weights as target_port_m_d_ref
  target_port_m_d_ref <- stock_universe_m_d_ref %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::left_join(benchmark_weights_m_d_ref %>%
                       dplyr::select(id, ibov), by = "id") %>%
    dplyr::rename(target_weights = ibov)

  #Classify stock universe
  # In a top_down proxy context, arguments not needed to the top down proxy might
  # have been passed early based on micro port construction method
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    target_port_m_d_ref = target_port_m_d_ref,
    ridge_pen = ridge_pen,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  # Create covariance matrix
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  eligible_tickers <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  covariance_matrix <- estimate_covariance_matrix(tickers = eligible_tickers,
                                                  returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 60, cov_estimation_method = "cc",
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  #Set top down micro weights

    ## Get eligible stock universe
    eligible_universe_m_d_ref <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)

    ## Get groups
    groups <- eligible_universe_m_d_ref$macro_sector %>% unique()
    group_members <- lapply(groups, function(g) {
      eligible_universe_m_d_ref %>%
        dplyr::filter(macro_sector == g) %>%
        dplyr::pull(tickers)
    })
    names(group_members) <- groups



    ## For each group
    micro_port_list <- list()
    for (g in seq_along(groups)){

      group <- groups[g]

      ## Get members
      group_tickers <- eligible_universe_m_d_ref %>%
        dplyr::filter(macro_sector == group) %>%
        dplyr::pull(tickers)
      sub_universe_m_d_ref <- eligible_universe_m_d_ref %>%
        dplyr::filter(tickers %in% group_tickers)

      ## Get covariance matrix
      sub_cov_matrix <- covariance_matrix[group_tickers, group_tickers, drop = FALSE]

      ## Get liquidity m_df
      sub_liquidity_m_d_ref <- liquidity_m_d_ref %>%
        dplyr::filter(tickers %in% group_tickers)

      ## Defensively remove weight cols
      sub_universe_m_d_ref$ibov_bench_weights <- NULL
      sub_universe_m_d_ref$target_weights <- NULL

      ## Set micro weights
      micro_port_list[[g]] <- set_portfolio_weights(
        universe_m_d_ref = sub_universe_m_d_ref,
        port_construction_method = "rp",
        liquidity_constraint_policy = NULL,
        liquidity_m_d_ref = sub_liquidity_m_d_ref,
        concentration_constraint_policy = NULL,
        turnover_constraint_policy = NULL,
        groups_m_d_ref = NULL,
        covariance_matrix = sub_cov_matrix
      )

    }

    names(micro_port_list) <- groups
    expected_results <- micro_port_list

    #Process micro portfolios
    results <- process_micro_portfolios(
      parallel = TRUE,
      groups = groups,
      group_members = group_members,
      group_weights = NULL,
      micro_port_construction_method = "rp",
      universe_m_d_ref = eligible_universe_m_d_ref,
      covariance_matrix = covariance_matrix,
      liquidity_m_d_ref = liquidity_m_d_ref
    )

    # Test that there are as many micro portfolios as groups
    testthat::expect_equal(length(results), length(groups))

    # Test that all groups are contained in results
    testthat::expect_true(all(groups %in% names(results)))

    # Test that each micro portfolio is of S4 class "port"
    testthat::expect_true(all(sapply(results, function(x) inherits(x, "port"))))

    # Test that weights sum to 1 in each micro portfolio
    testthat::expect_true(all(sapply(results, function(x) abs(sum(x@universe_m_d_ref@data$weights) - 1) < 1e-6)))

    # Test that all tickers in each micro portfolio belong to the correct group
    testthat::expect_true(all(sapply(names(results), function(g) {
      all(results[[g]]@universe_m_d_ref@data$tickers %in% group_members[[g]])
    })))

    # Test that tickers in each micro portfolio are part of the eligible universe
    testthat::expect_true(all(sapply(names(results), function(g) {
      all(results[[g]]@universe_m_d_ref@data$tickers %in% eligible_universe_m_d_ref$tickers)
    })))

    # Test that results and expected results match
    testthat::expect_equal(results, expected_results)


})

test_that("set top down micro weights works without group weights for simple CS case in a top_down_proxy context", {

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
  ridge_pen <- 1
  n_resamples <- 3
  exp_ret_score_jitter <- 0.02
  cov_jitter <- 0.01
  concentration_constraint_policy$max_abs_active_group_weight <- NULL

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                                          chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Set ibov_bench_weights as target_port_m_d_ref
  target_port_m_d_ref <- stock_universe_m_d_ref %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::left_join(benchmark_weights_m_d_ref %>%
                       dplyr::select(id, ibov), by = "id") %>%
    dplyr::rename(target_weights = ibov)

  #Classify stock universe
  # In a top_down proxy context, arguments not needed to the top down proxy might
  # have been passed early based on micro port construction method
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    target_port_m_d_ref = target_port_m_d_ref,
    ridge_pen = ridge_pen,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  # Create covariance matrix
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  eligible_tickers <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  covariance_matrix <- estimate_covariance_matrix(tickers = eligible_tickers,
                                                  returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 60, cov_estimation_method = "cc",
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  #Set top down micro weights

  ## Get eligible stock universe
  eligible_universe_m_d_ref <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)

  ## Get groups
  groups <- eligible_universe_m_d_ref$macro_sector %>% unique()
  group_members <- lapply(groups, function(g) {
    eligible_universe_m_d_ref %>%
      dplyr::filter(macro_sector == g) %>%
      dplyr::pull(tickers)
  })
  names(group_members) <- groups



  ## For each group
  micro_port_list <- list()
  for (g in seq_along(groups)){

    group <- groups[g]

    ## Get members
    group_tickers <- eligible_universe_m_d_ref %>%
      dplyr::filter(macro_sector == group) %>%
      dplyr::pull(tickers)
    sub_universe_m_d_ref <- eligible_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

    ## Get covariance matrix
    sub_cov_matrix <- covariance_matrix[group_tickers, group_tickers, drop = FALSE]

    ## Get liquidity m_df
    sub_liquidity_m_d_ref <- liquidity_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

    ## Defensively remove weight cols
    sub_universe_m_d_ref$ibov_bench_weights <- NULL
    sub_universe_m_d_ref$target_weights <- NULL

    ## Set micro weights
    micro_port_list[[g]] <- set_portfolio_weights(
      universe_m_d_ref = sub_universe_m_d_ref,
      port_construction_method = "cs",
      liquidity_constraint_policy = NULL,
      liquidity_m_d_ref = sub_liquidity_m_d_ref,
      concentration_constraint_policy = NULL,
      turnover_constraint_policy = NULL,
      groups_m_d_ref = NULL,
      covariance_matrix = sub_cov_matrix,
      cap_weighting_metric = "mean_volfin_3m"
    )

  }

  names(micro_port_list) <- groups
  expected_results <- micro_port_list

  #Process micro portfolios
  results <- process_micro_portfolios(
    parallel = TRUE,
    groups = groups,
    group_members = group_members,
    group_weights = NULL,
    micro_port_construction_method = "cs",
    cap_weighting_metric = "mean_volfin_3m",
    universe_m_d_ref = eligible_universe_m_d_ref,
    covariance_matrix = covariance_matrix,
    liquidity_m_d_ref = liquidity_m_d_ref
  )

  # Test that there are as many micro portfolios as groups
  testthat::expect_equal(length(results), length(groups))

  # Test that all groups are contained in results
  testthat::expect_true(all(groups %in% names(results)))

  # Test that each micro portfolio is of S4 class "port"
  testthat::expect_true(all(sapply(results, function(x) inherits(x, "port"))))

  # Test that weights sum to 1 in each micro portfolio
  testthat::expect_true(all(sapply(results, function(x) abs(sum(x@universe_m_d_ref@data$weights) - 1) < 1e-6)))

  # Test that all tickers in each micro portfolio belong to the correct group
  testthat::expect_true(all(sapply(names(results), function(g) {
    all(results[[g]]@universe_m_d_ref@data$tickers %in% group_members[[g]])
  })))

  # Test that tickers in each micro portfolio are part of the eligible universe
  testthat::expect_true(all(sapply(names(results), function(g) {
    all(results[[g]]@universe_m_d_ref@data$tickers %in% eligible_universe_m_d_ref$tickers)
  })))

  # Test that results and expected results match
  testthat::expect_equal(results, expected_results)


})

test_that("set top down micro weights work with group weights for a MVO + constraints in final step,
          considering edge cases: a) Weights summing more than 1, b) One sector with 0%,
          c) one sector with 100% and rest with 0%, d) One with 99% and other with 1%,
          causing individual constraints to break", {


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
  ridge_pen <- 1
  n_resamples <- 3
  exp_ret_score_jitter <- 0.02
  cov_jitter <- 0.01
  concentration_constraint_policy$max_abs_active_group_weight <- NULL

  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref,
                                                          chosen_score_metric_and_position = c(vol_36m = "short"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Set ibov_bench_weights as target_port_m_d_ref
  target_port_m_d_ref <- stock_universe_m_d_ref %>%
    dplyr::select(id, tickers, dates) %>%
    dplyr::left_join(benchmark_weights_m_d_ref %>%
                       dplyr::select(id, ibov), by = "id") %>%
    dplyr::rename(target_weights = ibov)

  #Classify stock universe
  # In a top_down proxy context, arguments not needed to the top down proxy might
  # have been passed early based on micro port construction method
  stock_universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range,
    liquidity_m_d_ref = liquidity_m_d_ref,
    target_port_m_d_ref = target_port_m_d_ref,
    ridge_pen = ridge_pen,
    liquidity_constraint_policy = liquidity_constraint_policy,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy
  )

  # Create covariance matrix
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  elig_tickers <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  covariance_matrix <- estimate_covariance_matrix(tickers = elig_tickers,
                                                  returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 60, cov_estimation_method = "cc",
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  #Set top down micro weights

  ## Get eligible stock universe
  elig_universe_m_d_ref <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)

  ## Get groups
  groups <- elig_universe_m_d_ref$macro_sector %>% unique()
  group_members <- lapply(groups, function(g) {
    elig_universe_m_d_ref %>%
      dplyr::filter(macro_sector == g) %>%
      dplyr::pull(tickers)
  })
  names(group_members) <- groups

  # Define group weights ad hoc (Exportador sums more than 1)
  group_weights <- c(0.5, 0.2, 0.2, 0.1)
  names(group_weights) <- groups


  ## For each group
  micro_port_list <- list()
  set.seed(123)
  for (g in seq_along(groups)){

    group <- groups[g]

    ## Get members
    group_tickers <- elig_universe_m_d_ref %>%
      dplyr::filter(macro_sector == group) %>%
      dplyr::pull(tickers)
    sub_univ_m_d_ref <- elig_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

    ## Get covariance matrix
    sub_cov_matrix <- covariance_matrix[group_tickers, group_tickers, drop = FALSE]

    ## Get liquidity m_df
    sub_liquidity_m_d_ref <- liquidity_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

    ## Rescale weights
    sub_univ_m_d_ref$ibov_bench_weights <-
      sub_univ_m_d_ref$ibov_bench_weights / group_weights[group]
    if (sum(sub_univ_m_d_ref$ibov_bench_weights) > 1){
      sub_univ_m_d_ref$ibov_bench_weights <-
        sub_univ_m_d_ref$ibov_bench_weights / sum(sub_univ_m_d_ref$ibov_bench_weights)
    }

    sub_univ_m_d_ref$target_weights <-
      sub_univ_m_d_ref$target_weights / group_weights[group]
    if (sum(sub_univ_m_d_ref$target_weights) > 1){
      sub_univ_m_d_ref$target_weights <-
        sub_univ_m_d_ref$target_weights / sum(sub_univ_m_d_ref$target_weights)
    }

    sub_concentration_constraint_pol <- concentration_constraint_policy
    sub_concentration_constraint_pol$max_abs_active_individual_weight <-
      sub_concentration_constraint_pol$max_abs_active_individual_weight / group_weights[group]

    sub_liquidity_constraint_pol <- liquidity_constraint_policy
    sub_liquidity_constraint_pol$liquidity_cap_rules <-
      sub_liquidity_constraint_pol$liquidity_cap_rules / group_weights[group]


    ## Set micro weights
    micro_port_list[[g]] <- set_portfolio_weights(
      universe_m_d_ref = sub_univ_m_d_ref,
      port_construction_method = "mvo",
      liquidity_constraint_policy = sub_liquidity_constraint_pol,
      liquidity_m_d_ref = sub_liquidity_m_d_ref,
      concentration_constraint_policy = sub_concentration_constraint_pol,
      turnover_constraint_policy = NULL,
      groups_m_d_ref = NULL,
      covariance_matrix = sub_cov_matrix,
      cap_weighting_metric = "mean_volfin_3m",
      ridge_pen = 0.5,
      n_resamples = 3,
      exp_ret_score_jitter = 0.2,
      cov_eigval_jitter = 0.01,
      opt_objective = "risk",
      n_random_ports = 500
    )

  }

  names(micro_port_list) <- groups
  expected_results <- micro_port_list

  ## Do the same with the function
  set.seed(123)
  results <- process_micro_portfolios(
    parallel = FALSE,
    groups = groups,
    group_members = group_members,
    group_weights = group_weights,
    micro_port_construction_method = "mvo",
    cap_weighting_metric = "mean_volfin_3m",
    universe_m_d_ref = elig_universe_m_d_ref,
    covariance_matrix = covariance_matrix,
    liquidity_m_d_ref = liquidity_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    liquidity_constraint_policy = liquidity_constraint_policy,
    ridge_pen = 0.5,
    n_resamples = 3,
    exp_ret_score_jitter = 0.2,
    cov_eigval_jitter = 0.01,
    opt_objective = "risk",
    n_random_ports = 500
  )

  ## Test that constraints are satisfied at the final portfolio-level
  for (group in groups){
    group_port <- results[[group]]@universe_m_d_ref@data
    ## Concentration constraints
    max_weights <- elig_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_port$tickers) %>%
      dplyr::pull(ibov_bench_weights) + concentration_constraint_policy$max_abs_active_individual_weight
    min_weights <- pmax(elig_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_port$tickers) %>%
      dplyr::pull(ibov_bench_weights) - concentration_constraint_policy$max_abs_active_individual_weight,
      0)

    testthat::expect_true(all(group_port$weights * group_weights[group] <= max_weights))
    testthat::expect_true(all(group_port$weights * group_weights[group] >= min_weights))

    ## Liquidity constraints
    liquidity_caps <- liquidity_constraint_policy$liquidity_cap_rules
    for (cap in names(liquidity_caps)){
      testthat::expect_true(
        all(group_port %>%
              dplyr::filter(liquidity_classification == cap) %>%
              dplyr::pull(weights) * group_weights[group] <= liquidity_caps[cap]
        )
      )
    }
  }

  # Test that there are as many micro portfolios as groups
  testthat::expect_equal(length(results), length(groups))

  # Test that all groups are contained in results
  testthat::expect_true(all(groups %in% names(results)))

  # Test that each micro portfolio is of S4 class "port"
  testthat::expect_true(all(sapply(results, function(x) inherits(x, "port"))))

  # Test that weights sum to 1 in each micro portfolio
  testthat::expect_true(all(sapply(results, function(x) abs(sum(x@universe_m_d_ref@data$weights) - 1) < 1e-6)))

  # Test that all tickers in each micro portfolio belong to the correct group
  testthat::expect_true(all(sapply(names(results), function(g) {
    all(results[[g]]@universe_m_d_ref@data$tickers %in% group_members[[g]])
  })))

  # Test that tickers in each micro portfolio are part of the eligible universe
  testthat::expect_true(all(sapply(names(results), function(g) {
    all(results[[g]]@universe_m_d_ref@data$tickers %in% elig_universe_m_d_ref$tickers)
  })))

  # Test that results and expected results match
  testthat::expect_equal(results, expected_results)




  # Define group weights ad hoc (Indústria with zero weight)
  group_weights <- c(0.5, 0.2, 0.3, 0)
  names(group_weights) <- groups


  ## For each group
  micro_port_list <- list()
  set.seed(123)
  for (g in seq_along(groups)){

    group <- groups[g]

    if (group == "Indústria") next

    ## Get members
    group_tickers <- elig_universe_m_d_ref %>%
      dplyr::filter(macro_sector == group) %>%
      dplyr::pull(tickers)
    sub_univ_m_d_ref <- elig_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

    ## Get covariance matrix
    sub_cov_matrix <- covariance_matrix[group_tickers, group_tickers, drop = FALSE]

    ## Get liquidity m_df
    sub_liquidity_m_d_ref <- liquidity_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

    ## Rescale weights
    sub_univ_m_d_ref$ibov_bench_weights <-
      sub_univ_m_d_ref$ibov_bench_weights / group_weights[group]
    if (sum(sub_univ_m_d_ref$ibov_bench_weights) > 1){
      sub_univ_m_d_ref$ibov_bench_weights <-
        sub_univ_m_d_ref$ibov_bench_weights / sum(sub_univ_m_d_ref$ibov_bench_weights)
    }

    sub_univ_m_d_ref$target_weights <-
      sub_univ_m_d_ref$target_weights / group_weights[group]
    if (sum(sub_univ_m_d_ref$target_weights) > 1){
      sub_univ_m_d_ref$target_weights <-
        sub_univ_m_d_ref$target_weights / sum(sub_univ_m_d_ref$target_weights)
    }

    sub_concentration_constraint_pol <- concentration_constraint_policy
    sub_concentration_constraint_pol$max_abs_active_individual_weight <-
      sub_concentration_constraint_pol$max_abs_active_individual_weight / group_weights[group]

    sub_liquidity_constraint_pol <- liquidity_constraint_policy
    sub_liquidity_constraint_pol$liquidity_cap_rules <-
      sub_liquidity_constraint_pol$liquidity_cap_rules / group_weights[group]


    ## Set micro weights
    micro_port_list[[g]] <- set_portfolio_weights(
      universe_m_d_ref = sub_univ_m_d_ref,
      port_construction_method = "mvo",
      liquidity_constraint_policy = sub_liquidity_constraint_pol,
      liquidity_m_d_ref = sub_liquidity_m_d_ref,
      concentration_constraint_policy = sub_concentration_constraint_pol,
      turnover_constraint_policy = NULL,
      groups_m_d_ref = NULL,
      covariance_matrix = sub_cov_matrix,
      cap_weighting_metric = "mean_volfin_3m",
      ridge_pen = 0.5,
      n_resamples = 3,
      exp_ret_score_jitter = 0.2,
      cov_eigval_jitter = 0.01,
      opt_objective = "risk",
      n_random_ports = 500
    )

  }

  # Add a 'NULL' element for the group with zero weight
  micro_port_list <- c(micro_port_list, list(Indústria = NULL))
  names(micro_port_list) <- groups
  expected_results <- micro_port_list

  ## Do the same with the function
  set.seed(123)
  results <- process_micro_portfolios(
    parallel = FALSE,
    groups = groups,
    group_members = group_members,
    group_weights = group_weights,
    micro_port_construction_method = "mvo",
    cap_weighting_metric = "mean_volfin_3m",
    universe_m_d_ref = elig_universe_m_d_ref,
    covariance_matrix = covariance_matrix,
    liquidity_m_d_ref = liquidity_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    liquidity_constraint_policy = liquidity_constraint_policy,
    ridge_pen = 0.5,
    n_resamples = 3,
    exp_ret_score_jitter = 0.2,
    cov_eigval_jitter = 0.01,
    opt_objective = "risk",
    n_random_ports = 500
  )

  ## Test that constraints are satisfied at the final portfolio-level
  for (group in groups){

    if (group == "Indústria") {
      testthat::expect_null(results[[group]])
      next
    }

    group_port <- results[[group]]@universe_m_d_ref@data
    ## Concentration constraints
    max_weights <- elig_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_port$tickers) %>%
      dplyr::pull(ibov_bench_weights) + concentration_constraint_policy$max_abs_active_individual_weight
    min_weights <- pmax(elig_universe_m_d_ref %>%
                          dplyr::filter(tickers %in% group_port$tickers) %>%
                          dplyr::pull(ibov_bench_weights) - concentration_constraint_policy$max_abs_active_individual_weight,
                        0)

    testthat::expect_true(all(group_port$weights * group_weights[group] <= max_weights))
    testthat::expect_true(all(group_port$weights * group_weights[group] >= min_weights))

    ## Liquidity constraints
    liquidity_caps <- liquidity_constraint_policy$liquidity_cap_rules
    for (cap in names(liquidity_caps)){
      testthat::expect_true(
        all(group_port %>%
              dplyr::filter(liquidity_classification == cap) %>%
              dplyr::pull(weights) * group_weights[group] <= liquidity_caps[cap]
        )
      )
    }
  }

  # Test that there are as many micro portfolios as groups
  testthat::expect_equal(length(results), length(groups))

  # Test that all groups are contained in results
  testthat::expect_true(all(groups %in% names(results)))

  # Test that each micro portfolio is of S4 class "port", except the last one
  testthat::expect_true(all(sapply(results[-4], function(x) inherits(x, "port"))))

  # Test that weights sum to 1 in each micro portfolio
  testthat::expect_true(all(sapply(results[-4], function(x) abs(sum(x@universe_m_d_ref@data$weights) - 1) < 1e-6)))

  # Test that all tickers in each micro portfolio belong to the correct group
  testthat::expect_true(all(sapply(names(results)[-4], function(g) {
    all(results[[g]]@universe_m_d_ref@data$tickers %in% group_members[[g]])
  })))

  # Test that tickers in each micro portfolio are part of the eligible universe
  testthat::expect_true(all(sapply(names(results)[-4], function(g) {
    all(results[[g]]@universe_m_d_ref@data$tickers %in% elig_universe_m_d_ref$tickers)
  })))

  # Test that results and expected results match
  testthat::expect_equal(results, expected_results)





  # Define group weights ad hoc (Exportador 100% weight)
  group_weights <- c(0, 1, 0, 0)
  names(group_weights) <- groups


  ## For each group
  micro_port_list <- list()
  set.seed(123)
  for (g in seq_along(groups)){

    group <- groups[g]

    if (group != "Exportador") next

    ## Get members
    group_tickers <- elig_universe_m_d_ref %>%
      dplyr::filter(macro_sector == group) %>%
      dplyr::pull(tickers)
    sub_univ_m_d_ref <- elig_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

    ## Get covariance matrix
    sub_cov_matrix <- covariance_matrix[group_tickers, group_tickers, drop = FALSE]

    ## Get liquidity m_df
    sub_liquidity_m_d_ref <- liquidity_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

    ## Rescale weights
    sub_univ_m_d_ref$ibov_bench_weights <-
      sub_univ_m_d_ref$ibov_bench_weights / group_weights[group]
    if (sum(sub_univ_m_d_ref$ibov_bench_weights) > 1){
      sub_univ_m_d_ref$ibov_bench_weights <-
        sub_univ_m_d_ref$ibov_bench_weights / sum(sub_univ_m_d_ref$ibov_bench_weights)
    }

    sub_univ_m_d_ref$target_weights <-
      sub_univ_m_d_ref$target_weights / group_weights[group]
    if (sum(sub_univ_m_d_ref$target_weights) > 1){
      sub_univ_m_d_ref$target_weights <-
        sub_univ_m_d_ref$target_weights / sum(sub_univ_m_d_ref$target_weights)
    }

    sub_concentration_constraint_pol <- concentration_constraint_policy
    sub_concentration_constraint_pol$max_abs_active_individual_weight <-
      sub_concentration_constraint_pol$max_abs_active_individual_weight / group_weights[group]

    sub_liquidity_constraint_pol <- liquidity_constraint_policy
    sub_liquidity_constraint_pol$liquidity_cap_rules <-
      sub_liquidity_constraint_pol$liquidity_cap_rules / group_weights[group]


    ## Set micro weights
    micro_port_list[[g]] <- set_portfolio_weights(
      universe_m_d_ref = sub_univ_m_d_ref,
      port_construction_method = "mvo",
      liquidity_constraint_policy = sub_liquidity_constraint_pol,
      liquidity_m_d_ref = sub_liquidity_m_d_ref,
      concentration_constraint_policy = sub_concentration_constraint_pol,
      turnover_constraint_policy = NULL,
      groups_m_d_ref = NULL,
      covariance_matrix = sub_cov_matrix,
      cap_weighting_metric = "mean_volfin_3m",
      ridge_pen = 0.5,
      n_resamples = 3,
      exp_ret_score_jitter = 0.2,
      cov_eigval_jitter = 0.01,
      opt_objective = "risk",
      n_random_ports = 500
    )

  }

  # Add a 'NULL' element for all groups with zero weight
  expected_results <- list(
    `Doméstico Defensivo` = NULL,
    Exportador = micro_port_list[[2]],
    `Doméstico Cíclico` = NULL,
    Indústria = NULL
  )

  ## Do the same with the function
  set.seed(123)
  results <- process_micro_portfolios(
    parallel = FALSE,
    groups = groups,
    group_members = group_members,
    group_weights = group_weights,
    micro_port_construction_method = "mvo",
    cap_weighting_metric = "mean_volfin_3m",
    universe_m_d_ref = elig_universe_m_d_ref,
    covariance_matrix = covariance_matrix,
    liquidity_m_d_ref = liquidity_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    liquidity_constraint_policy = liquidity_constraint_policy,
    ridge_pen = 0.5,
    n_resamples = 3,
    exp_ret_score_jitter = 0.2,
    cov_eigval_jitter = 0.01,
    opt_objective = "risk",
    n_random_ports = 500
  )

  ## Test that constraints are satisfied at the final portfolio-level
  for (group in groups){

    if (group != "Exportador") {
      testthat::expect_null(results[[group]])
      next
    }

    group_port <- results[[group]]@universe_m_d_ref@data
    ## Concentration constraints
    max_weights <- elig_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_port$tickers) %>%
      dplyr::pull(ibov_bench_weights) + concentration_constraint_policy$max_abs_active_individual_weight
    min_weights <- pmax(elig_universe_m_d_ref %>%
                          dplyr::filter(tickers %in% group_port$tickers) %>%
                          dplyr::pull(ibov_bench_weights) - concentration_constraint_policy$max_abs_active_individual_weight,
                        0)

    testthat::expect_true(all(group_port$weights * group_weights[group] <= max_weights))
    testthat::expect_true(all(group_port$weights * group_weights[group] >= min_weights))

    ## Liquidity constraints
    liquidity_caps <- liquidity_constraint_policy$liquidity_cap_rules
    for (cap in names(liquidity_caps)){
      testthat::expect_true(
        all(group_port %>%
              dplyr::filter(liquidity_classification == cap) %>%
              dplyr::pull(weights) * group_weights[group] <= liquidity_caps[cap]
        )
      )
    }
  }

  # Test that there are as many micro portfolios as groups
  testthat::expect_equal(length(results), length(groups))

  # Test that all groups are contained in results
  testthat::expect_true(all(groups %in% names(results)))

  # Test that each micro portfolio is of S4 class "port", except the last one
  testthat::expect_true(all(sapply(results[2], function(x) inherits(x, "port"))))

  # Test that weights sum to 1 in each micro portfolio
  testthat::expect_true(all(sapply(results[2], function(x) abs(sum(x@universe_m_d_ref@data$weights) - 1) < 1e-6)))

  # Test that all tickers in each micro portfolio belong to the correct group
  testthat::expect_true(all(sapply(names(results)[2], function(g) {
    all(results[[g]]@universe_m_d_ref@data$tickers %in% group_members[[g]])
  })))

  # Test that tickers in each micro portfolio are part of the eligible universe
  testthat::expect_true(all(sapply(names(results)[2], function(g) {
    all(results[[g]]@universe_m_d_ref@data$tickers %in% elig_universe_m_d_ref$tickers)
  })))

  # Test that results and expected results match
  testthat::expect_equal(results, expected_results)


  # Define group weights ad hoc (Exportador 99% weight, Domestico Defensivo 1%)
  # This causes one single stock to be > 100%
  group_weights <- c(0.01, 0.99, 0, 0)
  names(group_weights) <- groups


  ## For each group
  micro_port_list <- list()
  set.seed(123)
  for (g in seq_along(groups)){

    group <- groups[g]

    if (g %in% c(3,4)) next

    ## Get members
    group_tickers <- elig_universe_m_d_ref %>%
      dplyr::filter(macro_sector == group) %>%
      dplyr::pull(tickers)
    sub_univ_m_d_ref <- elig_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

    ## Get covariance matrix
    sub_cov_matrix <- covariance_matrix[group_tickers, group_tickers, drop = FALSE]

    ## Get liquidity m_df
    sub_liquidity_m_d_ref <- liquidity_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

    ## Rescale weights
    sub_univ_m_d_ref$ibov_bench_weights <-
      sub_univ_m_d_ref$ibov_bench_weights / group_weights[group]
    if (sum(sub_univ_m_d_ref$ibov_bench_weights) > 1){
      sub_univ_m_d_ref$ibov_bench_weights <-
        sub_univ_m_d_ref$ibov_bench_weights / sum(sub_univ_m_d_ref$ibov_bench_weights)
    }

    sub_univ_m_d_ref$target_weights <-
      sub_univ_m_d_ref$target_weights / group_weights[group]
    if (sum(sub_univ_m_d_ref$target_weights) > 1){
      sub_univ_m_d_ref$target_weights <-
        sub_univ_m_d_ref$target_weights / sum(sub_univ_m_d_ref$target_weights)
    }

    sub_concentration_constraint_pol <- concentration_constraint_policy
    sub_concentration_constraint_pol$max_abs_active_individual_weight <-
      sub_concentration_constraint_pol$max_abs_active_individual_weight / group_weights[group]

    sub_liquidity_constraint_pol <- liquidity_constraint_policy
    sub_liquidity_constraint_pol$liquidity_cap_rules <-
      sub_liquidity_constraint_pol$liquidity_cap_rules / group_weights[group]


    ## Set micro weights
    micro_port_list[[g]] <- set_portfolio_weights(
      universe_m_d_ref = sub_univ_m_d_ref,
      port_construction_method = "mvo",
      liquidity_constraint_policy = sub_liquidity_constraint_pol,
      liquidity_m_d_ref = sub_liquidity_m_d_ref,
      concentration_constraint_policy = sub_concentration_constraint_pol,
      turnover_constraint_policy = NULL,
      groups_m_d_ref = NULL,
      covariance_matrix = sub_cov_matrix,
      cap_weighting_metric = "mean_volfin_3m",
      ridge_pen = 0.5,
      n_resamples = 3,
      exp_ret_score_jitter = 0.2,
      cov_eigval_jitter = 0.01,
      opt_objective = "risk",
      n_random_ports = 500
    )

  }

  # Add a 'NULL' element for all groups with zero weight
  expected_results <- list(
    `Doméstico Defensivo` = micro_port_list[[1]],
    Exportador = micro_port_list[[2]],
    `Doméstico Cíclico` = NULL,
    Indústria = NULL
  )

  ## Do the same with the function
  set.seed(123)
  results <- process_micro_portfolios(
    parallel = FALSE,
    groups = groups,
    group_members = group_members,
    group_weights = group_weights,
    micro_port_construction_method = "mvo",
    cap_weighting_metric = "mean_volfin_3m",
    universe_m_d_ref = elig_universe_m_d_ref,
    covariance_matrix = covariance_matrix,
    liquidity_m_d_ref = liquidity_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    liquidity_constraint_policy = liquidity_constraint_policy,
    ridge_pen = 0.5,
    n_resamples = 3,
    exp_ret_score_jitter = 0.2,
    cov_eigval_jitter = 0.01,
    opt_objective = "risk",
    n_random_ports = 500
  )

  ## Test that only liquidity constraints are satisfied at the final portfolio-level
  for (group in groups){

    if (!group %in% c("Doméstico Defensivo", "Exportador")) {
      testthat::expect_null(results[[group]])
      next
    }

    group_port <- results[[group]]@universe_m_d_ref@data
    ## Concentration constraints
    #max_weights <- elig_universe_m_d_ref %>%
    #  dplyr::filter(tickers %in% group_port$tickers) %>%
    #  dplyr::pull(ibov_bench_weights) + concentration_constraint_policy$max_abs_active_individual_weight
    #min_weights <- pmax(elig_universe_m_d_ref %>%
    #                      dplyr::filter(tickers %in% group_port$tickers) %>%
    #                      dplyr::pull(ibov_bench_weights) - concentration_constraint_policy$max_abs_active_individual_weight,
    #                    0)

    #testthat::expect_true(all(group_port$weights * group_weights[group] <= max_weights))
    #testthat::expect_true(all(group_port$weights * group_weights[group] >= min_weights))

    ## Liquidity constraints
    liquidity_caps <- liquidity_constraint_policy$liquidity_cap_rules
    for (cap in names(liquidity_caps)){
      testthat::expect_true(
        all(group_port %>%
              dplyr::filter(liquidity_classification == cap) %>%
              dplyr::pull(weights) * group_weights[group] <= liquidity_caps[cap]
        )
      )
    }
  }

  # Test that there are as many micro portfolios as groups
  testthat::expect_equal(length(results), length(groups))

  # Test that all groups are contained in results
  testthat::expect_true(all(groups %in% names(results)))

  # Test that each micro portfolio is of S4 class "port", except the last one
  testthat::expect_true(all(sapply(results[c(1,2)], function(x) inherits(x, "port"))))

  # Test that weights sum to 1 in each micro portfolio
  testthat::expect_true(all(sapply(results[c(1,2)], function(x) abs(sum(x@universe_m_d_ref@data$weights) - 1) < 1e-6)))

  # Test that all tickers in each micro portfolio belong to the correct group
  testthat::expect_true(all(sapply(names(results)[c(1,2)], function(g) {
    all(results[[g]]@universe_m_d_ref@data$tickers %in% group_members[[g]])
  })))

  # Test that tickers in each micro portfolio are part of the eligible universe
  testthat::expect_true(all(sapply(names(results)[c(1,2)], function(g) {
    all(results[[g]]@universe_m_d_ref@data$tickers %in% elig_universe_m_d_ref$tickers)
  })))

  # Test that results and expected results match
  testthat::expect_equal(results, expected_results)

})

