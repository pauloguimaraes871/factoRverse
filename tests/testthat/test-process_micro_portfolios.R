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
    ## Get eligible stock universe
    eligible_universe_m_d_ref <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)
    eligible_tickers <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
    selected_daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[
      which(zoo::index(daily_stock_returns_m_xts) <= current_date), eligible_tickers]
    bench_assets_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[
      which(zoo::index(daily_stock_returns_m_xts) <= current_date),
      benchmark_weights_m_d_ref %>% dplyr::filter(ibov > 0) %>% dplyr::pull(tickers)
      ]

  #Set top down micro weights

    ## Get groups
    groups <- stock_universe_m_d_ref$macro_sector %>% unique()
    group_members <- lapply(groups, function(g) {
      stock_universe_m_d_ref %>%
        dplyr::filter(macro_sector == g) %>%
        dplyr::pull(tickers)
    })
    names(group_members) <- groups


    ## For each group
    micro_port_list <- list()
    for (g in seq_along(groups)){

      group <- groups[g]

      ## Get members
      group_tickers <- stock_universe_m_d_ref %>%
        dplyr::filter(macro_sector == group) %>%
        dplyr::pull(tickers)
      sub_universe_m_d_ref <- stock_universe_m_d_ref %>%
        dplyr::filter(tickers %in% group_tickers)

      ## Get liquidity m_df
      sub_liquidity_m_d_ref <- liquidity_m_d_ref %>%
        dplyr::filter(tickers %in% group_tickers)

      sub_selected_daily_stock_returns_m_xts_upd_ref <- selected_daily_stock_returns_m_xts_upd_ref[
        ,sub_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
      ]

      ## Defensively remove weight cols
      sub_universe_m_d_ref$ibov_bench_weights <- NULL
      sub_universe_m_d_ref$target_weights <- NULL

      ## Set micro weights
      micro_port_list[[g]] <- set_portfolio_weights(
        universe_m_d_ref = sub_universe_m_d_ref,
        port_construction_method = "rp",
        liquidity_constraint_policy = NULL,
        liquidity_m_d_ref = sub_liquidity_m_d_ref,
        eligible_returns_m_xts_upd_ref = sub_selected_daily_stock_returns_m_xts_upd_ref,
        cov_matrix_sample_size = 60,
        cov_estimation_method = "cc",
        active_returns = FALSE,
        concentration_constraint_policy = NULL,
        turnover_constraint_policy = NULL,
        groups_m_d_ref = stock_groups_m_d_ref,
        level = "sub_port"
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
      universe_m_d_ref = stock_universe_m_d_ref,
      eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
      cov_matrix_sample_size = 60,
      cov_estimation_method = "cc",
      active_returns = FALSE,
      groups_m_d_ref = stock_groups_m_d_ref,
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

    # Test that tickers in each micro portfolio obey eligibility
    testthat::expect_true(all(sapply(names(results), function(g) {
      all(
        all(results[[g]]@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 0) %>% dplyr::pull(weights) == 0)
        )
    })))

    # Test that port stats follow expected structure
    testthat::expect_true(
      all(
        purrr::map_lgl(
          names(results), function(g){
            results[[g]]@port_stats %>% dplyr::select(dplyr::contains("act_")) %>% ncol() == 0
          }
        )
      )
    )
    testthat::expect_equal(
      results$`Doméstico Cíclico`@port_stats,
      calculate_port_stats(
        universe_m_d_ref = results$`Doméstico Cíclico`@universe_m_d_ref@data,
        all_returns_m_xts_upd_ref = sub_selected_daily_stock_returns_m_xts_upd_ref,
        cov_matrix_sample_size = 60,
        cov_estimation_method = "cc",
        groups_m_d_ref = stock_groups_m_d_ref
      )$port_stats
    )


    # Test that results and expected results match
    testthat::expect_equal(results, expected_results)


})

test_that("set top down micro weights works without group weights for simple HRP case in a top_down_proxy context", {

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
  selected_daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[
    which(zoo::index(daily_stock_returns_m_xts) <= current_date),
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
    ]
  eligible_tickers <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)


  #Set top down micro weights

  ## Get eligible stock universe
  eligible_universe_m_d_ref <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)

  ## Get groups
  groups <- stock_universe_m_d_ref$macro_sector %>% unique()
  group_members <- lapply(groups, function(g) {
    stock_universe_m_d_ref %>%
      dplyr::filter(macro_sector == g) %>%
      dplyr::pull(tickers)
  })
  names(group_members) <- groups


  ## For each group
  micro_port_list <- list()
  for (g in seq_along(groups)){

    group <- groups[g]

    ## Get members
    group_tickers <- stock_universe_m_d_ref %>%
      dplyr::filter(macro_sector == group) %>%
      dplyr::pull(tickers)
    sub_universe_m_d_ref <- stock_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

    ## Get liquidity m_df
    sub_liquidity_m_d_ref <- liquidity_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

    ## Defensively remove weight cols
    sub_universe_m_d_ref$ibov_bench_weights <- NULL
    sub_universe_m_d_ref$target_weights <- NULL

    ## Set micro weights
    micro_port_list[[g]] <- set_portfolio_weights(
      universe_m_d_ref = sub_universe_m_d_ref,
      port_construction_method = "hrp",
      linkage = "single",
      liquidity_constraint_policy = NULL,
      liquidity_m_d_ref = sub_liquidity_m_d_ref,
      concentration_constraint_policy = NULL,
      turnover_constraint_policy = NULL,
      eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref[
        ,sub_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
      ],
      cov_matrix_sample_size = 60,
      cov_estimation_method = "cc",
      active_returns = FALSE,
      groups_m_d_ref = stock_groups_m_d_ref,
      level = "sub_port"
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
    micro_port_construction_method = "hrp",
    linkage = "single",
    universe_m_d_ref = stock_universe_m_d_ref,
    eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
    cov_matrix_sample_size = 60,
    cov_estimation_method = "cc",
    active_returns = FALSE,
    groups_m_d_ref = stock_groups_m_d_ref,
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

  # Test that both ineligible and eligible tickers exist, but only eligible have weights
  testthat::expect_true(all(sapply(names(results), function(g) {
    all(results[[g]]@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 0) %>% dplyr::pull(weights) == 0)
  })))
  testthat::expect_true(all(sapply(names(results), function(g) {
    all(results[[g]]@universe_m_d_ref@data %>% dplyr::pull(is_eligible) %>% unique() %in% c(0,1))
  })))

  # Test that port stats follow expected structure
  testthat::expect_true(
    all(
      purrr::map_lgl(
        names(results), function(g){
          results[[g]]@port_stats %>% dplyr::select(dplyr::contains("act_")) %>% ncol() == 0
        }
      )
    )
  )
  testthat::expect_true(
    all(
      purrr::map_lgl(
        names(results), function(g){
          results[[g]]@port_stats %>% dplyr::select(dplyr::contains("group_")) %>% ncol() == 0
        }
      )
    )
  )
  testthat::expect_equal(
    results$`Doméstico Cíclico`@port_stats,
    calculate_port_stats(
      universe_m_d_ref = results$`Doméstico Cíclico`@universe_m_d_ref@data,
      all_returns_m_xts_upd_ref =  selected_daily_stock_returns_m_xts_upd_ref[
        ,sub_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
      ],
      cov_matrix_sample_size = 60,
      cov_estimation_method = "cc",
      groups_m_d_ref = stock_groups_m_d_ref
    )$port_stats
  )

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
  selected_daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[
    which(zoo::index(daily_stock_returns_m_xts) <= current_date),
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
    ]
  eligible_tickers <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  #Set top down micro weights

  ## Get eligible stock universe
  eligible_universe_m_d_ref <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)

  ## Get groups
  groups <- eligible_universe_m_d_ref$macro_sector %>% unique()
  group_members <- lapply(groups, function(g) {
    stock_universe_m_d_ref %>%
      dplyr::filter(macro_sector == g) %>%
      dplyr::pull(tickers)
  })
  names(group_members) <- groups

  ## For each group
  micro_port_list <- list()
  for (g in seq_along(groups)){

    group <- groups[g]

    ## Get members
    group_tickers <- stock_universe_m_d_ref %>%
      dplyr::filter(macro_sector == group) %>%
      dplyr::pull(tickers)
    sub_universe_m_d_ref <- stock_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)


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
      eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref[
        ,sub_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
      ],
      cov_matrix_sample_size = 60,
      cov_estimation_method = "cc",
      active_returns = FALSE,
      groups_m_d_ref = stock_groups_m_d_ref,
      level = "sub_port",
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
    universe_m_d_ref = stock_universe_m_d_ref,
    eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
    cov_matrix_sample_size = 60,
    cov_estimation_method = "cc",
    active_returns = FALSE,
    groups_m_d_ref = stock_groups_m_d_ref,
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

  # Test that both ineligible and eligible tickers exist, but only eligible have weights
  testthat::expect_true(all(sapply(names(results), function(g) {
    all(results[[g]]@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 0) %>% dplyr::pull(weights) == 0)
  })))
  testthat::expect_true(all(sapply(names(results), function(g) {
    all(results[[g]]@universe_m_d_ref@data %>% dplyr::pull(is_eligible) %>% unique() %in% c(0,1))
  })))

  # Test that port stats follow expected structure
  testthat::expect_true(
    all(
      purrr::map_lgl(
        names(results), function(g){
          results[[g]]@port_stats %>% dplyr::select(dplyr::contains("act_")) %>% ncol() == 0
        }
      )
    )
  )
  testthat::expect_true(
    all(
      purrr::map_lgl(
        names(results), function(g){
          results[[g]]@port_stats %>% dplyr::select(dplyr::contains("group_")) %>% ncol() == 0
        }
      )
    )
  )
  testthat::expect_equal(
    results$`Indústria`@port_stats,
    calculate_port_stats(
      universe_m_d_ref = results$`Indústria`@universe_m_d_ref@data,
      all_returns_m_xts_upd_ref =  selected_daily_stock_returns_m_xts_upd_ref[
        ,sub_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
      ],
      cov_matrix_sample_size = 60,
      cov_estimation_method = "cc",
      groups_m_d_ref = stock_groups_m_d_ref
    )$port_stats
  )

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
  n_resamples <- 2
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
  elig_tickers <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]
  selected_daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[
    which(zoo::index(daily_stock_returns_m_xts) <= current_date),
    stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
    ]
  bench_assets_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[
    which(zoo::index(daily_stock_returns_m_xts) <= current_date),
    benchmark_weights_m_d_ref %>% dplyr::filter(ibov > 0) %>% dplyr::pull(tickers)
    ]

  #Set top down micro weights

  ## Get groups
  groups <- stock_universe_m_d_ref$macro_sector %>% unique()
  group_members <- lapply(groups, function(g) {
    stock_universe_m_d_ref %>%
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
    group_tickers <- stock_universe_m_d_ref %>%
      dplyr::filter(macro_sector == group) %>%
      dplyr::pull(tickers)
    sub_univ_m_d_ref <- stock_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

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
      groups_m_d_ref = stock_groups_m_d_ref,
      cap_weighting_metric = "mean_volfin_3m",
      eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref[
        ,sub_univ_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
      ],
      bench_assets_returns_m_xts_upd_ref = bench_assets_returns_m_xts_upd_ref,
      selected_benchmark = "ibov",
      cov_matrix_sample_size = 60,
      cov_estimation_method = "cc",
      active_returns = FALSE,
      level = "sub_port",
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
  testthat::expect_warning(
  testthat::expect_warning(
  testthat::expect_warning(
  testthat::expect_warning(
  results <- process_micro_portfolios(
    parallel = FALSE,
    groups = groups,
    group_members = group_members,
    group_weights = group_weights,
    micro_port_construction_method = "mvo",
    cap_weighting_metric = "mean_volfin_3m",
    universe_m_d_ref = stock_universe_m_d_ref,
    eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
    bench_assets_returns_m_xts_upd_ref = bench_assets_returns_m_xts_upd_ref,
    selected_benchmark = "ibov",
    cov_matrix_sample_size = 60,
    cov_estimation_method = "cc",
    active_returns = FALSE,
    groups_m_d_ref = stock_groups_m_d_ref,
    liquidity_m_d_ref = liquidity_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    liquidity_constraint_policy = liquidity_constraint_policy,
    ridge_pen = 0.5,
    n_resamples = 3,
    exp_ret_score_jitter = 0.2,
    cov_eigval_jitter = 0.01,
    opt_objective = "risk",
    n_random_ports = 500
  ),
  "For concentration constraint: after scaling, ibov_bench_weights in group Exportador sums to more than 1. Normalizing to sum to 1.This might indicate that overall constraints do not hold because of this group."
  ),
 "For target weights: after scaling, target_weights in group Exportador sums to more than 1. Normalizing to sum to 1.This might indicate that overall constraints do not hold because of this group."
  ),
 "For concentration constraint: after scaling, ibov_bench_weights in group Doméstico Cíclico sums to more than 1. Normalizing to sum to 1.This might indicate that overall constraints do not hold because of this group."
  ),
 "For target weights: after scaling, target_weights in group Doméstico Cíclico sums to more than 1. Normalizing to sum to 1.This might indicate that overall constraints do not hold because of this group."
  )

  ## Test that constraints are satisfied at the final portfolio-level
  for (group in groups){
    group_port <- results[[group]]@universe_m_d_ref@data
    ## Concentration constraints
    max_weights <- stock_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_port$tickers) %>%
      dplyr::pull(ibov_bench_weights) + concentration_constraint_policy$max_abs_active_individual_weight
    min_weights <- pmax(stock_universe_m_d_ref %>%
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

  # Test that both ineligible and eligible tickers exist, but only eligible have weights
  testthat::expect_true(all(sapply(names(results), function(g) {
    all(results[[g]]@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 0) %>% dplyr::pull(weights) == 0)
  })))
  testthat::expect_true(all(sapply(names(results), function(g) {
    all(results[[g]]@universe_m_d_ref@data %>% dplyr::pull(is_eligible) %>% unique() %in% c(0,1))
  })))


  # Test that port stats follow expected structure
  testthat::expect_true(
    all(
      purrr::map_lgl(
        names(results), function(g){
          results[[g]]@port_stats %>% dplyr::select(dplyr::contains("act_")) %>% ncol() > 0
        }
      )
    )
  )
  testthat::expect_true(
    all(
      purrr::map_lgl(
        names(results), function(g){
          results[[g]]@port_stats %>% dplyr::select(dplyr::contains("group_")) %>% ncol() == 0
        }
      )
    )
  )

  # Test that benchmark portfolio was correctly produced
  bench_weights_m_d_ref <- sub_univ_m_d_ref %>%
    dplyr::select(id, tickers, dates, ibov_bench_weights) %>%
    dplyr::rename(weights = ibov_bench_weights)

  bench_universe_m_d_ref <- sub_univ_m_d_ref %>%
    dplyr::left_join(bench_weights_m_d_ref %>% dplyr::select(id, weights), by = "id") %>%
    dplyr::mutate(is_eligible = ifelse(weights > 0, 1, 0))


  all_tickers <- results$`Doméstico Cíclico`@universe_m_d_ref@data %>%
    dplyr::filter(is_eligible == 1) %>%
    dplyr::pull(tickers)
  all_tickers <- dplyr::union(all_tickers,
                              bench_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  expected_port_stats <- calculate_port_stats(
    universe_m_d_ref = results$`Doméstico Cíclico`@universe_m_d_ref@data,
    all_returns_m_xts_upd_ref =  daily_stock_returns_m_xts_upd_ref[,all_tickers],
    bench_universe_m_d_ref = bench_universe_m_d_ref,
    selected_benchmark = "ibov",
    cov_matrix_sample_size = 60,
    cov_estimation_method = "cc",
    groups_m_d_ref = stock_groups_m_d_ref
  )

  testthat::expect_equal(
    results$`Doméstico Cíclico`@port_stats,
    expected_port_stats$port_stats
  )

  expect_equal(
    expected_port_stats$assets_stats$weights,
    results$`Doméstico Cíclico`@universe_m_d_ref@data %>%
      dplyr::filter(tickers %in% expected_port_stats$assets_stats$tickers) %>%
      dplyr::pull(act_weights)
  )

  # Test that results and expected results match
  testthat::expect_equal(results, expected_results)


  # Define group weights ad hoc (Indústria  with zero weight)
  group_weights <- c(0.5, 0, 0.3, 0.2)
  names(group_weights) <- groups


  ## For each group
  micro_port_list <- list()
  set.seed(123)
  for (g in seq_along(groups)){

    group <- groups[g]

    if (group == "Indústria") next

    ## Get members
    group_tickers <- stock_universe_m_d_ref %>%
      dplyr::filter(macro_sector == group) %>%
      dplyr::pull(tickers)
    sub_univ_m_d_ref <- stock_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

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
      cap_weighting_metric = "mean_volfin_3m",
      eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref[
        ,sub_univ_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
      ],
      bench_assets_returns_m_xts_upd_ref = bench_assets_returns_m_xts_upd_ref,
      selected_benchmark = "ibov",
      cov_matrix_sample_size = 60,
      cov_estimation_method = "cc",
      active_returns = FALSE,
      level = "sub_port",
      groups_m_d_ref = stock_groups_m_d_ref,
      ridge_pen = 0.5,
      n_resamples = 3,
      exp_ret_score_jitter = 0.2,
      cov_eigval_jitter = 0.01,
      opt_objective = "risk",
      n_random_ports = 500
    )

  }

  # Add a 'NULL' element for the group with zero weight
  names(micro_port_list) <- groups
  expected_results <- micro_port_list

  ## Do the same with the function
  set.seed(123)
  testthat::expect_warning(
  testthat::expect_warning(
  testthat::expect_warning(
  testthat::expect_warning(
    results <- process_micro_portfolios(
      parallel = FALSE,
      groups = groups,
      group_members = group_members,
      group_weights = group_weights,
      micro_port_construction_method = "mvo",
      cap_weighting_metric = "mean_volfin_3m",
      universe_m_d_ref = stock_universe_m_d_ref,
      eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
      bench_assets_returns_m_xts_upd_ref = bench_assets_returns_m_xts_upd_ref,
      selected_benchmark = "ibov",
      cov_matrix_sample_size = 60,
      cov_estimation_method = "cc",
      active_returns = FALSE,
      groups_m_d_ref = stock_groups_m_d_ref,
      liquidity_m_d_ref = liquidity_m_d_ref,
      concentration_constraint_policy = concentration_constraint_policy,
      liquidity_constraint_policy = liquidity_constraint_policy,
      ridge_pen = 0.5,
      n_resamples = 3,
      exp_ret_score_jitter = 0.2,
      cov_eigval_jitter = 0.01,
      opt_objective = "risk",
      n_random_ports = 500
  ),
    "For concentration constraint: after scaling, ibov_bench_weights in group Exportador sums to more than 1. Normalizing to sum to 1.This might indicate that overall constraints do not hold because of this group."
  ),
  "For target weights: after scaling, target_weights in group Exportador sums to more than 1. Normalizing to sum to 1.This might indicate that overall constraints do not hold because of this group."
  ),
  "For concentration constraint: after scaling, ibov_bench_weights in group Doméstico Cíclico sums to more than 1. Normalizing to sum to 1.This might indicate that overall constraints do not hold because of this group."
  ),
  "For target weights: after scaling, target_weights in group Doméstico Cíclico sums to more than 1. Normalizing to sum to 1.This might indicate that overall constraints do not hold because of this group."
  )


  ## Test that constraints are satisfied at the final portfolio-level
  for (group in groups){

    if (group == "Indústria") {
      testthat::expect_null(results[[group]])
      next
    }

    group_port <- results[[group]]@universe_m_d_ref@data
    ## Concentration constraints
    max_weights <- stock_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_port$tickers) %>%
      dplyr::pull(ibov_bench_weights) + concentration_constraint_policy$max_abs_active_individual_weight
    min_weights <- pmax(stock_universe_m_d_ref %>%
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
  testthat::expect_true(all(sapply(results[-2], function(x) inherits(x, "port"))))

  # Test that weights sum to 1 in each micro portfolio
  testthat::expect_true(all(sapply(results[-2], function(x) abs(sum(x@universe_m_d_ref@data$weights) - 1) < 1e-6)))

  # Test that all tickers in each micro portfolio belong to the correct group
  testthat::expect_true(all(sapply(names(results)[-2], function(g) {
    all(results[[g]]@universe_m_d_ref@data$tickers %in% group_members[[g]])
  })))

  # Test that both ineligible and eligible tickers exist, but only eligible have weights
  testthat::expect_true(all(sapply(names(results)[-2], function(g) {
    all(results[[g]]@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 0) %>% dplyr::pull(weights) == 0)
  })))
  testthat::expect_true(all(sapply(names(results)[-2], function(g) {
    all(results[[g]]@universe_m_d_ref@data %>% dplyr::pull(is_eligible) %>% unique() %in% c(0,1))
  })))

  # Test that port stats follow expected structure
  testthat::expect_true(
    all(
      purrr::map_lgl(
        names(results)[-2], function(g){
          results[[g]]@port_stats %>% dplyr::select(dplyr::contains("act_")) %>% ncol() > 0
        }
      )
    )
  )
  testthat::expect_true(
    all(
      purrr::map_lgl(
        names(results)[-2], function(g){
          results[[g]]@port_stats %>% dplyr::select(dplyr::contains("group_")) %>% ncol() == 0
        }
      )
    )
  )

  # Test that benchmark portfolio was correctly produced
  bench_weights_m_d_ref <- sub_univ_m_d_ref %>%
    dplyr::select(id, tickers, dates, ibov_bench_weights) %>%
    dplyr::rename(weights = ibov_bench_weights)

  bench_universe_m_d_ref <- sub_univ_m_d_ref %>%
    dplyr::left_join(bench_weights_m_d_ref %>% dplyr::select(id, weights), by = "id") %>%
    dplyr::mutate(is_eligible = ifelse(weights > 0, 1, 0))


  all_tickers <- results$`Doméstico Cíclico`@universe_m_d_ref@data %>%
    dplyr::filter(is_eligible == 1) %>%
    dplyr::pull(tickers)
  all_tickers <- dplyr::union(all_tickers,
                              bench_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers))

  expected_port_stats <- calculate_port_stats(
    universe_m_d_ref = results$`Doméstico Cíclico`@universe_m_d_ref@data,
    all_returns_m_xts_upd_ref =  daily_stock_returns_m_xts_upd_ref[,all_tickers],
    bench_universe_m_d_ref = bench_universe_m_d_ref,
    selected_benchmark = "ibov",
    cov_matrix_sample_size = 60,
    cov_estimation_method = "cc",
    groups_m_d_ref = stock_groups_m_d_ref
  )

  testthat::expect_equal(
    results$`Doméstico Cíclico`@port_stats,
    expected_port_stats$port_stats
  )

  expect_equal(
    expected_port_stats$assets_stats$weights,
    results$`Doméstico Cíclico`@universe_m_d_ref@data %>%
      dplyr::filter(tickers %in% expected_port_stats$assets_stats$tickers) %>%
      dplyr::pull(act_weights)
  )

  # Test that results and expected results match
  testthat::expect_equal(results, expected_results)


  # Define group weights ad hoc (Exportador 100% weight)
  group_weights <- c(0, 0, 1, 0)
  names(group_weights) <- groups


  ## For each group
  micro_port_list <- list()
  set.seed(123)
  for (g in seq_along(groups)){

    group <- groups[g]

    if (group != "Exportador") next

    ## Get members
    group_tickers <- stock_universe_m_d_ref %>%
      dplyr::filter(macro_sector == group) %>%
      dplyr::pull(tickers)
    sub_univ_m_d_ref <- stock_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

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
      cap_weighting_metric = "mean_volfin_3m",
      eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref[
        ,sub_univ_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
      ],
      bench_assets_returns_m_xts_upd_ref = bench_assets_returns_m_xts_upd_ref,
      selected_benchmark = "ibov",
      cov_matrix_sample_size = 60,
      cov_estimation_method = "cc",
      active_returns = FALSE,
      level = "sub_port",
      groups_m_d_ref = stock_groups_m_d_ref,
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
    Indústria = NULL,
    Exportador = micro_port_list[[3]],
    `Doméstico Cíclico` = NULL
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
    universe_m_d_ref = stock_universe_m_d_ref,
    liquidity_m_d_ref = liquidity_m_d_ref,
    concentration_constraint_policy = concentration_constraint_policy,
    liquidity_constraint_policy = liquidity_constraint_policy,
    eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
    bench_assets_returns_m_xts_upd_ref = bench_assets_returns_m_xts_upd_ref,
    selected_benchmark = "ibov",
    cov_matrix_sample_size = 60,
    cov_estimation_method = "cc",
    active_returns = FALSE,
    groups_m_d_ref = stock_groups_m_d_ref,
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
    max_weights <- stock_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_port$tickers) %>%
      dplyr::pull(ibov_bench_weights) + concentration_constraint_policy$max_abs_active_individual_weight
    min_weights <- pmax(stock_universe_m_d_ref %>%
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
  testthat::expect_true(all(sapply(results[3], function(x) inherits(x, "port"))))

  # Test that weights sum to 1 in each micro portfolio
  testthat::expect_true(all(sapply(results[3], function(x) abs(sum(x@universe_m_d_ref@data$weights) - 1) < 1e-6)))

  # Test that all tickers in each micro portfolio belong to the correct group
  testthat::expect_true(all(sapply(names(results)[3], function(g) {
    all(results[[g]]@universe_m_d_ref@data$tickers %in% group_members[[g]])
  })))

  # Test that results and expected results match
  testthat::expect_equal(results, expected_results)


  # Define group weights ad hoc (Exportador 99% weight, Domestico Defensivo 1%,
  # Indústria < Machine$douple.eps )
  # This causes one single stock to be > 100%
  group_weights <- c(0.01, .Machine$double.eps / 10, 0.99, 0)
  names(group_weights) <- groups


  ## For each group
  micro_port_list <- list()
  set.seed(123)
  for (g in seq_along(groups)){

    group <- groups[g]

    if (g %in% c(2,4)) next

    ## Get members
    group_tickers <- stock_universe_m_d_ref %>%
      dplyr::filter(macro_sector == group) %>%
      dplyr::pull(tickers)
    sub_univ_m_d_ref <- stock_universe_m_d_ref %>%
      dplyr::filter(tickers %in% group_tickers)

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
      cap_weighting_metric = "mean_volfin_3m",
      eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref[
        ,sub_univ_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
      ],
      bench_assets_returns_m_xts_upd_ref = bench_assets_returns_m_xts_upd_ref,
      selected_benchmark = "ibov",
      cov_matrix_sample_size = 60,
      cov_estimation_method = "cc",
      active_returns = FALSE,
      level = "sub_port",
      groups_m_d_ref = stock_groups_m_d_ref,
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
    Indústria = NULL,
    Exportador = micro_port_list[[3]],
    `Doméstico Cíclico` = NULL
  )

  ## Do the same with the function
  set.seed(123)
  testthat::expect_warning(
  testthat::expect_warning(
    results <- process_micro_portfolios(
      parallel = FALSE,
      groups = groups,
      group_members = group_members,
      group_weights = group_weights,
      micro_port_construction_method = "mvo",
      cap_weighting_metric = "mean_volfin_3m",
      universe_m_d_ref = stock_universe_m_d_ref,
      liquidity_m_d_ref = liquidity_m_d_ref,
      concentration_constraint_policy = concentration_constraint_policy,
      liquidity_constraint_policy = liquidity_constraint_policy,
      eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
      bench_assets_returns_m_xts_upd_ref = bench_assets_returns_m_xts_upd_ref,
      selected_benchmark = "ibov",
      cov_matrix_sample_size = 60,
      cov_estimation_method = "cc",
      active_returns = FALSE,
      groups_m_d_ref = stock_groups_m_d_ref,
      ridge_pen = 0.5,
      n_resamples = 3,
      exp_ret_score_jitter = 0.2,
      cov_eigval_jitter = 0.01,
      opt_objective = "risk",
      n_random_ports = 500
    )))


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
  testthat::expect_true(all(sapply(results[c(1,3)], function(x) inherits(x, "port"))))

  # Test that weights sum to 1 in each micro portfolio
  testthat::expect_true(all(sapply(results[c(1,3)], function(x) abs(sum(x@universe_m_d_ref@data$weights) - 1) < 1e-6)))

  # Test that all tickers in each micro portfolio belong to the correct group
  testthat::expect_true(all(sapply(names(results)[c(1,3)], function(g) {
    all(results[[g]]@universe_m_d_ref@data$tickers %in% group_members[[g]])
  })))

  # Test that results and expected results match
  testthat::expect_equal(results, expected_results)

})

testthat::test_that("group_members order does not affect results", {

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
  n_resamples <- 2
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
  selected_daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts_upd_ref[, elig_tickers]


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


  # Shuffle tickers inside each group's member list
  shuffled <- group_members
  shuffled <- lapply(shuffled, sample)

  res1 <- process_micro_portfolios(
    parallel = FALSE, groups = groups, group_members = group_members,
    group_weights = NULL, micro_port_construction_method = "rp",
    universe_m_d_ref = stock_universe_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
    liquidity_m_d_ref = liquidity_m_d_ref
  )
  res2 <- process_micro_portfolios(
    parallel = FALSE, groups = groups, group_members = shuffled,
    group_weights = NULL, micro_port_construction_method = "rp",
    universe_m_d_ref = stock_universe_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
    liquidity_m_d_ref = liquidity_m_d_ref
  )

  testthat::expect_equal(res1, res2)
})

testthat::test_that("single-asset group yields weight 1", {

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
  eligible_tickers <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
  selected_daily_stock_returns_m_xts_upd_ref <-
    daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),eligible_tickers]


  #Set top down micro weights

  ## Get eligible stock universe
  eligible_universe_m_d_ref <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1)

  ## Get groups
  groups <- stock_universe_m_d_ref$macro_sector %>% unique()
  group_members <- lapply(groups, function(g) {
    stock_universe_m_d_ref %>%
      dplyr::filter(macro_sector == g) %>%
      dplyr::pull(tickers)
  })
  names(group_members) <- groups

    # Clone and force a single-asset group by trimming its member list
  single_member <- group_members
  first_g <- groups[1]
  single_member[[first_g]] <- "ABEV3"

  res <- process_micro_portfolios(
    parallel = FALSE, groups = groups, group_members = single_member,
    group_weights = NULL, micro_port_construction_method = "rp",
    universe_m_d_ref = stock_universe_m_d_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
    liquidity_m_d_ref = liquidity_m_d_ref
  )

  w <- res[[first_g]]@universe_m_d_ref@data$weights
  testthat::expect_true(abs(sum(w) - 1) < 1e-6)
  testthat::expect_true(sum(w > 0.999) == 1L) # essentially all weight on the only asset
})

testthat::test_that("process_micro_portfolios errors when constraints require group_weights but are NULL", {

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
  elig_tickers <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
  selected_daily_stock_returns_m_xts_upd_ref <-
    daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date), elig_tickers]

  #Set top down micro weights

  ## Get groups
  groups <- stock_universe_m_d_ref$macro_sector %>% unique()
  group_members <- lapply(groups, function(g) {
    stock_universe_m_d_ref %>%
      dplyr::filter(macro_sector == g) %>%
      dplyr::pull(tickers)
  })
  names(group_members) <- groups

  # Define group weights ad hoc (Exportador sums more than 1)
  group_weights <- c(0.5, 0.2, 0.2, 0.1)
  names(group_weights) <- groups

  # Use a minimal non-random method (rp) with a constraint to trigger the check
  conc_pol <- concentration_constraint_policy
  conc_pol$max_abs_active_individual_weight <- 0.05

  testthat::expect_error(
    process_micro_portfolios(
      parallel = FALSE,
      groups = groups,
      group_members = group_members,
      group_weights = NULL,
      micro_port_construction_method = "rp",
      universe_m_d_ref = stock_universe_m_d_ref,
      eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
      liquidity_m_d_ref = liquidity_m_d_ref,
      concentration_constraint_policy = conc_pol
    ),
    "group_weights must be provided if any constraint or ridge pen are defined."
  )
})

testthat::test_that("process_micro_portfolios errors when covariance matrix our groups are problematic", {

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
  elig_tickers <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
  selected_daily_stock_returns_m_xts_upd_ref <-
    daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date), elig_tickers]


  #Set top down micro weights
  ## Get groups
  groups <- stock_universe_m_d_ref$macro_sector %>% unique()
  group_members <- lapply(groups, function(g) {
    stock_universe_m_d_ref %>%
      dplyr::filter(macro_sector == g) %>%
      dplyr::pull(tickers)
  })
  names(group_members) <- groups

  # Define group weights ad hoc (Exportador sums more than 1)
  group_weights <- c(0.5, 0.2, 0.2, 0.1)
  names(group_weights) <- groups

  #Eligible returns missing tickers
  wrong_selected_daily_stock_returns_m_xts_upd_ref <- selected_daily_stock_returns_m_xts_upd_ref[, -1]

  testthat::expect_error(
    process_micro_portfolios(
      parallel = FALSE,
      groups = groups,
      group_members = group_members,
      group_weights = NULL,
      micro_port_construction_method = "rp",
      universe_m_d_ref = stock_universe_m_d_ref,
      eligible_returns_m_xts_upd_ref = wrong_selected_daily_stock_returns_m_xts_upd_ref,
      liquidity_m_d_ref = liquidity_m_d_ref
    ),
    "Some tickers in group Doméstico Defensivo are not in eligible_returns_m_xts_upd_ref."
  )

  #Groups with no members
  wrong_group_members <- group_members
  wrong_group_members[[1]] <- character(0)

  testthat::expect_error(
    process_micro_portfolios(
      parallel = FALSE,
      groups = groups,
      group_members = wrong_group_members,
      group_weights = NULL,
      micro_port_construction_method = "rp",
      universe_m_d_ref = stock_universe_m_d_ref,
      eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
      liquidity_m_d_ref = liquidity_m_d_ref
    ),
    "Group Doméstico Defensivo has no eligible tickers."
  )

  #Groups with members not in universe
  wrong_group_members <- group_members
  wrong_group_members[[1]] <- c("AAA", "BBB")

  testthat::expect_error(
    process_micro_portfolios(
      parallel = FALSE,
      groups = groups,
      group_members = wrong_group_members,
      group_weights = NULL,
      micro_port_construction_method = "rp",
      universe_m_d_ref = stock_universe_m_d_ref,
      eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
      liquidity_m_d_ref = liquidity_m_d_ref
    ),
    "Group Doméstico Defensivo has no eligible tickers."
  )

  #Duplicated tickers
  wrong_group_members <- group_members
  wrong_group_members[[1]] <- c(wrong_group_members[[1]], wrong_group_members[[1]][1])

  testthat::expect_error(
    process_micro_portfolios(
      parallel = FALSE,
      groups = groups,
      group_members = wrong_group_members,
      group_weights = NULL,
      micro_port_construction_method = "rp",
      universe_m_d_ref = stock_universe_m_d_ref,
      eligible_returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref,
      liquidity_m_d_ref = liquidity_m_d_ref
    ),
    "Group Doméstico Defensivo has duplicated tickers."
  )


})


