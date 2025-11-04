testthat::test_that("calculate_port_stats(port) - happy path without covariance nor groups - ew", {

    #Create signals_m_d_ref
    load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

    #Quantile Range
    eligibility_quantile_range <- c(0.67, 1)

    #Current date
    current_date <- "2001-06-15"

    #Initial Preps
    signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
    liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
    benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
    stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
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

    stock_universe_m_d_ref$weights <- rep(0.25, 4)

    results <- calculate_port_stats(universe_m_d_ref = stock_universe_m_d_ref)$port_stats

    # Expected return
    w <- stock_universe_m_d_ref$weights
    exp_ret_manual <- sum(w * stock_universe_m_d_ref$exp_ret_score)
    testthat::expect_equal(results$exp_ret, exp_ret_manual, tolerance = 1e-12)

    # Risk (stdev)
    expect_true(
      all(results %>%
            dplyr::select(risk, sharpe, diversification_ratio, wavg_pairwise_corr,
                          dplyr::contains("rrc")) %>%
            sapply(function(x) is.na(x)))
    )

    #Groups
    expect_true(
      all(results %>%
            dplyr::select(dplyr::contains("group")) %>%
            sapply(function(x) is.na(x)))
    )


    #others
    expect_equal(results$hhi_weights, sum(w^2))
    expect_equal(results$n_eff_weights, 1 / sum(w^2))
    expect_equal(results$entropy_weights, -sum(w * log(w)))
    expect_equal(results$entropy_effective_n, exp(results$entropy_weights))
    abs_diff <- list()
    for(i in seq_along(w)) {
      for(j in seq_along(w)) {
        if (i != j) {
          abs_diff[[i]] <- abs(w[i] - w[j])
        }
      }
    }

    expect_equal(results$gini_weights, sum(unlist(abs_diff)) / (2 * sum(w) * sum(w)))
    expect_equal(results$top_5_concentration, sum(sort(w, decreasing = TRUE)[1:min(5, length(w))]), tolerance = 1e-12)
    expect_equal(results$top_10_concentration, sum(sort(w, decreasing = TRUE)[1:min(10, length(w))]), tolerance = 1e-12)
    expect_equal(results$top_25_concentration, sum(sort(w, decreasing = TRUE)[1:min(25, length(w))]), tolerance = 1e-12)
    expect_equal(results$gross_exposure, sum(abs(w)))
    expect_equal(results$net_exposure,   sum(w))


    #As we are in EW, hhi_weights should be 1/N and n_eff_weights should be N
    N <- length(w)
    expect_equal(results$hhi_weights, 1 / N, tolerance = 1e-12)
    expect_equal(results$n_eff_weights, N, tolerance = 1e-12)
    expect_equal(results$entropy_weights, -unique(log(w)))
    expect_equal(results$entropy_weights, unique(log(1/w)))
    expect_equal(results$entropy_effective_n, N, tolerance = 1e-12)
    expect_equal(results$gini_weights, 0, tolerance = 1e-12)
    expect_equal(results$top_5_concentration, min(5, N) / N, tolerance = 1e-12)
    expect_equal(results$top_10_concentration, min(10, N) / N, tolerance = 1e-12)
    expect_equal(results$top_25_concentration, min(25, N) / N, tolerance = 1e-12)


  })

testthat::test_that("calculate_port_stats(port) - happy path with covariance and groups - rp", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
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

  #Force 1 ineligible
  stock_universe_m_d_ref$is_eligible[4] <- 0L

  #Test RP
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]

  covariance_matrix <- estimate_covariance_matrix(tickers = c("Stock A", "Stock C", "Stock D"), returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  rp_results <- riskParityPortfolio::riskParityPortfolio(Sigma = covariance_matrix)
  stock_universe_m_d_ref$rel_risk_contr <- c(rp_results$relative_risk_contribution, 0)
  stock_universe_m_d_ref$weights <- c(rp_results$w, 0)

  elig_universe_m_d_ref <- stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1L)

  w <- elig_universe_m_d_ref$weights
  colnames_cov <- elig_universe_m_d_ref$tickers
  covmat <- covariance_matrix
  cormat <- stats::cov2cor(covmat)

  # Group Portfolio
  group_col <- names(stock_groups_m_d_ref)[4]
  macro_objects_list <- compute_agg_macro_objects(
    universe_m_d_ref = stock_universe_m_d_ref,
    covariance_matrix = covariance_matrix,
    group_col = group_col,
    liquidity_m_d_ref = liquidity_m_d_ref,
    micro_universe_m_d_ref_list = NULL
  )

  group_universe_m_d_ref <- macro_objects_list$group_universe_m_d_ref

  group_weights_m_d_ref <- macro_objects_list$group_universe_m_d_ref %>%
    dplyr::select(id, tickers, dates, weights)

  ### Create benchmark port object
  macro_port_obj <- set_portfolio_weights(
    universe_m_d_ref = group_universe_m_d_ref %>% dplyr::select(-weights), #Universe
    port_construction_method = "custom_weights",
    custom_weights_m_d_ref = group_weights_m_d_ref,
    covariance_matrix = macro_objects_list$group_covariance_matrix,
    groups_m_d_ref = NULL,
    selected_benchmark = NULL, #Avoid infinite recursion
    level = "group"
  )

  results <- calculate_port_stats(
    universe_m_d_ref = stock_universe_m_d_ref,
    all_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    groups_m_d_ref = stock_groups_m_d_ref,
    cov_matrix_sample_size = 252,
    cov_estimation_method = covariance_estimation_method,
    group_universe_m_d_ref = group_universe_m_d_ref,
    group_cov_matrix = macro_objects_list$group_covariance_matrix
  )$port_stats


  # Expected return
  exp_ret_manual <- sum(w * elig_universe_m_d_ref$exp_ret_score)
  testthat::expect_equal(results$exp_ret, exp_ret_manual, tolerance = 1e-12)

  # Risk (stdev)
  risk_manual <- sqrt(as.numeric(t(w) %*% covmat[colnames_cov, colnames_cov] %*% w))
  expect_equal(results$risk, risk_manual, tolerance = 1e-12)

  # Sharpe
  sharpe_manual <- exp_ret_manual / risk_manual
  expect_equal(results$sharpe, sharpe_manual, tolerance = 1e-12)

  #others
  expect_equal(results$hhi_weights, sum(w^2))
  expect_equal(results$n_eff_weights, 1 / sum(w^2))
  expect_equal(results$entropy_weights, -sum(w * log(w)))
  expect_equal(results$entropy_effective_n, exp(results$entropy_weights))
  n <- length(w)
  mu <- mean(w)                # = 1/n
  acc <- 0
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      acc <- acc + abs(w[i] - w[j])
    }
  }

  expect_equal(results$gini_weights, as.numeric(acc / (2 * n^2 * mu)))
  expect_equal(results$top_5_concentration, sum(sort(w, decreasing = TRUE)[1:min(5, length(w))]), tolerance = 1e-12)
  expect_equal(results$top_10_concentration, sum(sort(w, decreasing = TRUE)[1:min(10, length(w))]), tolerance = 1e-12)
  expect_equal(results$top_25_concentration, sum(sort(w, decreasing = TRUE)[1:min(25, length(w))]), tolerance = 1e-12)
  expect_equal(results$gross_exposure, sum(abs(w)))
  expect_equal(results$net_exposure,   sum(w))
  ind_vol_times_weight <- list()
  for (i in seq_along(w)) {
    ind_vol_times_weight[[i]] <- sqrt(covmat[colnames_cov[i], colnames_cov[i]]) * w[i]
  }
  expect_equal(results$diversification_ratio,
               sum(unlist(ind_vol_times_weight))/risk_manual, tolerance = 1e-12)

  numerator <- 0
  denominator <- 0
  for (i in seq_along(w)) {
    for (j in seq_along(w)) {
      if (i != j) {
        numerator <- numerator + cormat[colnames_cov[i], colnames_cov[j]] * w[i] * w[j]
        denominator <- denominator + w[i] * w[j]
      }
    }
  }
  man_weighted_avg_corr <- numerator / denominator # Final step
  expect_equal(results$wavg_pairwise_corr, man_weighted_avg_corr, tolerance = 1e-12)

  #As we are in RC...
  expect_equal(results$hhi_rrc, mean(stock_universe_m_d_ref$rel_risk_contr[c(1:3)]), tolerance = 1e-12)
  expect_equal(results$n_eff_rrc, length(w))
  expect_equal(results$rrc_dist_to_erc, 0, tolerance = 1e-7)

  #Now for groups
  expect_equal(results$group_exp_ret,sum(group_universe_m_d_ref$weights * group_universe_m_d_ref$exp_ret_score),
               tolerance = 1e-12)
  expect_equal(results$group_hhi_weights,sum(group_universe_m_d_ref$weights^2), tolerance = 1e-12)
  expect_equal(results$group_n_eff_weights, 1 / sum(group_universe_m_d_ref$weights^2), tolerance = 1e-12)
  expect_equal(results$group_top_1_concentration, group_universe_m_d_ref$weights %>% max(), tolerance = 1e-12)
  group_rrc <- relative_risk_contribution(
    weights = group_universe_m_d_ref$weights,
    covariance_matrix = macro_objects_list$group_covariance_matrix
  )
  expect_equal(results$group_hhi_rrc, sum(group_rrc$rel_risk_contr^2), tolerance = 1e-12)
  expect_equal(results$group_n_eff_rrc, nrow(group_universe_m_d_ref), tolerance = 1e-12)
  expect_equal(results$group_rrc_dist_to_erc, 0, tolerance = 1e-7)

  expect_equal(macro_port_obj@port_stats$exp_ret,
               results$group_exp_ret, tolerance = 1e-12)
  expect_equal(macro_port_obj@port_stats$risk,
               results$group_risk, tolerance = 1e-12)
  expect_equal(macro_port_obj@port_stats$sharpe,
               results$group_sharpe, tolerance = 1e-12)
  expect_equal(macro_port_obj@port_stats$hhi_weights,
               results$group_hhi_weights, tolerance = 1e-12)
  expect_equal(macro_port_obj@port_stats$n_eff_weights,
               results$group_n_eff_weights, tolerance = 1e-12)
  expect_equal(macro_port_obj@port_stats$entropy_weights,
               results$group_entropy_weights, tolerance = 1e-12)
  expect_equal(macro_port_obj@port_stats$entropy_effective_n,
               results$group_entropy_effective_n, tolerance = 1e-12)
  expect_equal(macro_port_obj@port_stats$gini_weights,
               results$group_gini_weights, tolerance = 1e-12)
  expect_equal(macro_port_obj@port_stats$diversification_ratio,
               results$group_diversification_ratio, tolerance = 1e-12)
  expect_equal(macro_port_obj@port_stats$wavg_pairwise_corr,
               results$group_wavg_pairwise_corr, tolerance = 1e-12)
  expect_equal(macro_port_obj@port_stats$hhi_rrc,
               results$group_hhi_rrc, tolerance = 1e-12)
  expect_equal(macro_port_obj@port_stats$rrc_dist_to_erc,
               results$group_rrc_dist_to_erc, tolerance = 1e-12)


})

testthat::test_that("calculate_port_stats(port) - active", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2001-06-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
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

  #Force 1 ineligible
  stock_universe_m_d_ref$is_eligible[4] <- 0L
  #Force 1 out of benchmark
  stock_universe_m_d_ref$ibov_bench_weights[2] <- 0
  stock_universe_m_d_ref$ibov_bench_weights <- stock_universe_m_d_ref$ibov_bench_weights/sum(stock_universe_m_d_ref$ibov_bench_weights)

  #Test RP
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]

  covariance_matrix <- estimate_covariance_matrix(tickers = c("Stock A", "Stock C", "Stock D"),
                                                  returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  rp_results <- riskParityPortfolio::riskParityPortfolio(Sigma = covariance_matrix)
  stock_universe_m_d_ref$rel_risk_contr <- c(rp_results$relative_risk_contribution, 0)
  stock_universe_m_d_ref$weights <- c(rp_results$w, 0)


  #Benchmark
  selected_bench <- "ibov"
  bench_weights_m_d_ref <- stock_universe_m_d_ref %>%
    dplyr::select(id, tickers, dates, paste0(selected_bench, "_bench_weights")) %>%
    dplyr::rename(weights = paste0(selected_bench, "_bench_weights")) %>%
    dplyr::mutate(weights = weights/sum(weights))
  bench_universe_m_d_ref <- stock_universe_m_d_ref %>%
    dplyr::select(-weights, -rel_risk_contr) %>%
    dplyr::left_join(bench_weights_m_d_ref %>% dplyr::select(id, weights),
                     by = "id") %>%
    dplyr::mutate(is_eligible = ifelse(weights > 0, 1, 0)) %>%
    dplyr::select(-weights)

  expect_warning(
    selected_benchmark_port_obj <- set_portfolio_weights(
      universe_m_d_ref = bench_universe_m_d_ref, #Universe
      port_construction_method = "custom_weights",
      custom_weights_m_d_ref = bench_weights_m_d_ref,
      eligible_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, #Return sample
      cov_matrix_sample_size = 252, #Cov estimation
      cov_estimation_method = covariance_estimation_method,
      groups_m_d_ref = stock_groups_m_d_ref,
      selected_benchmark = NULL, #Avoid infinite recursion
      level = "benchmark"
    ),
    "The following groups are missing in macro eligible_assets: Financials"
  )

  #Macro
  group_col <- names(stock_groups_m_df)[4]

  ### Compute macro objects
  macro_objects_list <- compute_agg_macro_objects(
    universe_m_d_ref = stock_universe_m_d_ref,
    covariance_matrix = covariance_matrix,
    group_col = group_col,
    liquidity_m_d_ref = liquidity_m_d_ref,
    micro_universe_m_d_ref_list = NULL
  )

  ### Create a custom_weights_m_d_ref
  group_weights_m_d_ref <- macro_objects_list$group_universe_m_d_ref %>%
    dplyr::select(id, tickers, dates, weights)

  ### Create benchmark port object
  macro_port_obj <- set_portfolio_weights(
    universe_m_d_ref = macro_objects_list$group_universe_m_d_ref %>%
      dplyr::select(-weights), #Universe
    port_construction_method = "custom_weights",
    custom_weights_m_d_ref = group_weights_m_d_ref,
    covariance_matrix = macro_objects_list$group_covariance_matrix,
    groups_m_d_ref = NULL,
    selected_benchmark = NULL, #Avoid infinite recursion
    level = "group"
  )

  macro_port_obj@port_name <- group_col
  group_universe_m_d_ref <- macro_objects_list$group_universe_m_d_ref
  group_cov_matrix <- macro_objects_list$group_covariance_matrix

  results <- calculate_port_stats(
    universe_m_d_ref = stock_universe_m_d_ref,
    group_universe_m_d_ref = group_universe_m_d_ref,
    group_cov_matrix = group_cov_matrix,
    selected_benchmark = "ibov",
    bench_universe_m_d_ref = selected_benchmark_port_obj@universe_m_d_ref@data,
    all_returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
    groups_m_d_ref = stock_groups_m_d_ref
  )$port_stats

  w <- stock_universe_m_d_ref$weights -
    selected_benchmark_port_obj@universe_m_d_ref@data$weights
  covmat <- estimate_covariance_matrix(tickers = c("Stock A", "Stock C", "Stock D", "Stock E"),
                                       returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                       cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                       active_returns = FALSE,
                                       groups_m_d_ref = stock_groups_m_d_ref)


  # Expected return
  exp_ret_manual <- sum(w * stock_universe_m_d_ref$exp_ret_score)
  testthat::expect_equal(results$act_exp_ret, exp_ret_manual, tolerance = 1e-12)

  # Risk (stdev)
  risk_manual <- sqrt(as.numeric(t(w) %*% covmat %*% w))
  expect_equal(results$act_risk, risk_manual, tolerance = 1e-12)

  # Sharpe
  info_ratio_manual <- exp_ret_manual / risk_manual
  expect_equal(results$info_ratio, info_ratio_manual, tolerance = 1e-12)

  #others
  ww <- abs(w)/sum(abs(w))
  expect_equal(results$act_hhi_weights, sum(ww^2))
  expect_equal(results$act_n_eff_weights, 1 / sum(ww^2))
  expect_equal(results$act_entropy_weights, -sum(ww * log(ww)))
  expect_equal(results$act_entropy_effective_n, exp(results$act_entropy_weights))

  n <- length(ww)
  mu <- mean(ww)                # = 1/n
  acc <- 0
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      acc <- acc + abs(ww[i] - ww[j])
    }
  }

  expect_equal(results$act_gini_weights, as.numeric(acc / (2 * n)))
  expect_equal(results$act_top_5_concentration, sum(sort(ww, decreasing = TRUE)[1:min(5, length(w))]), tolerance = 1e-12)
  expect_equal(results$act_top_10_concentration, sum(sort(ww, decreasing = TRUE)[1:min(10, length(w))]), tolerance = 1e-12)
  expect_equal(results$act_top_25_concentration, sum(sort(ww, decreasing = TRUE)[1:min(25, length(w))]), tolerance = 1e-12)
  expect_equal(results$act_gross_exposure, sum(abs(w)))
  expect_equal(results$act_net_exposure,   sum(w))
  ind_vol_times_weight <- list()
  colnames_cov <- stock_universe_m_d_ref$tickers
  for (i in seq_along(ww)) {
    ind_vol_times_weight[[i]] <- sqrt(covmat[colnames_cov[i], colnames_cov[i]]) * abs(w[i])
  }
  expect_equal(results$act_diversification_ratio,
               sum(unlist(ind_vol_times_weight))/risk_manual, tolerance = 1e-12)

  numerator <- 0
  denominator <- 0
  cormat <- stats::cov2cor(covmat)
  for (i in seq_along(w)) {
    for (j in seq_along(w)) {
      if (i != j) {
        numerator <- numerator + cormat[colnames_cov[i], colnames_cov[j]] * abs(w[i] * w[j])
        denominator <- denominator + abs(w[i] * w[j])
      }
    }
  }
  man_weighted_avg_corr <- numerator / denominator # Final step
  expect_equal(results$act_wavg_pairwise_corr, man_weighted_avg_corr, tolerance = 1e-12)

  #As we are in RC...
  rrc <- relative_risk_contribution(
    weights = w,
    covariance_matrix = covmat
  )
  expect_equal(results$act_hhi_rrc, sum(rrc$rel_risk_contr^2), tolerance = 1e-12)
  expect_equal(results$act_n_eff_rrc, 1/results$act_hhi_rrc)
  expect_equal(results$act_rrc_dist_to_erc, sqrt(sum((rrc$rel_risk_contr - 1/4)^2)), tolerance = 1e-7)

  #Now for groups
  act_group_w <- group_universe_m_d_ref$weights - group_universe_m_d_ref$ibov_bench_weights
  expect_equal(results$act_group_exp_ret, sum(act_group_w * group_universe_m_d_ref$exp_ret_score), tolerance = 1e-12)
  expect_equal(results$act_group_hhi_weights, sum((abs(act_group_w)/sum(abs(act_group_w)))^2), tolerance = 1e-12)
  expect_equal(results$act_group_n_eff_weights, 1/sum((abs(act_group_w)/sum(abs(act_group_w)))^2), tolerance = 1e-12)
  expect_equal(results$act_group_top_1_concentration, max(abs(act_group_w)/sum((abs(act_group_w)))), tolerance = 1e-12)
  expect_equal(results$act_group_hhi_rrc, sum((relative_risk_contribution(act_group_w, group_cov_matrix)$rel_risk_contr)^2), tolerance = 1e-12)
  expect_equal(results$act_group_n_eff_rrc, 1/results$act_group_hhi_rrc, tolerance = 1e-12)
  expect_equal(results$act_group_rrc_dist_to_erc,
               sqrt(sum((relative_risk_contribution(act_group_w, group_cov_matrix)$rel_risk_contr - rep(1/3, 3))^2)), tolerance = 1e-7)

  #Benchmark
  w_bench <- selected_benchmark_port_obj@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(weights)
  exp_ret_score_bench <- selected_benchmark_port_obj@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(exp_ret_score)

  bench_covmat <- estimate_covariance_matrix(tickers = c("Stock A","Stock D", "Stock E"),
                                       returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                       cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                       active_returns = FALSE,
                                       groups_m_d_ref = stock_groups_m_d_ref)


  # Expected return
  exp_ret_manual <- sum(w_bench * exp_ret_score_bench)
  testthat::expect_equal(selected_benchmark_port_obj@port_stats$exp_ret, exp_ret_manual, tolerance = 1e-12)

  # Risk (stdev)
  risk_manual <- sqrt(as.numeric(t(w_bench) %*% bench_covmat %*% w_bench))
  expect_equal(selected_benchmark_port_obj@port_stats$risk, risk_manual, tolerance = 1e-12)

  # Sharpe
  sharpe_manual <- exp_ret_manual / risk_manual
  expect_equal(selected_benchmark_port_obj@port_stats$sharpe, sharpe_manual, tolerance = 1e-12)

  #others
  expect_equal(selected_benchmark_port_obj@port_stats$hhi_weights, sum(w_bench^2))
  expect_equal(selected_benchmark_port_obj@port_stats$n_eff_weights, 1 / sum(w_bench^2))
  expect_equal(selected_benchmark_port_obj@port_stats$entropy_weights, -sum(w_bench * log(w_bench)))
  expect_equal(selected_benchmark_port_obj@port_stats$entropy_effective_n,
               exp(selected_benchmark_port_obj@port_stats$entropy_weights))

  n <- length(w_bench)
  mu <- mean(w_bench)                # = 1/n
  acc <- 0
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      acc <- acc + abs(w_bench[i] - w_bench[j])
    }
  }

  expect_equal(selected_benchmark_port_obj@port_stats$gini_weights, as.numeric(acc / (2 * n)))
  expect_equal(selected_benchmark_port_obj@port_stats$top_5_concentration, sum(sort(w_bench, decreasing = TRUE)[1:min(5, length(w_bench))]), tolerance = 1e-12)
  ind_vol_times_weight <- list()
  colnames_cov <- selected_benchmark_port_obj@universe_m_d_ref@data %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
  for (i in seq_along(w_bench)) {
    ind_vol_times_weight[[i]] <- sqrt(bench_covmat[colnames_cov[i], colnames_cov[i]]) * abs(w_bench[i])
  }
  expect_equal(selected_benchmark_port_obj@port_stats$diversification_ratio,
               sum(unlist(ind_vol_times_weight))/risk_manual, tolerance = 1e-12)

  numerator <- 0
  denominator <- 0
  bench_cormat <- stats::cov2cor(bench_covmat)
  for (i in seq_along(w_bench)) {
    for (j in seq_along(w_bench)) {
      if (i != j) {
        numerator <- numerator + bench_cormat[colnames_cov[i], colnames_cov[j]] * abs(w_bench[i] * w_bench[j])
        denominator <- denominator + abs(w_bench[i] * w_bench[j])
      }
    }
  }
  man_weighted_avg_corr <- numerator / denominator # Final step
  expect_equal(selected_benchmark_port_obj@port_stats$wavg_pairwise_corr, man_weighted_avg_corr, tolerance = 1e-12)


  #Now for groups
  group_w <- selected_benchmark_port_obj@macro@universe_m_d_ref@data$weights
  expect_equal(selected_benchmark_port_obj@port_stats$group_exp_ret,
               sum(group_w * selected_benchmark_port_obj@macro@universe_m_d_ref@data$exp_ret_score), tolerance = 1e-12)
  expect_equal(selected_benchmark_port_obj@port_stats$group_hhi_weights, sum((abs(group_w)/sum(abs(group_w)))^2), tolerance = 1e-12)
  expect_equal(selected_benchmark_port_obj@port_stats$group_n_eff_weights, 1/sum((abs(group_w)/sum(abs(group_w)))^2), tolerance = 1e-12)
  expect_equal(selected_benchmark_port_obj@port_stats$group_top_1_concentration, max(abs(group_w)/sum((abs(group_w)))), tolerance = 1e-12)
  expect_equal(selected_benchmark_port_obj@port_stats$group_hhi_rrc, sum((relative_risk_contribution(group_w, selected_benchmark_port_obj@macro@covariance_matrix)$rel_risk_contr)^2), tolerance = 1e-12)
  expect_equal(selected_benchmark_port_obj@port_stats$group_n_eff_rrc, 1/selected_benchmark_port_obj@port_stats$group_hhi_rrc, tolerance = 1e-12)
  expect_equal(selected_benchmark_port_obj@port_stats$group_rrc_dist_to_erc,
               sqrt(sum((relative_risk_contribution(group_w, selected_benchmark_port_obj@macro@covariance_matrix)$rel_risk_contr - rep(1/2, 2))^2)), tolerance = 1e-7)



})

testthat::test_that("errors are appropriately called", {

  # Helper: minimal universe rows
  mk_universe <- function(
    ids     = c("A-2000-01-01","B-2000-01-01","C-2000-01-01"),
    tickers = c("A","B","C"),
    elig    = c(1L,1L,1L),
    w       = c(0.5,0.3,0.2),
    exp     = c(0.02,0.01,0.03),
    relrc   = NULL,
    dates   = as.Date("2000-01-01")
  ) {
    n <- length(ids)

    # basic length checks (allow scalar recycling for dates/exp/relrc)
    stopifnot(length(tickers) == n, length(elig) == n, length(w) == n)
    if (!is.null(exp))  stopifnot(length(exp)  %in% c(1L, n))
    if (!is.null(relrc)) stopifnot(length(relrc) %in% c(1L, n))

    df <- list(
      id          = ids,
      tickers     = tickers,
      dates       = rep_len(as.Date(dates), n),
      is_eligible = as.integer(elig),
      weights     = w
    )

    if (!is.null(exp))   df$exp_ret_score   <- rep_len(exp, n)
    if (!is.null(relrc)) df$rel_risk_contr  <- rep_len(relrc, n)

    do.call(base::data.frame, c(df, list(stringsAsFactors = FALSE)))
  }

  # Tiny returns panel
  mk_returns <- function(cols = c("A","B","C"), n = 252) {
    set.seed(1)
    X <- matrix(stats::rnorm(n * length(cols), sd = 0.01), nrow = n, ncol = length(cols))
    colnames(X) <- cols
    X
  }

  # 1) NA checks on portfolio and benchmark columns
  uni <- mk_universe()
  uni$weights[2] <- NA_real_
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = uni),
    "NA values found in portfolio weights."
  )

  uni <- mk_universe()
  uni$exp_ret_score[1] <- NA_real_
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = uni),
    "NA values found in exp ret scores."
  )

  uni <- mk_universe(relrc = c(0.3, 0.4, NA))
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = uni),
    "NA values found in portfolio rel_risk_contr."
  )

  # Benchmark NA checks
  bench <- mk_universe(w = c(0.6, 0.4, 0))
  bench$weights[1] <- NA_real_
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = mk_universe(),
                         selected_benchmark = "ibov",
                         bench_universe_m_d_ref = bench),
    "NA values found in bench weights."
  )

  bench <- mk_universe(w = c(0.6, 0.4, 0))
  bench$exp_ret_score[2] <- NA_real_
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = mk_universe(),
                         selected_benchmark = "ibov",
                         bench_universe_m_d_ref = bench),
    "NA values found in bench exp ret scores."
  )

  bench <- mk_universe(w = c(0.6, 0.4, 0), relrc = c(0.5, NA, NA))
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = mk_universe(),
                         selected_benchmark = "ibov",
                         bench_universe_m_d_ref = bench),
    "NA values found in bench rel_risk_contr."
  )

  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = mk_universe(),
                         selected_benchmark = "ibov", bench_universe_m_d_ref = NULL),
    "Both selected_benchmark and bench_universe_m_d_ref must be provided together."
  )

  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = mk_universe(),
                         selected_benchmark = NULL, bench_universe_m_d_ref = mk_universe()),
    "Both selected_benchmark and bench_universe_m_d_ref must be provided together."
  )

  uni <- mk_universe()
  R <- mk_returns(cols = c("A","B"))  # missing "C"
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = uni, all_returns_m_xts_upd_ref = R),
    "Row/column names of all_returns_m_xts_upd_ref must match eligible tickers in portfolio."
  )

  # With benchmark
  bench <- mk_universe(w = c(0.5, 0.5, 0))
  Rb <- mk_returns(cols = c("A"))     # missing "B"
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = uni %>% dplyr::filter(tickers == "A"),
                         selected_benchmark = "ibov",
                         bench_universe_m_d_ref = bench,
                         all_returns_m_xts_upd_ref = Rb),
    "Row/column names of all_returns_m_xts_upd_ref must match eligible tickers in benchmark."
  )

  uni <- mk_universe()
  cov_ok <- diag(c(0.04,0.09,0.16))
  rownames(cov_ok) <- colnames(cov_ok) <- uni$tickers
  testthat::expect_silent(
    calculate_port_stats(universe_m_d_ref = uni, covariance_matrix = cov_ok)
  )

  # Wrong order or missing name -> error
  cov_bad <- diag(c(0.04,0.09))
  rownames(cov_bad) <- colnames(cov_bad) <- c("A","B")
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = uni, covariance_matrix = cov_bad),
    "Row/column names of covariance_matrix must match eligible tickers in portfolio."
  )

  # Benchmark + covariance_matrix forbidden
  bench <- mk_universe(w = c(0.6,0.4,0))
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = uni,
                         covariance_matrix = cov_ok,
                         selected_benchmark = "ibov",
                         bench_universe_m_d_ref = bench),
    "When a benchmark is provided, covariance_matrix must be NULL."
  )

  uni <- mk_universe(tickers = c("A","B","C"))
  bench <- mk_universe(tickers = c("A","B","D"), w = c(0.5,0.4,0.1))  # D not in port
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = uni,
                         selected_benchmark = "ibov",
                         bench_universe_m_d_ref = bench),
    "The following tickers are in bench_universe_m_d_ref but missing in universe_m_d_ref: D"
  )

  uni <- mk_universe()
  bench <- mk_universe(w = c(0.6, 0.4, 0))
  bench$is_eligible[1] <- 0L  # should be 1 when weight > 0
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = uni,
                         selected_benchmark = "ibov",
                         bench_universe_m_d_ref = bench),
    "In bench_universe_m_d_ref, is_eligible must match weights > 0 stocks."
  )

  bench <- mk_universe(w = c(0.6, 0, 0))  # B has 0 weight
  bench$is_eligible[2] <- 1L              # should be 0 here
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = uni,
                         selected_benchmark = "ibov",
                         bench_universe_m_d_ref = bench),
    "In bench_universe_m_d_ref, is_eligible must match weights > 0 stocks."
  )

  # NA after join: break the id match on one row to force NA bench_w
  uni2 <- mk_universe()
  bench2 <- mk_universe()
  bench2$id[1] <- "BROKEN-ID"
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = uni2,
                         selected_benchmark = "ibov",
                         bench_universe_m_d_ref = bench2),
    "After joining benchmark weights, some tickers have NA bench_w."
  )

  grp <- data.frame(
    id = paste0(c("G1","G2","G3"), "-2000-01-01"),
    tickers = c("G1","G2","G3"),
    dates = as.Date("2000-01-01"),
    is_eligible = 1L,
    weights = c(0.5,0.3,0.2),
    exp_ret_score = c(0.01,0.02,0.03),
    rel_risk_contr = c(0.4,0.3,0.3),
    stringsAsFactors = FALSE
  )

  # NA weights
  grp_bad <- grp; grp_bad$weights[2] <- NA_real_
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = mk_universe(),
                         group_universe_m_d_ref = grp_bad),
    "NA values found in group weights."
  )

  # NA exp_ret_score
  grp_bad <- grp; grp_bad$exp_ret_score[1] <- NA_real_
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = mk_universe(),
                         group_universe_m_d_ref = grp_bad),
    "NA values found in group exp_ret_score."
  )

  # NA rel_risk_contr
  grp_bad <- grp; grp_bad$rel_risk_contr[3] <- NA_real_
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = mk_universe(),
                         group_universe_m_d_ref = grp_bad),
    "NA values found in group rel_risk_contr."
  )

  # Any ineligible group → error
  grp_bad <- grp; grp_bad$is_eligible[2] <- 0L
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = mk_universe(),
                         group_universe_m_d_ref = grp_bad),
    "In group_universe_m_d_ref, all groups must be eligible."
  )

  # Group cov name alignment
  Gcov <- diag(c(0.05,0.07,0.09)); rownames(Gcov) <- colnames(Gcov) <- c("G1","G2","G9")
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = mk_universe(),
                         group_universe_m_d_ref = grp,
                         group_cov_matrix = Gcov),
    "Row/column names of group_cov_matrix must match eligible tickers in group_universe_m_d_ref."
  )

  # Benchmark provided but group df missing required bench column
  testthat::expect_error(
    calculate_port_stats(universe_m_d_ref = mk_universe(),
                         group_universe_m_d_ref = grp,
                         selected_benchmark = "ibov",
                         bench_universe_m_d_ref = mk_universe()),
    "group_universe_m_d_ref must contain a column named ibov_bench_weights"
  )

  uni <- mk_universe(elig = c(0L,0L,0L), w = c(0,0,0))
  # Weights do not sum to 1
  expect_error(calculate_port_stats(universe_m_d_ref = uni), "Weights in universe_m_d_ref should sum to 1.")


})

testthat::test_that("calculate_port_stats - top_k bounds (k>N and k=1)", {
  uni_df <- data.frame(
    id = c("A-2020-01-01","B-2020-01-01"),
    tickers = c("A","B"),
    dates = as.Date("2020-01-01"),
    weights = c(0.7, 0.3),
    exp_ret_score = c(0.02, 0.01),
    stringsAsFactors = FALSE
  )
  w <- uni_df$weights
  covmat <- matrix(c(0.04, 0.01, 0.01, 0.09), 2, 2, byrow = TRUE,
                   dimnames = list(uni_df$tickers, uni_df$tickers))

  # k > N => equals sum of all weights = 1
  s1 <- top_k_concentration(uni_df$weights, k = 10L)
  testthat::expect_equal(s1, 1.0, tolerance = 1e-12)

  # k = 1 => equals largest weight
  s2 <- top_k_concentration(uni_df$weights, k = 1L)
  testthat::expect_equal(s2, max(w), tolerance = 1e-12)
})

