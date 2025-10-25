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

    results <- calculate_port_stats(universe_m_d_ref = stock_universe_m_d_ref)

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
    eligible_universe_m_d_ref = elig_universe_m_d_ref,
    covariance_matrix = covariance_matrix,
    group_col = group_col,
    liquidity_m_d_ref = liquidity_m_d_ref,
    micro_universe_m_d_ref_list = NULL
  )
  group_rrc <- relative_risk_contribution(
    weights = macro_objects_list$group_universe_m_d_ref$weights,
    covariance_matrix = macro_objects_list$group_covariance_matrix
  )
  group_universe_m_d_ref <- macro_objects_list$group_universe_m_d_ref %>%
    dplyr::left_join(group_rrc, by = "tickers") %>%
    dplyr::relocate(rel_risk_contr, .before = weights)

  results <- calculate_port_stats(
    universe_m_d_ref = stock_universe_m_d_ref,
    covariance_matrix = covmat,
    group_universe_m_d_ref = group_universe_m_d_ref,
    group_cov_matrix = macro_objects_list$group_covariance_matrix
  )


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
  expect_equal(results$group_hhi_rrc, mean(group_universe_m_d_ref$rel_risk_contr), tolerance = 1e-12)
  expect_equal(results$group_n_eff_rrc, nrow(group_universe_m_d_ref), tolerance = 1e-12)
  expect_equal(results$group_rrc_dist_to_erc, 0, tolerance = 1e-7)

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
      returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref, #Return sample
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
    eligible_universe_m_d_ref = stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1),
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
    returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
    cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
    active_returns = FALSE,
    groups_m_d_ref = stock_groups_m_d_ref
  )

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
  expect_equal(results$act_hhi_rrc, mean(stock_universe_m_d_ref$rel_risk_contr[c(1:3)]), tolerance = 1e-12)
  expect_equal(results$act_n_eff_rrc, length(w))
  expect_equal(results$act_rrc_dist_to_erc, 0, tolerance = 1e-7)

  #Now for groups
  expect_equal(results$group_exp_ret,sum(group_universe_m_d_ref$weights * group_universe_m_d_ref$exp_ret_score),
               tolerance = 1e-12)

  expect_equal(results$group_hhi_weights,sum(group_universe_m_d_ref$weights^2), tolerance = 1e-12)
  expect_equal(results$group_n_eff_weights, 1 / sum(group_universe_m_d_ref$weights^2), tolerance = 1e-12)
  expect_equal(results$group_top_1_concentration, group_universe_m_d_ref$weights %>% max(), tolerance = 1e-12)
  expect_equal(results$group_hhi_rrc, mean(group_universe_m_d_ref$rel_risk_contr), tolerance = 1e-12)
  expect_equal(results$group_n_eff_rrc, nrow(group_universe_m_d_ref), tolerance = 1e-12)
  expect_equal(results$group_rrc_dist_to_erc, 0, tolerance = 1e-7)





})




testthat::test_that("calculate_port_stats(port) - corr provided; negatives ok", {
  w <- c(0.5, -0.2, 0.7)  # sums to 1
  exp_ret <- c(0.015, 0.01, 0.025)

  covmat <- matrix(
    c(0.04, 0.01, 0.00,
      0.01, 0.09, 0.02,
      0.00, 0.02, 0.16),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("A","B","C"), c("A","B","C"))
  )
  corr <- stats::cov2cor(covmat)

  uni_df <- data.frame(
    id      = c("A-2020-01-01","B-2020-01-01","C-2020-01-01"),
    tickers = c("A","B","C"),
    dates   = as.Date(c("2020-01-01","2020-01-01","2020-01-01")),
    stringsAsFactors = FALSE
  )
  uni_mdf <- create_meta_dataframe(data = uni_df, type = "generic")

  port_obj <- methods::new("port",
                           universe_m_d_ref         = uni_mdf,
                           port_construction_method = "rp",
                           eligible_assets          = c("A","B","C"),
                           exp_ret_score            = exp_ret,  # slot used
                           covariance_matrix        = covmat,
                           correlation_matrix       = corr,
                           weights                  = w,
                           rel_risk_contr           = relative_risk_contribution(w,covmat)$rel_risk_contr,
                           clusters                 = NULL,
                           mvo_port_spec            = NULL,
                           ind_max_weights          = NULL,
                           ind_min_weights          = NULL,
                           random_port_weights      = NULL,
                           groups                   = NULL,
                           mmaf_method              = NULL,
                           mmaf_group_col           = NULL,
                           group_cov_matrix         = NULL,
                           micro                    = NULL,
                           macro                    = NULL,
                           port_name                = "neg_w"
  )

  stats_df <- calculate_port_stats(port_obj)

  exp_ret_manual <- sum(w * exp_ret)
  var_manual <- as.numeric(t(w) %*% covmat %*% w)
  risk_manual <- sqrt(var_manual)
  sharpe_manual <- exp_ret_manual / risk_manual

  testthat::expect_equal(stats_df$port_exp_ret, exp_ret_manual, tolerance = 1e-12)
  testthat::expect_equal(stats_df$port_risk, risk_manual, tolerance = 1e-12)
  testthat::expect_equal(stats_df$port_sharpe, sharpe_manual, tolerance = 1e-12)
  testthat::expect_false(is.na(stats_df$wavg_pairwise_corr))
  testthat::expect_false(is.na(stats_df$diversification_ratio))
})

testthat::test_that("calculate_port_stats(port) - no covmat -> risk and sharpe NA; exp_ret ok", {
  w <- c(0.4, 0.6)
  exp_ret <- c(0.02, 0.01)

  uni_df <- data.frame(
    id      = c("A-2020-01-01","B-2020-01-01"),
    tickers = c("A","B"),
    dates   = as.Date(c("2020-01-01","2020-01-01")),
    stringsAsFactors = FALSE
  )
  uni_mdf <- create_meta_dataframe(uni_df)

  port_obj <- methods::new("port",
                           universe_m_d_ref         = uni_mdf,
                           port_construction_method = "ew",
                           eligible_assets          = c("A","B"),
                           exp_ret_score            = exp_ret,  # available
                           covariance_matrix        = NULL,     # none
                           correlation_matrix       = NULL,     # none
                           weights                  = w,
                           rel_risk_contr           = NULL,
                           clusters                 = NULL,
                           mvo_port_spec            = NULL,
                           ind_max_weights          = NULL,
                           ind_min_weights          = NULL,
                           random_port_weights      = NULL,
                           groups                   = NULL,
                           mmaf_method              = NULL,
                           mmaf_group_col           = NULL,
                           group_cov_matrix         = NULL,
                           micro                    = NULL,
                           macro                    = NULL,
                           port_name                = "ew_no_cov"
  )

  stats_df <- calculate_port_stats(port_obj)

  testthat::expect_equal(stats_df$port_exp_ret, sum(w * exp_ret), tolerance = 1e-12)
  testthat::expect_true(is.na(stats_df$port_risk))
  testthat::expect_true(is.na(stats_df$port_sharpe))
  testthat::expect_true(is.na(stats_df$diversification_ratio))
  testthat::expect_true(is.na(stats_df$wavg_pairwise_corr))
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
  uni_mdf <- create_meta_dataframe(uni_df)
  w <- uni_df$weights
  covmat <- matrix(c(0.04, 0.01, 0.01, 0.09), 2, 2, byrow = TRUE,
                   dimnames = list(uni_df$tickers, uni_df$tickers))

  port_obj <- methods::new("port",
                           universe_m_d_ref= uni_mdf,
                           port_construction_method="rp",
                           eligible_assets=uni_df$tickers,
                           exp_ret_score=NULL,
                           covariance_matrix=covmat,
                           correlation_matrix=NULL,
                           weights=w,
                           rel_risk_contr=relative_risk_contribution(w, covmat)$rel_risk_contr,
                           groups=NULL, mmaf_method=NULL, mmaf_group_col=NULL,
                           group_cov_matrix=NULL, micro=NULL, macro=NULL, port_name="k_bounds"
  )

  # k > N => equals sum of all weights = 1
  s1 <- calculate_port_stats(port_obj, top_k = 10L)
  testthat::expect_equal(s1$top_k_concentration, 1.0, tolerance = 1e-12)

  # k = 1 => equals largest weight
  s2 <- calculate_port_stats(port_obj, top_k = 1L)
  testthat::expect_equal(s2$top_k_concentration, max(w), tolerance = 1e-12)
})

