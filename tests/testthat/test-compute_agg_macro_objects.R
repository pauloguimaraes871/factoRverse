test_that("compute_agg_magro_object works for happy path with cov matrix and liquidity m d ref", {

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


  #Covariance and returns
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]

  covariance_matrix <- estimate_covariance_matrix(tickers = c("Stock A", "Stock C", "Stock D", "Stock E"), returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  #Set RP
  rp_results <- riskParityPortfolio::riskParityPortfolio(Sigma = covariance_matrix)
  stock_universe_m_d_ref$rel_risk_contr <- rp_results$relative_risk_contribution
  stock_universe_m_d_ref$weights <- rp_results$w

  #Calculate group covariance matrix
  oil_universe_m_d_ref <- stock_universe_m_d_ref[1,]
  financials_universe_m_d_ref <- stock_universe_m_d_ref[2,]
  cyclical_universe_m_d_ref <- stock_universe_m_d_ref[c(3,4),]

  #Normalize
  oil_universe_m_d_ref$weights <- oil_universe_m_d_ref$weights/sum(oil_universe_m_d_ref$weights)
  financials_universe_m_d_ref$weights <- financials_universe_m_d_ref$weights/sum(financials_universe_m_d_ref$weights)
  cyclical_universe_m_d_ref$weights <- cyclical_universe_m_d_ref$weights/sum(cyclical_universe_m_d_ref$weights)

  #Compute group covariance matrix
  expect_group_cov <- matrix(NA_real_, nrow = 3, ncol = 3)
  rownames(expect_group_cov) <- c("Oil", "Financials", "Cyclical")
  colnames(expect_group_cov) <- c("Oil", "Financials", "Cyclical")

  #Oil x Oil
  expect_group_cov["Oil", "Oil"] <-
    t(oil_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% oil_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% oil_universe_m_d_ref$tickers] %*%
    oil_universe_m_d_ref$weights

  #Oil x Financials
  expect_group_cov["Oil", "Financials"] <-
    t(oil_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% oil_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% financials_universe_m_d_ref$tickers] %*%
    financials_universe_m_d_ref$weights

  #Oil x Cyclicals
  expect_group_cov["Oil", "Cyclical"] <-
    t(oil_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% oil_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers] %*%
    cyclical_universe_m_d_ref$weights

  #Financials x Oil
  expect_group_cov["Financials", "Oil"] <-
    t(financials_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% financials_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% oil_universe_m_d_ref$tickers] %*%
    oil_universe_m_d_ref$weights

  #Financials x Financials
  expect_group_cov["Financials", "Financials"] <-
    t(financials_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% financials_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% financials_universe_m_d_ref$tickers] %*%
    financials_universe_m_d_ref$weights

  #Financials x Cyclicals
  expect_group_cov["Financials", "Cyclical"] <-
    t(financials_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% financials_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers] %*%
    cyclical_universe_m_d_ref$weights

  #Cyclicals x Oil
  expect_group_cov["Cyclical", "Oil"] <-
    t(cyclical_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% oil_universe_m_d_ref$tickers] %*%
    oil_universe_m_d_ref$weights

  #Cyclicals x Financials
  expect_group_cov["Cyclical", "Financials"] <-
    t(cyclical_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% financials_universe_m_d_ref$tickers] %*%
    financials_universe_m_d_ref$weights

  #Cyclicals x Cyclicals
  expect_group_cov["Cyclical", "Cyclical"] <-
    t(cyclical_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers] %*%
    cyclical_universe_m_d_ref$weights

  #Re-order alphabetically
  expect_group_cov <- expect_group_cov[sort(rownames(expect_group_cov)), sort(colnames(expect_group_cov))]

  #Compute group universe
  group_universe_m_d_ref <- data.frame(
    id = paste0(c("Oil", "Financials", "Cyclical"), "-", current_date),
    tickers = c("Oil", "Financials", "Cyclical"),
    dates = as.Date(current_date),
    mean_volfin_3m = c(oil_universe_m_d_ref$mean_volfin_3m,
                       financials_universe_m_d_ref$mean_volfin_3m,
                       cyclical_universe_m_d_ref$mean_volfin_3m[1] * cyclical_universe_m_d_ref$weights[1] +
                         cyclical_universe_m_d_ref$mean_volfin_3m[2] * cyclical_universe_m_d_ref$weights[2]
    ),
    presence = c(oil_universe_m_d_ref$presence,
                 financials_universe_m_d_ref$presence,
                 cyclical_universe_m_d_ref$presence[1] * cyclical_universe_m_d_ref$weights[1] +
                   cyclical_universe_m_d_ref$presence[2] * cyclical_universe_m_d_ref$weights[2]
    ),
    ibov_bench_weights = c(oil_universe_m_d_ref$ibov_bench_weights,
                           financials_universe_m_d_ref$ibov_bench_weights,
                           cyclical_universe_m_d_ref$ibov_bench_weights[1] + cyclical_universe_m_d_ref$ibov_bench_weights[2]
    ),
    exp_ret_score = c(oil_universe_m_d_ref$exp_ret_score,
                      financials_universe_m_d_ref$exp_ret_score,
                      cyclical_universe_m_d_ref$exp_ret_score[1] * cyclical_universe_m_d_ref$weights[1] +
                        cyclical_universe_m_d_ref$exp_ret_score[2] * cyclical_universe_m_d_ref$weights[2]
    ),
    is_eligible = 1,
    weights = c(stock_universe_m_d_ref$weights[1], stock_universe_m_d_ref$weights[2],
                stock_universe_m_d_ref$weights[3] + stock_universe_m_d_ref$weights[4])
  ) %>%
    dplyr::arrange(tickers)

  group_liquidity_m_d_ref <- group_universe_m_d_ref %>% dplyr::select(id, tickers, dates, mean_volfin_3m, presence)

  results <- compute_agg_macro_objects(
    covariance_matrix = covariance_matrix,
    universe_m_d_ref = stock_universe_m_d_ref,
    group_col = "Sector",
    liquidity_m_d_ref = liquidity_m_d_ref
  )

  expect_equal(results$group_universe_m_d_ref, group_universe_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$group_liquidity_m_d_ref, group_liquidity_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$group_covariance_matrix, expect_group_cov, ignore_attr = TRUE)
  expect_equal(results$micro_universe_m_d_ref_list$Oil, oil_universe_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$micro_universe_m_d_ref_list$Financials, financials_universe_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$micro_universe_m_d_ref_list$Cyclical, cyclical_universe_m_d_ref, ignore_attr = TRUE)

  #Test that covariance matrix is ordered alphabetically
  expect_equal(rownames(results$group_covariance_matrix), sort(rownames(results$group_covariance_matrix)))
  expect_equal(colnames(results$group_covariance_matrix), sort(colnames(results$group_covariance_matrix)))

})

test_that("compute_agg_magro_object works for happy path without cov matrix and liquidity m d ref", {

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


  #Covariance and returns
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]

  covariance_matrix <- estimate_covariance_matrix(tickers = c("Stock A", "Stock C", "Stock D", "Stock E"), returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  #Set RP
  rp_results <- riskParityPortfolio::riskParityPortfolio(Sigma = covariance_matrix)
  stock_universe_m_d_ref$rel_risk_contr <- rp_results$relative_risk_contribution
  stock_universe_m_d_ref$weights <- rp_results$w

  #Calculate group covariance matrix
  oil_universe_m_d_ref <- stock_universe_m_d_ref[1,]
  financials_universe_m_d_ref <- stock_universe_m_d_ref[2,]
  cyclical_universe_m_d_ref <- stock_universe_m_d_ref[c(3,4),]

  #Normalize
  oil_universe_m_d_ref$weights <- oil_universe_m_d_ref$weights/sum(oil_universe_m_d_ref$weights)
  financials_universe_m_d_ref$weights <- financials_universe_m_d_ref$weights/sum(financials_universe_m_d_ref$weights)
  cyclical_universe_m_d_ref$weights <- cyclical_universe_m_d_ref$weights/sum(cyclical_universe_m_d_ref$weights)

  #Compute group universe
  group_universe_m_d_ref <- data.frame(
    id = paste0(c("Oil", "Financials", "Cyclical"), "-", current_date),
    tickers = c("Oil", "Financials", "Cyclical"),
    dates = as.Date(current_date),
    exp_ret_score = c(oil_universe_m_d_ref$exp_ret_score,
                      financials_universe_m_d_ref$exp_ret_score,
                      cyclical_universe_m_d_ref$exp_ret_score[1] * cyclical_universe_m_d_ref$weights[1] +
                        cyclical_universe_m_d_ref$exp_ret_score[2] * cyclical_universe_m_d_ref$weights[2]
    ),
    is_eligible = 1,
    weights = c(stock_universe_m_d_ref$weights[1], stock_universe_m_d_ref$weights[2],
                stock_universe_m_d_ref$weights[3] + stock_universe_m_d_ref$weights[4])
  ) %>%
    dplyr::arrange(tickers)

  results <- compute_agg_macro_objects(
    universe_m_d_ref = stock_universe_m_d_ref %>% dplyr::select(-ibov_bench_weights, -mean_volfin_3m, -presence),
    group_col = "Sector"
  )

  expect_equal(results$group_universe_m_d_ref, group_universe_m_d_ref, ignore_attr = TRUE)
  expect_null(results$group_liquidity_m_d_ref)
  expect_null(results$group_covariance_matrix)


})

test_that("compute_agg_magro_object works when micro_universe is provided (mmaf style)", {

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


  #Covariance and returns
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]

  covariance_matrix <- estimate_covariance_matrix(tickers = c("Stock A", "Stock C", "Stock D", "Stock E"), returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  #Set RP
  rp_results <- riskParityPortfolio::riskParityPortfolio(Sigma = covariance_matrix)
  stock_universe_m_d_ref$rel_risk_contr <- rp_results$relative_risk_contribution
  stock_universe_m_d_ref$weights <- rp_results$w

  #Force 0 weight
  stock_universe_m_d_ref$weights[c(3,4)] <- 0

  #Calculate group covariance matrix
  oil_universe_m_d_ref <- stock_universe_m_d_ref[1,]
  financials_universe_m_d_ref <- stock_universe_m_d_ref[2,]
  cyclical_universe_m_d_ref <- stock_universe_m_d_ref[c(3,4),]

  #Normalize
  oil_universe_m_d_ref$weights <- oil_universe_m_d_ref$weights/sum(oil_universe_m_d_ref$weights)
  financials_universe_m_d_ref$weights <- financials_universe_m_d_ref$weights/sum(financials_universe_m_d_ref$weights)
  cyclical_universe_m_d_ref$weights <- c(0.5, 0.5)  #Manually set since original weights were 0

  #Compute group covariance matrix
  expect_group_cov <- matrix(NA_real_, nrow = 3, ncol = 3)
  rownames(expect_group_cov) <- c("Oil", "Financials", "Cyclical")
  colnames(expect_group_cov) <- c("Oil", "Financials", "Cyclical")

  #Oil x Oil
  expect_group_cov["Oil", "Oil"] <-
    t(oil_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% oil_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% oil_universe_m_d_ref$tickers] %*%
    oil_universe_m_d_ref$weights

  #Oil x Financials
  expect_group_cov["Oil", "Financials"] <-
    t(oil_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% oil_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% financials_universe_m_d_ref$tickers] %*%
    financials_universe_m_d_ref$weights

  #Oil x Cyclicals
  expect_group_cov["Oil", "Cyclical"] <-
    t(oil_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% oil_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers] %*%
    cyclical_universe_m_d_ref$weights

  #Financials x Oil
  expect_group_cov["Financials", "Oil"] <-
    t(financials_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% financials_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% oil_universe_m_d_ref$tickers] %*%
    oil_universe_m_d_ref$weights

  #Financials x Financials
  expect_group_cov["Financials", "Financials"] <-
    t(financials_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% financials_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% financials_universe_m_d_ref$tickers] %*%
    financials_universe_m_d_ref$weights

  #Financials x Cyclicals
  expect_group_cov["Financials", "Cyclical"] <-
    t(financials_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% financials_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers] %*%
    cyclical_universe_m_d_ref$weights

  #Cyclicals x Oil
  expect_group_cov["Cyclical", "Oil"] <-
    t(cyclical_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% oil_universe_m_d_ref$tickers] %*%
    oil_universe_m_d_ref$weights

  #Cyclicals x Financials
  expect_group_cov["Cyclical", "Financials"] <-
    t(cyclical_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% financials_universe_m_d_ref$tickers] %*%
    financials_universe_m_d_ref$weights

  #Cyclicals x Cyclicals
  expect_group_cov["Cyclical", "Cyclical"] <-
    t(cyclical_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers] %*%
    cyclical_universe_m_d_ref$weights

  #Re-order alphabetically
  expect_group_cov <- expect_group_cov[sort(rownames(expect_group_cov)), sort(colnames(expect_group_cov))]

  #Compute group universe
  group_universe_m_d_ref <- data.frame(
    id = paste0(c("Oil", "Financials", "Cyclical"), "-", current_date),
    tickers = c("Oil", "Financials", "Cyclical"),
    dates = as.Date(current_date),
    mean_volfin_3m = c(oil_universe_m_d_ref$mean_volfin_3m,
                       financials_universe_m_d_ref$mean_volfin_3m,
                       cyclical_universe_m_d_ref$mean_volfin_3m[1] * cyclical_universe_m_d_ref$weights[1] +
                         cyclical_universe_m_d_ref$mean_volfin_3m[2] * cyclical_universe_m_d_ref$weights[2]
    ),
    presence = c(oil_universe_m_d_ref$presence,
                 financials_universe_m_d_ref$presence,
                 cyclical_universe_m_d_ref$presence[1] * cyclical_universe_m_d_ref$weights[1] +
                   cyclical_universe_m_d_ref$presence[2] * cyclical_universe_m_d_ref$weights[2]
    ),
    ibov_bench_weights = c(oil_universe_m_d_ref$ibov_bench_weights,
                           financials_universe_m_d_ref$ibov_bench_weights,
                           cyclical_universe_m_d_ref$ibov_bench_weights[1] + cyclical_universe_m_d_ref$ibov_bench_weights[2]
    ),
    exp_ret_score = c(oil_universe_m_d_ref$exp_ret_score,
                      financials_universe_m_d_ref$exp_ret_score,
                      cyclical_universe_m_d_ref$exp_ret_score[1] * cyclical_universe_m_d_ref$weights[1] +
                        cyclical_universe_m_d_ref$exp_ret_score[2] * cyclical_universe_m_d_ref$weights[2]
    ),
    is_eligible = 1
  ) %>%
    dplyr::arrange(tickers)

  group_liquidity_m_d_ref <- group_universe_m_d_ref %>% dplyr::select(id, tickers, dates, mean_volfin_3m, presence)

  expect_warning(
    results <- compute_agg_macro_objects(
      covariance_matrix = covariance_matrix,
      universe_m_d_ref = stock_universe_m_d_ref %>% dplyr::select(-weights),
      group_col = "Sector",
      liquidity_m_d_ref = liquidity_m_d_ref,
      micro_universe_m_d_ref_list = list(
        Oil = oil_universe_m_d_ref,
        Financials = financials_universe_m_d_ref,
        Cyclical = cyclical_universe_m_d_ref %>% dplyr::mutate(weights = 0)
      )
    ),
    "Weights for group 'Cyclical' sum to zero. Fallback to equal weights."
  )

  expect_equal(results$group_universe_m_d_ref, group_universe_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$group_liquidity_m_d_ref, group_liquidity_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$group_covariance_matrix, expect_group_cov, ignore_attr = TRUE)
  expect_equal(results$micro_universe_m_d_ref_list$Oil, oil_universe_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$micro_universe_m_d_ref_list$Financials, financials_universe_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$micro_universe_m_d_ref_list$Cyclical,
               cyclical_universe_m_d_ref %>% dplyr::mutate(weights = 0), ignore_attr = TRUE)

  #Test that covariance matrix is ordered alphabetically
  expect_equal(rownames(results$group_covariance_matrix), sort(rownames(results$group_covariance_matrix)))
  expect_equal(colnames(results$group_covariance_matrix), sort(colnames(results$group_covariance_matrix)))

})

test_that("compute_agg_magro_object works when a given group has no eligible tickers and errors accordingly", {

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

  #Force ineligibility
  stock_universe_m_d_ref$is_eligible[1] <- 0

  #Covariance and returns
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]

  covariance_matrix <- estimate_covariance_matrix(tickers = c("Stock C", "Stock D", "Stock E"), returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  #Set RP
  rp_results <- riskParityPortfolio::riskParityPortfolio(Sigma = covariance_matrix)
  stock_universe_m_d_ref$rel_risk_contr <- c(0, rp_results$relative_risk_contribution)
  stock_universe_m_d_ref$weights <- c(0, rp_results$w)

  #Calculate group covariance matrix
  financials_universe_m_d_ref <- stock_universe_m_d_ref[2,]
  cyclical_universe_m_d_ref <- stock_universe_m_d_ref[c(3,4),]

  #Normalize
  financials_universe_m_d_ref$weights <- financials_universe_m_d_ref$weights/sum(financials_universe_m_d_ref$weights)
  cyclical_universe_m_d_ref$weights <- cyclical_universe_m_d_ref$weights/sum(cyclical_universe_m_d_ref$weights)

  #Compute group covariance matrix
  expected_cov_matrix <- matrix(NA_real_, nrow = 2, ncol = 2)
  rownames(expected_cov_matrix) <- c("Financials", "Cyclical")
  colnames(expected_cov_matrix) <- c("Financials", "Cyclical")

  #Financials x Financials
  expected_cov_matrix["Financials", "Financials"] <-
    t(financials_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% financials_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% financials_universe_m_d_ref$tickers] %*%
    financials_universe_m_d_ref$weights

  #Financials x Cyclicals
  expected_cov_matrix["Financials", "Cyclical"] <-
    t(financials_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% financials_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers] %*%
    cyclical_universe_m_d_ref$weights

  #Cyclicals x Financials
  expected_cov_matrix["Cyclical", "Financials"] <-
    t(cyclical_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% financials_universe_m_d_ref$tickers] %*%
    financials_universe_m_d_ref$weights

  #Cyclicals x Cyclicals
  expected_cov_matrix["Cyclical", "Cyclical"] <-
    t(cyclical_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers] %*%
    cyclical_universe_m_d_ref$weights

  #Re-order alphabetically
  expected_cov_matrix <- expected_cov_matrix[sort(rownames(expected_cov_matrix)), sort(colnames(expected_cov_matrix))]

  #Compute group universe
  group_universe_m_d_ref <- data.frame(
    id = paste0(c("Financials", "Cyclical"), "-", current_date),
    tickers = c("Financials", "Cyclical"),
    dates = as.Date(current_date),
    mean_volfin_3m = c(financials_universe_m_d_ref$mean_volfin_3m,
                       cyclical_universe_m_d_ref$mean_volfin_3m[1] * cyclical_universe_m_d_ref$weights[1] +
                         cyclical_universe_m_d_ref$mean_volfin_3m[2] * cyclical_universe_m_d_ref$weights[2]
    ),
    presence = c(financials_universe_m_d_ref$presence,
                 cyclical_universe_m_d_ref$presence[1] * cyclical_universe_m_d_ref$weights[1] +
                   cyclical_universe_m_d_ref$presence[2] * cyclical_universe_m_d_ref$weights[2]
    ),
    ibov_bench_weights = c(financials_universe_m_d_ref$ibov_bench_weights,
                           cyclical_universe_m_d_ref$ibov_bench_weights[1] + cyclical_universe_m_d_ref$ibov_bench_weights[2]
    ),
    exp_ret_score = c(financials_universe_m_d_ref$exp_ret_score,
                      cyclical_universe_m_d_ref$exp_ret_score[1] * cyclical_universe_m_d_ref$weights[1] +
                        cyclical_universe_m_d_ref$exp_ret_score[2] * cyclical_universe_m_d_ref$weights[2]
    ),
    is_eligible = 1,
    weights = c(stock_universe_m_d_ref$weights[2],
                stock_universe_m_d_ref$weights[3] + stock_universe_m_d_ref$weights[4])
  ) %>%
    dplyr::arrange(tickers)

  group_liquidity_m_d_ref <- group_universe_m_d_ref %>% dplyr::select(id, tickers, dates, mean_volfin_3m, presence)

  results <- compute_agg_macro_objects(
    covariance_matrix = covariance_matrix,
    universe_m_d_ref = stock_universe_m_d_ref,
    group_col = "Sector",
    liquidity_m_d_ref = liquidity_m_d_ref
  )

  expect_equal(results$group_universe_m_d_ref, group_universe_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$group_liquidity_m_d_ref, group_liquidity_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$group_covariance_matrix, expected_cov_matrix, ignore_attr = TRUE)
  expect_equal(results$micro_universe_m_d_ref_list$Financials, financials_universe_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$micro_universe_m_d_ref_list$Cyclical, cyclical_universe_m_d_ref, ignore_attr = TRUE)

  #Test that covariance matrix is ordered alphabetically
  expect_equal(rownames(results$group_covariance_matrix), sort(rownames(results$group_covariance_matrix)))
  expect_equal(colnames(results$group_covariance_matrix), sort(colnames(results$group_covariance_matrix)))

  # Trying to pass a group with no eligible tickers should return an error
  expect_error(
    compute_agg_macro_objects(
      covariance_matrix = covariance_matrix,
      universe_m_d_ref = stock_universe_m_d_ref %>% dplyr::mutate(is_eligible = 0),
      group_col = "Sector",
      liquidity_m_d_ref = liquidity_m_d_ref
    ), "Row names of covariance_matrix must match eligible tickers."
  )


  covariance_matrix <- estimate_covariance_matrix(tickers = c("Stock A", "Stock C", "Stock D", "Stock E"), returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  expect_error(
    compute_agg_macro_objects(
      covariance_matrix = covariance_matrix,
      universe_m_d_ref = stock_universe_m_d_ref,
      group_col = "Sector",
      liquidity_m_d_ref = liquidity_m_d_ref
      ),"Row names of covariance_matrix must match eligible tickers."
  )




})

test_that("compute_agg_magro_object works for group with weights equal to 0", {

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


  #Covariance and returns
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]

  covariance_matrix <- estimate_covariance_matrix(tickers = c("Stock A", "Stock C", "Stock D", "Stock E"), returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  #Set RP
  rp_results <- riskParityPortfolio::riskParityPortfolio(Sigma = covariance_matrix)
  stock_universe_m_d_ref$rel_risk_contr <- rp_results$relative_risk_contribution
  stock_universe_m_d_ref$weights <- rp_results$w

  #Force weight = 0
  stock_universe_m_d_ref$weights[3:4] <- 0
  stock_universe_m_d_ref$weights[1:2] <- stock_universe_m_d_ref$weights[1:2]/sum(stock_universe_m_d_ref$weights[1:2])

  #Calculate group covariance matrix
  oil_universe_m_d_ref <- stock_universe_m_d_ref[1,]
  financials_universe_m_d_ref <- stock_universe_m_d_ref[2,]
  cyclical_universe_m_d_ref <- stock_universe_m_d_ref[c(3,4),]

  #Normalize
  oil_universe_m_d_ref$weights <- oil_universe_m_d_ref$weights/sum(oil_universe_m_d_ref$weights)
  financials_universe_m_d_ref$weights <- financials_universe_m_d_ref$weights/sum(financials_universe_m_d_ref$weights)
  cyclical_universe_m_d_ref$weights <- c(0.5, 0.5)

  #Compute group covariance matrix
  expected_cov_matrix <- matrix(NA_real_, nrow = 3, ncol = 3)
  rownames(expected_cov_matrix) <- c("Oil", "Financials", "Cyclical")
  colnames(expected_cov_matrix) <- c("Oil", "Financials", "Cyclical")

  #Oil x Oil
  expected_cov_matrix["Oil", "Oil"] <-
    t(oil_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% oil_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% oil_universe_m_d_ref$tickers] %*%
    oil_universe_m_d_ref$weights

  #Oil x Financials
  expected_cov_matrix["Oil", "Financials"] <-
    t(oil_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% oil_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% financials_universe_m_d_ref$tickers] %*%
    financials_universe_m_d_ref$weights

  #Oil x Cyclicals
  expected_cov_matrix["Oil", "Cyclical"] <-
    t(oil_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% oil_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers] %*%
    cyclical_universe_m_d_ref$weights

  #Financials x Oil
  expected_cov_matrix["Financials", "Oil"] <-
    t(financials_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% financials_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% oil_universe_m_d_ref$tickers] %*%
    oil_universe_m_d_ref$weights

  #Financials x Financials
  expected_cov_matrix["Financials", "Financials"] <-
    t(financials_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% financials_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% financials_universe_m_d_ref$tickers] %*%
    financials_universe_m_d_ref$weights

  #Financials x Cyclicals
  expected_cov_matrix["Financials", "Cyclical"] <-
    t(financials_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% financials_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers] %*%
    cyclical_universe_m_d_ref$weights

  #Cyclicals x Oil
  expected_cov_matrix["Cyclical", "Oil"] <-
    t(cyclical_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% oil_universe_m_d_ref$tickers] %*%
    oil_universe_m_d_ref$weights

  #Cyclicals x Financials
  expected_cov_matrix["Cyclical", "Financials"] <-
    t(cyclical_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% financials_universe_m_d_ref$tickers] %*%
    financials_universe_m_d_ref$weights

  #Cyclicals x Cyclicals
  expected_cov_matrix["Cyclical", "Cyclical"] <-
    t(cyclical_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers] %*%
    cyclical_universe_m_d_ref$weights

  #Re-order alphabetically
  expected_cov_matrix <- expected_cov_matrix[sort(rownames(expected_cov_matrix)), sort(colnames(expected_cov_matrix))]

  #Compute group universe
  group_universe_m_d_ref <- data.frame(
    id = paste0(c("Oil", "Financials", "Cyclical"), "-", current_date),
    tickers = c("Oil", "Financials", "Cyclical"),
    dates = as.Date(current_date),
    mean_volfin_3m = c(oil_universe_m_d_ref$mean_volfin_3m,
                       financials_universe_m_d_ref$mean_volfin_3m,
                       cyclical_universe_m_d_ref$mean_volfin_3m[1] * cyclical_universe_m_d_ref$weights[1] +
                         cyclical_universe_m_d_ref$mean_volfin_3m[2] * cyclical_universe_m_d_ref$weights[2]
    ),
    presence = c(oil_universe_m_d_ref$presence,
                 financials_universe_m_d_ref$presence,
                 cyclical_universe_m_d_ref$presence[1] * cyclical_universe_m_d_ref$weights[1] +
                   cyclical_universe_m_d_ref$presence[2] * cyclical_universe_m_d_ref$weights[2]
    ),
    ibov_bench_weights = c(oil_universe_m_d_ref$ibov_bench_weights,
                           financials_universe_m_d_ref$ibov_bench_weights,
                           cyclical_universe_m_d_ref$ibov_bench_weights[1] + cyclical_universe_m_d_ref$ibov_bench_weights[2]
    ),
    exp_ret_score = c(oil_universe_m_d_ref$exp_ret_score,
                      financials_universe_m_d_ref$exp_ret_score,
                      cyclical_universe_m_d_ref$exp_ret_score[1] * cyclical_universe_m_d_ref$weights[1] +
                        cyclical_universe_m_d_ref$exp_ret_score[2] * cyclical_universe_m_d_ref$weights[2]
    ),
    is_eligible = 1,
    weights = c(stock_universe_m_d_ref$weights[1], stock_universe_m_d_ref$weights[2],
                stock_universe_m_d_ref$weights[3] + stock_universe_m_d_ref$weights[4])
  ) %>%
    dplyr::arrange(tickers)

  group_liquidity_m_d_ref <- group_universe_m_d_ref %>% dplyr::select(id, tickers, dates, mean_volfin_3m, presence)

  expect_warning(
  results <- compute_agg_macro_objects(
    covariance_matrix = covariance_matrix,
    universe_m_d_ref = stock_universe_m_d_ref,
    group_col = "Sector",
    liquidity_m_d_ref = liquidity_m_d_ref
  ),
  "Weights for group 'Cyclical' sum to zero. Fallback to equal weights."
  )

  expect_equal(results$group_universe_m_d_ref, group_universe_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$group_liquidity_m_d_ref, group_liquidity_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$group_covariance_matrix, expected_cov_matrix, ignore_attr = TRUE)
  expect_equal(results$micro_universe_m_d_ref_list$Oil, oil_universe_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$micro_universe_m_d_ref_list$Financials, financials_universe_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$micro_universe_m_d_ref_list$Cyclical, cyclical_universe_m_d_ref, ignore_attr = TRUE)

  #Test that covariance matrix is ordered alphabetically
  expect_equal(rownames(results$group_covariance_matrix), sort(rownames(results$group_covariance_matrix)))
  expect_equal(colnames(results$group_covariance_matrix), sort(colnames(results$group_covariance_matrix)))



})

test_that("compute_agg_macro_objects works when is_eligible < number of total assets when computing group weights", {

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

  #Force inelegibility
  stock_universe_m_d_ref$is_eligible[c(2,4)] <- 0


  #Covariance and returns
  daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date),]

  covariance_matrix <- estimate_covariance_matrix(tickers = c("Stock A", "Stock D"), returns_m_xts_upd_ref = daily_stock_returns_m_xts_upd_ref,
                                                  cov_matrix_sample_size = 252, cov_estimation_method = covariance_estimation_method,
                                                  active_returns = FALSE,
                                                  groups_m_d_ref = stock_groups_m_d_ref
  )

  #Set RP
  rp_results <- riskParityPortfolio::riskParityPortfolio(Sigma = covariance_matrix)
  stock_universe_m_d_ref$rel_risk_contr <- rp_results$relative_risk_contribution
  stock_universe_m_d_ref$weights <- rp_results$w

  #Calculate group covariance matrix
  oil_universe_m_d_ref <- stock_universe_m_d_ref[1,]
  cyclical_universe_m_d_ref <- stock_universe_m_d_ref[c(3),]

  #Normalize
  oil_universe_m_d_ref$weights <- oil_universe_m_d_ref$weights/sum(oil_universe_m_d_ref$weights)
  cyclical_universe_m_d_ref$weights <- cyclical_universe_m_d_ref$weights/sum(cyclical_universe_m_d_ref$weights)

  #Compute group covariance matrix
  expect_group_cov <- matrix(NA_real_, nrow = 2, ncol = 2)
  rownames(expect_group_cov) <- c("Oil", "Cyclical")
  colnames(expect_group_cov) <- c("Oil", "Cyclical")

  #Oil x Oil
  expect_group_cov["Oil", "Oil"] <-
    t(oil_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% oil_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% oil_universe_m_d_ref$tickers] %*%
    oil_universe_m_d_ref$weights

  #Oil x Cyclicals
  expect_group_cov["Oil", "Cyclical"] <-
    t(oil_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% oil_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers] %*%
    cyclical_universe_m_d_ref$weights

  #Cyclicals x Oil
  expect_group_cov["Cyclical", "Oil"] <-
    t(cyclical_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% oil_universe_m_d_ref$tickers] %*%
    oil_universe_m_d_ref$weights

  #Cyclicals x Cyclicals
  expect_group_cov["Cyclical", "Cyclical"] <-
    t(cyclical_universe_m_d_ref$weights) %*%
    covariance_matrix[rownames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers, colnames(covariance_matrix) %in% cyclical_universe_m_d_ref$tickers] %*%
    cyclical_universe_m_d_ref$weights

  #Re-order alphabetically
  expect_group_cov <- expect_group_cov[sort(rownames(expect_group_cov)), sort(colnames(expect_group_cov))]

  #Compute group universe
  group_universe_m_d_ref <- data.frame(
    id = paste0(c("Oil", "Cyclical"), "-", current_date),
    tickers = c("Oil", "Cyclical"),
    dates = as.Date(current_date),
    mean_volfin_3m = c(oil_universe_m_d_ref$mean_volfin_3m,
                       cyclical_universe_m_d_ref$mean_volfin_3m),
    presence = c(oil_universe_m_d_ref$presence,
                 cyclical_universe_m_d_ref$presence
    ),
    ibov_bench_weights = c(oil_universe_m_d_ref$ibov_bench_weights,
                           stock_universe_m_d_ref$ibov_bench_weights[3] + stock_universe_m_d_ref$ibov_bench_weights[4]
    ),
    exp_ret_score = c(oil_universe_m_d_ref$exp_ret_score,
                      cyclical_universe_m_d_ref$exp_ret_score
    ),
    is_eligible = 1,
    weights = c(stock_universe_m_d_ref$weights[1],
                stock_universe_m_d_ref$weights[3])
  ) %>%
    dplyr::arrange(tickers)

  group_liquidity_m_d_ref <- group_universe_m_d_ref %>% dplyr::select(id, tickers, dates, mean_volfin_3m, presence)

    results <- compute_agg_macro_objects(
      covariance_matrix = covariance_matrix,
      universe_m_d_ref = stock_universe_m_d_ref,
      group_col = "Sector",
      liquidity_m_d_ref = liquidity_m_d_ref
    )

  expect_equal(results$group_universe_m_d_ref, group_universe_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$group_liquidity_m_d_ref, group_liquidity_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$group_covariance_matrix, expect_group_cov, ignore_attr = TRUE)
  expect_equal(results$micro_universe_m_d_ref_list$Oil, oil_universe_m_d_ref, ignore_attr = TRUE)
  expect_equal(results$micro_universe_m_d_ref_list$Cyclical, cyclical_universe_m_d_ref, ignore_attr = TRUE)

  #Test that covariance matrix is ordered alphabetically
  expect_equal(rownames(results$group_covariance_matrix), sort(rownames(results$group_covariance_matrix)))
  expect_equal(colnames(results$group_covariance_matrix), sort(colnames(results$group_covariance_matrix)))






})

test_that("errors if covariance_matrix is not a numeric matrix", {
  groups <- c("A","B")
  eligible <- data.frame(
    tickers = c("s1","s2"),
    Sector  = c("A","B"),
    weights = c(0.6, 0.4),
    is_eligible = c(1,1),
    stringsAsFactors = FALSE
  )
  bad_Sigma <- as.data.frame(matrix(1, 2, 2))
  rownames(bad_Sigma) <- colnames(bad_Sigma) <- eligible$tickers

  testthat::expect_error(
    compute_agg_macro_objects(
      universe_m_d_ref = eligible,
      covariance_matrix = bad_Sigma,   # not a matrix
      group_col = "Sector"
    ),
    "covariance_matrix must be a numeric matrix\\."
  )
})

test_that("errors if covariance_matrix has no row/col names", {
  groups <- c("A","B")
  eligible <- data.frame(
    tickers = c("s1","s2"),
    Sector  = c("A","B"),
    weights = c(0.6, 0.4),
    is_eligible = c(1,1),
    stringsAsFactors = FALSE
  )
  Sigma <- matrix(c(0.01,0.002,0.002,0.02), 2, 2)  # no dimnames

  expect_error(
    compute_agg_macro_objects(
      universe_m_d_ref = eligible,
      covariance_matrix = Sigma,
      group_col = "Sector"
    ),
    "covariance_matrix must have row and column names \\(tickers\\)\\."
  )
})

test_that("errors if covariance_matrix rownames != eligible tickers", {
  groups <- c("A","B")
  eligible <- data.frame(
    tickers = c("s1","s2"),
    Sector  = c("A","B"),
    weights = c(0.6, 0.4),
    is_eligible = c(1,1),
    stringsAsFactors = FALSE
  )
  Sigma <- matrix(c(0.01,0.002,0.002,0.02), 2, 2)
  rownames(Sigma) <- colnames(Sigma) <- c("s1","sX")  # mismatch

  expect_error(
    compute_agg_macro_objects(
      universe_m_d_ref = eligible,
      covariance_matrix = Sigma,
      group_col = "Sector"
    ),
    "Row names of covariance_matrix must match eligible tickers."
  )
})

test_that("errors if group_col missing in groups_m_d_ref or eligible", {
  groups <- c("A","B")
  eligible <- data.frame(
    tickers = c("s1","s2"),
    SectorX = c("A","B"),   # wrong column name on purpose
    weights = c(0.6, 0.4),
    is_eligible = c(1,1),
    stringsAsFactors = FALSE
  )
  groups_map <- data.frame(
    tickers = c("s1","s2"),
    SectorX = c("A","B"),   # wrong column name on purpose
    stringsAsFactors = FALSE
  )
  Sigma <- matrix(c(0.01,0.002,0.002,0.02), 2, 2)
  rownames(Sigma) <- colnames(Sigma) <- c("s1","s2")

  # group_col not found in eligible_universe_m_d_ref
  expect_error(
    compute_agg_macro_objects(
      universe_m_d_ref = eligible,
      covariance_matrix = Sigma,
      group_col = "Sector"
    ),
    "group_col 'Sector' not found in eligible_universe_m_d_ref\\."
  )
})

