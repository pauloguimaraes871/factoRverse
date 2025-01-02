#Signals
test_that("classify_investment_universe works with no additional rules for signals (frequentist), respecting group representativeness", {

  #THEME SB
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Create signal_universe_m_d_ref
  set.seed(123)
  signals_universe_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                tickers = c("Alpha", "low_Beta", "Gamma"),
                                dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                mean_active_return = rnorm(3, 0, 1),
                                tracking_error = runif(3, 0, 1),
                                IR = rnorm(3,0,1),
                                alpha = rnorm(3,0,1),
                                alpha_t_stat = rnorm(3,0,1),
                                beta = rnorm(3,0,1),
                                treynor = rnorm(3,0,1),
                                p_value = c(0.05,0.20,0.03)
                                )

  signals_groups_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                       tickers = c("Alpha", "low_Beta", "Gamma"),
                                       dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                       theme = c("Value", "Momentum", "Value")
  )

  signals_universe_m_d_ref$adjusted_p_value <- p.adjust(signals_universe_m_d_ref$p_value, "none")
  signals_universe_m_d_ref$exp_ret_score <- signal_transform(signals_universe_m_d_ref$alpha, 0.99, 0.01)

  expected_results <- signals_universe_m_d_ref
  expected_results$top_assets <- c(1,0,1)


  #GET SE BENCHMARKS
  se_benchmarks <- create_se_benchmarks(expected_results, signals_groups_m_d_ref)

  expected_results$theme_ss_bench_weights <- se_benchmarks$theme_ss
  expected_results$theme_sb_bench_weights <- se_benchmarks$theme_sb
  expected_results$theme = signals_groups_m_d_ref$theme


  expected_results$is_eligible <- c(1,1,1)

  expect_equal(
    classify_investment_universe(signals_m_d_ref = signals_universe_m_d_ref, signal_significance_threshold = signal_significance_threshold,
                                 groups_m_d_ref = signals_groups_m_d_ref,
                                 concentration_constraint_policy = list(
                                   benchmark = c("theme_ss", "theme_sb"),
                                   max_abs_active_group_weight = 0.1
                                 ),
                                 asset_object = "signals"),

    expected_results
  )


})

test_that("classify_investment_universe works with no additional rules for signals (frequentist),
          respecting group representativeness when there are two competing unsignificant signals and
          when alpha is negative", {

            #THEME SB
            load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

            #Create signal_universe_m_d_ref
            set.seed(103)
            signals_universe_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15", "Delta-2001-07-15"),
                                                   tickers = c("Alpha", "low_Beta", "Gamma", "Delta"),
                                                   dates = c("2001-07-15", "2001-07-15", "2001-07-15", "2001-07-15"),
                                                   mean_active_return = rnorm(4, 0, 1),
                                                   tracking_error = runif(4, 0, 1),
                                                   IR = rnorm(4,0,1),
                                                   alpha = rnorm(4,0,1),
                                                   alpha_t_stat = rnorm(4,0,1),
                                                   beta = rnorm(4,0,1),
                                                   treynor = rnorm(4,0,1),
                                                   p_value = c(0.05,0.20,0.03, 0.10)
            )

            signals_groups_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15", "Delta-2001-07-15"),
                                                 tickers = c("Alpha", "low_Beta", "Gamma", "Delta"),
                                                 dates = c("2001-07-15", "2001-07-15", "2001-07-15", "2001-07-15"),
                                                 theme = c("Value", "Momentum", "Value", "Momentum")
            )


            signals_universe_m_d_ref$adjusted_p_value <- p.adjust(signals_universe_m_d_ref$p_value, "none")
            signals_universe_m_d_ref$exp_ret_score <- signal_transform(signals_universe_m_d_ref$alpha, 0.99, 0.01)

            expected_results <- signals_universe_m_d_ref
            expected_results$top_assets <- c(0,0,1,0)

            #GET SE BENCHMARKS
            se_benchmarks <- create_se_benchmarks(expected_results, signals_groups_m_d_ref)

            expected_results$theme_ss_bench_weights <- se_benchmarks$theme_ss
            expected_results$theme_sb_bench_weights <- se_benchmarks$theme_sb
            expected_results$theme = signals_groups_m_d_ref$theme


            expected_results$is_eligible <- c(0,0,1,1)

            expect_equal(
              classify_investment_universe(signals_m_d_ref = signals_universe_m_d_ref, signal_significance_threshold = signal_significance_threshold,
                                           groups_m_d_ref = signals_groups_m_d_ref,
                                           concentration_constraint_policy = list(
                                             benchmark = c("theme_ss", "theme_sb"),
                                             max_abs_active_group_weight = 0.1
                                           ),
                                           asset_object = "signals"),

              expected_results
            )

          })

test_that("classify_investment_universe works with no additional rules for signals (bayesian)", {

  #THEME SB
  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  set.seed(123)
  signals_universe_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                         tickers = c("Alpha", "low_Beta", "Gamma"),
                                         dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                         mean_active_return = rnorm(3, 0, 1),
                                         tracking_error = runif(3, 0, 1),
                                         IR = rnorm(3,0,1),
                                         theme_alpha = rnorm(3,0,1),
                                         individual_alpha = rnorm(3,0,1),
                                         alpha_se = rnorm(3,0,1),
                                         alpha_t_stat = rnorm(3,0,1),
                                         beta = rnorm(3,0,1),
                                         specific_risk = rnorm(3,0,1),
                                         treynor = rnorm(3,0,1),
                                         p_value = c(0.05,0.20,0.03),
                                         posterior_theme_alpha = rnorm(3,0,1),
                                         posterior_individual_alpha = rnorm(3,0,1),
                                         posterior_alpha_t_stat = rnorm(3,0,1),
                                         posterior_theme_beta = c(0.07, -0.05, 0.07),
                                         posterior_individual_beta = rnorm(3,0,0.02),
                                         posterior_treynor = rnorm(3,0,1),
                                         pd_theme_alpha = c(0.90,0.99,0.90),
                                         pd_alpha = c(0.99,0.75,0.99)
  )

  signals_groups_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                       tickers = c("Alpha", "low_Beta", "Gamma"),
                                       dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                       theme = c("Value", "Momentum", "Value")
  )


  signals_universe_m_d_ref$exp_ret_score <- signal_transform(signals_universe_m_d_ref$posterior_alpha, 0.99, 0.01)

  expected_results <- signals_universe_m_d_ref
  expected_results$top_assets <- c(1,0,1)


  #GET SE BENCHMARK
  se_benchmarks <- create_se_benchmarks(expected_results, signals_groups_m_d_ref)

  expected_results$theme_sb_bench_weights <- se_benchmarks$theme_sb
  expected_results$theme_ss_bench_weights <- se_benchmarks$theme_ss

  expected_results$theme = signals_groups_m_d_ref$theme


  expected_results$is_eligible <- c(1,1,1)

  expect_equal(
    classify_investment_universe(signals_m_d_ref = signals_universe_m_d_ref, signal_significance_threshold = 0.05,
                                 groups_m_d_ref = signals_groups_m_d_ref,
                                 concentration_constraint_policy = list(
                                   benchmark = c("theme_sb", "theme_ss"),
                                   max_abs_active_group_weight = 0.1
                                 ),
                                 asset_object = "signals"),

    expected_results
  )

})

#Stocks

test_that("classify_investment_universe works with no additional rules for stocks", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-07-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date),]
  signals_m_d_ref$exp_ret_score <- c(1.2, 0.8, 0.3, 0.2)

  expected_results <- signals_m_d_ref
  top_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.50)
  expected_results$top_assets <- c(1,1,0,0)
  expected_results$is_eligible <- c(1,1,0,0)

  expect_equal(
    classify_investment_universe(signals_m_d_ref = signals_m_d_ref, top_assets_quantile = 0.50),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule", {


  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-07-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date),]
  signals_m_d_ref$exp_ret_score <- c(1.2, 0.8, 0.3, 0.2)
  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date),]

  liquidity_floor_rule <- classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, liquidity_m_df = liquidity_m_df,
                                                   liquidity_floor_rule = liquidity_constraint_policy$liquidity_floor_rule, apply_liquidity_floor_rule = TRUE)


  expected_results <- signals_m_d_ref
  top_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.50)
  expected_results$top_assets <- c(1,1,0,0)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref$presence
  expected_results$liquidity_classification <- c("mid_caps", "micro_caps", "mid_caps", "mid_caps")
  expected_results$liquidity_floor <- c(1,1,1,1)
  expected_results$is_eligible <- c(1,1,0,0)
  rownames(expected_results) <- c(1L,2L,3L,4L)

  expect_equal(
    classify_investment_universe(signals_m_d_ref = signals_m_d_ref, top_assets_quantile = 0.50, liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list,
                                 liquidity_m_d_ref = liquidity_m_d_ref, liquidity_constraint_policy = liquidity_constraint_policy),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule and buffer_rule and that buffer_rule dominates liquidity_floor_rule", {

  #Create signals_m_d_ref_test
  signals_m_d_ref_test <- data.frame(
    tickers = c("Stock A", "Stock B", "Stock C"),
    signal_1 = c(1, -0.5, 0),
    signal_2 = c(-1, 0, 1),
    exp_ret_score = c(0.5, 0.25, 0.4)
  )

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 1000, presence = 97.5),
    small_caps = c(mean_volfin_3m = 5000, presence = 99),
    mid_caps = c(mean_volfin_3m = 25000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mega_caps = c(mean_volfin_3m = 500000, presence = 100)
  )

  #Create liquidity_m_d_ref_test
  liquidity_m_d_ref_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(1000, 6000, 24500),
    presence = c(100, 99, 100)
  )

  #Create old port weights test

  portfolio_weights_m_lstd_ref_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    old_portfolio_weights = c(0.2, 0.25, 0)
  )


  expected_results <- signals_m_d_ref_test
  top_quantile_buffer <- quantile(signals_m_d_ref_test$exp_ret_score, 0.50)
  expected_results$top_assets <- c(1,0,1)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref_test$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref_test$presence
  expected_results$liquidity_classification <- c("micro_caps", "small_caps", "small_caps")
  expected_results$liquidity_floor <- c(0,1,1)
  expected_results$old_portfolio_weights <- c(0.2, 0.25, 0)
  expected_results$buffer_zone_1 <- c(1,0,0)
  expected_results$is_eligible <- c(1,0,1)

  liquidity_constraint_policy <- list(liquidity_floor_rule = "small_caps")
  turnover_constraint_policy <- list(buffer_zone_1 = list(top_stock_quantile_buffer = 0.25, liquidity_classification = "micro_caps"))


  expect_equal(
    classify_investment_universe(signals_m_d_ref = signals_m_d_ref_test, top_assets_quantile = 0.50,
                                 liquidity_constraint_policy = liquidity_constraint_policy, liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_d_ref = liquidity_m_d_ref_test,
                                 turnover_constraint_policy = turnover_constraint_policy, portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref_test),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule and 2 buffer_zones", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Create signals_m_d_ref_test
  current_date <- "2001-04-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date),]
  signals_m_d_ref$exp_ret_score <- c(1.2,0.2,0,.04,3)

  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date),]


  #Create old port weights test
  last_date <- "2001-03-15"
  portfolio_weights_m_lstd_ref_test <- signals_m_df[which(signals_m_df$dates == last_date),c("id","tickers","dates")]
  portfolio_weights_m_lstd_ref_test$old_portfolio_weights <- c(0.5, 0, 0.5)

  expected_results <- signals_m_d_ref
  top_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.50)
  expected_results$top_assets <- c(1,1,0,0,1)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref$presence

  liquidity_classification_m_d_ref <-
    classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, liquidity_m_df = liquidity_m_d_ref,
                             liquidity_floor_rule = liquidity_constraint_policy$liquidity_floor_rule,apply_liquidity_floor_rule = TRUE)

  expected_results$liquidity_classification <- liquidity_classification_m_d_ref$liquidity_classification
  expected_results$liquidity_floor <- liquidity_classification_m_d_ref$liquidity_floor



  buffe_rule_m_d_ref <- apply_buffer_rule(signals_m_d_ref = signals_m_d_ref, top_assets_quantile_buffer = turnover_constraint_policy$buffer_zone_1$top_stock_quantile_buffer,
                                          portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref_test, liquidity_m_d_ref = liquidity_m_d_ref,
                                          liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, buffer_rule = turnover_constraint_policy$buffer_zone_1$liquidity_classification)


  expected_results$old_portfolio_weights <- c(NA, 0.5, NA, 0, 0.5)
  expected_results$buffer_zone_1 <- c(0,0,0,0,0)

  buffe_rule_m_d_ref <- apply_buffer_rule(signals_m_d_ref = signals_m_d_ref, top_assets_quantile_buffer = turnover_constraint_policy$buffer_zone_2$top_stock_quantile_buffer,
                                          portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref_test, liquidity_m_d_ref = liquidity_m_d_ref,
                                          liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, buffer_rule = turnover_constraint_policy$buffer_zone_2$liquidity_classification)


  expected_results$buffer_zone_2 <- c(0,0,0,0,1)

  expected_results$is_eligible <- c(1,1,0,0,1)
  rownames(expected_results) <- c(1L, 2L, 3L, 4L, 5L)


  expect_equal(
    classify_investment_universe(signals_m_d_ref = signals_m_d_ref, top_assets_quantile = 0.50,
                                 liquidity_constraint_policy = liquidity_constraint_policy, liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list,
                                 liquidity_m_d_ref = liquidity_m_d_ref,
                                 turnover_constraint_policy = turnover_constraint_policy, portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref_test),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule, 2 buffer_zones and max_max_abs_active_weight_individual_rule ", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Create signals_m_d_ref_test
  current_date <- "2001-04-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date),]
  signals_m_d_ref$exp_ret_score <- c(1.2,0.2,0,.04,3)

  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date),]


  #Create old port weights test
  last_date <- "2001-03-15"
  portfolio_weights_m_lstd_ref_test <- signals_m_df[which(signals_m_df$dates == last_date),c("id","tickers","dates")]
  portfolio_weights_m_lstd_ref_test$old_portfolio_weights <- c(0.5, 0, 0.5)

  expected_results <- signals_m_d_ref
  top_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.50)
  expected_results$top_assets <- c(1,1,0,0,1)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref$presence

  liquidity_classification_m_d_ref <-
    classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, liquidity_m_df = liquidity_m_d_ref,
                             liquidity_floor_rule = liquidity_constraint_policy$liquidity_floor_rule,apply_liquidity_floor_rule = TRUE)

  expected_results$liquidity_classification <- liquidity_classification_m_d_ref$liquidity_classification
  expected_results$liquidity_floor <- liquidity_classification_m_d_ref$liquidity_floor


  benchmark_weights_m_d_ref <- benchmark_weights_m_df[which(benchmark_weights_m_df$dates == current_date),]
  expected_results$IBOV_bench_weights <- benchmark_weights_m_d_ref$IBOV
  expected_results$max_abs_aw_ind <- c(1,1,1,1,1)



  buffe_rule_m_d_ref <- apply_buffer_rule(signals_m_d_ref = signals_m_d_ref, top_assets_quantile_buffer = turnover_constraint_policy$buffer_zone_1$top_stock_quantile_buffer,
                                          portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref_test, liquidity_m_d_ref = liquidity_m_d_ref,
                                          liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, buffer_rule = turnover_constraint_policy$buffer_zone_1$liquidity_classification)


  expected_results$old_portfolio_weights <- c(NA, 0.5, NA, 0, 0.5)
  expected_results$buffer_zone_1 <- c(0,0,0,0,0)

  buffe_rule_m_d_ref <- apply_buffer_rule(signals_m_d_ref = signals_m_d_ref, top_assets_quantile_buffer = turnover_constraint_policy$buffer_zone_2$top_stock_quantile_buffer,
                                          portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref_test, liquidity_m_d_ref = liquidity_m_d_ref,
                                          liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, buffer_rule = turnover_constraint_policy$buffer_zone_2$liquidity_classification)


  expected_results$buffer_zone_2 <- c(0,0,0,0,1)

  expected_results$is_eligible <- c(1,1,1,1,1)
  rownames(expected_results) <- c(1L, 2L, 3L, 4L, 5L)



  expect_equal(
    classify_investment_universe(signals_m_d_ref = signals_m_d_ref, top_assets_quantile = 0.50,
                                 liquidity_constraint_policy = liquidity_constraint_policy, liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list,
                                 liquidity_m_d_ref = liquidity_m_d_ref,
                                 turnover_constraint_policy = turnover_constraint_policy, portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref_test,
                                 concentration_constraint_policy = concentration_constraint_policy, benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
    ),
    expected_results
  )

})

test_that("classify_investment_universe works when concentration_constraint_policy is set but not max_abs_active_individual_weight (useful for signals universe)", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Create signals_m_d_ref_test
  current_date <- "2001-04-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date),]
  signals_m_d_ref$exp_ret_score <- c(1.2,0.2,0,.04,3)

  expected_results <- signals_m_d_ref
  top_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.50)
  expected_results$top_assets <- c(1,1,0,0,1)

  benchmark_weights_m_d_ref <- benchmark_weights_m_df[which(benchmark_weights_m_df$dates == current_date),]
  expected_results$IBOV_bench_weights <- benchmark_weights_m_d_ref$IBOV


  expected_results$is_eligible <- c(1,1,0,0,1)
  rownames(expected_results) <- c(1L, 2L, 3L, 4L, 5L)

  concentration_constraint_policy_test <- concentration_constraint_policy
  concentration_constraint_policy_test$max_abs_active_individual_weight <- NULL

  expect_equal(
    classify_investment_universe(signals_m_d_ref = signals_m_d_ref, top_assets_quantile = 0.50,
                                 concentration_constraint_policy = concentration_constraint_policy_test, benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
    ),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule, 2 buffer_zones and groups representativeness, picking the representative with highest exp_ret_score ", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Create signals_m_d_ref_test
  current_date <- "2001-04-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date),]
  signals_m_d_ref$exp_ret_score <- c(1.2,0.2,0,.04,3)

  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date),]
  benchmark_weights_m_d_ref <- benchmark_weights_m_df[which(benchmark_weights_m_df$dates == current_date),]



  #Create old port weights test
  last_date <- "2001-03-15"
  portfolio_weights_m_lstd_ref_test <- signals_m_df[which(signals_m_df$dates == last_date),c("id","tickers","dates")]
  portfolio_weights_m_lstd_ref_test$old_portfolio_weights <- c(0.5, 0, 0.5)

  expected_results <- signals_m_d_ref
  top_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.50)
  expected_results$top_assets <- c(1,1,0,0,1)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref$presence

  liquidity_classification_m_d_ref <-
    classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, liquidity_m_df = liquidity_m_d_ref,
                             liquidity_floor_rule = liquidity_constraint_policy$liquidity_floor_rule,apply_liquidity_floor_rule = TRUE)



  expected_results$liquidity_classification <- liquidity_classification_m_d_ref$liquidity_classification
  expected_results$liquidity_floor <- liquidity_classification_m_d_ref$liquidity_floor

  expected_results$IBOV_bench_weights <- benchmark_weights_m_d_ref$IBOV


  buffe_rule_m_d_ref <- apply_buffer_rule(signals_m_d_ref = signals_m_d_ref, top_assets_quantile_buffer = turnover_constraint_policy$buffer_zone_1$top_stock_quantile_buffer,
                                          portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref_test, liquidity_m_d_ref = liquidity_m_d_ref,
                                          liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, buffer_rule = turnover_constraint_policy$buffer_zone_1$liquidity_classification)


  expected_results$old_portfolio_weights <- c(NA, 0.5, NA, 0, 0.5)
  expected_results$buffer_zone_1 <- c(0,0,0,0,0)

  buffe_rule_m_d_ref <- apply_buffer_rule(signals_m_d_ref = signals_m_d_ref, top_assets_quantile_buffer = turnover_constraint_policy$buffer_zone_2$top_stock_quantile_buffer,
                                          portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref_test, liquidity_m_d_ref = liquidity_m_d_ref,
                                          liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, buffer_rule = turnover_constraint_policy$buffer_zone_2$liquidity_classification)


  expected_results$buffer_zone_2 <- c(0,0,0,0,1)

  stocks_groups_m_d_ref <- groups_m_df_list$stocks[which(groups_m_df_list$stocks$dates == current_date), ]
  stocks_groups_m_d_ref_test <- stocks_groups_m_d_ref
  stocks_groups_m_d_ref_test$Sector[4] <- "Financials"
  stocks_groups_m_d_ref_test$Subsector[4] <- "Insurance"


  expected_results$Sector <- stocks_groups_m_d_ref_test$Sector
  expected_results$Subsector <- stocks_groups_m_d_ref_test$Subsector



  expected_results$is_eligible <- c(1,1,0,1,1)
  rownames(expected_results) <- c(1L, 2L, 3L, 4L, 5L)


  concentration_constraint_policy_test <- concentration_constraint_policy
  concentration_constraint_policy_test$max_abs_active_individual_weight <- NULL

  expect_equal(
    classify_investment_universe(signals_m_d_ref = signals_m_d_ref, top_assets_quantile = 0.50,
                                 liquidity_constraint_policy = liquidity_constraint_policy, liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list,
                                 liquidity_m_d_ref = liquidity_m_d_ref,
                                 turnover_constraint_policy = turnover_constraint_policy, portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref_test,
                                 concentration_constraint_policy = concentration_constraint_policy_test, benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
                                 groups_m_d_ref = stocks_groups_m_d_ref_test
    ),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule, buffer_rule, max_abs_active_weight_individual_rule and user_defined_OR_rules", {

  #Create signals_m_d_ref_test
  signals_m_d_ref_test <- data.frame(
    tickers = c("Stock A", "Stock B", "Stock C"),
    signal_1 = c(1, -0.5, 0),
    signal_2 = c(-1, 0, 1),
    exp_ret_score = c(0.5, 0.25, 0.4)
  )

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 1000, presence = 97.5),
    small_caps = c(mean_volfin_3m = 5000, presence = 99),
    mid_caps = c(mean_volfin_3m = 25000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mega_caps = c(mean_volfin_3m = 500000, presence = 100)
  )

  #Create liquidity_m_d_ref_test
  liquidity_m_d_ref_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create old port weights test
  portfolio_weights_m_lstd_ref_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    old_portfolio_weights = c(0, 0.25, 0)
  )

  #Create selected_benchmark_weights_m_d_ref
  benchmark_weights_m_d_ref_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    IBOV = c(0.25, 0.50, 0.25)
  )

  #Create user_defined_OR_rules_list
  user_defined_OR_rules_list <- list(
    pretty_stocks = data.frame(tickers = c("Stock A", "Stock B", "Stock C"), beauty = c("pretty", "pretty", "ugly"), pretty_stocks = c(1,1,0)))



  expected_results <- signals_m_d_ref_test
  top_quantile_buffer <- quantile(signals_m_d_ref_test$exp_ret_score, 0.50)
  expected_results$top_assets <- c(1,0,1)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref_test$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref_test$presence
  expected_results$liquidity_classification <- c("nano_caps", "small_caps", "small_caps")
  expected_results$liquidity_floor <- c(0,1,1)
  expected_results$old_portfolio_weights <- c(0, 0.25, 0)
  expected_results$buffer_zone_1 <- c(0,0,0)
  expected_results$beauty <- c("pretty", "pretty", "ugly")
  expected_results$pretty_stocks <- c(1,1,0)

  expected_results$is_eligible <- c(1,1,1)

  liquidity_constraint_policy <- list(
    liquidity_floor_rule = "small_caps",
    liquidity_cap_rule = list(
      liquidity_classification = "small_caps",
      turnover_cap = 0.05
    )
  )

  turnover_constraint_policy <- list(
    buffer_zone_1 = list(
      liquidity_classification = "micro_caps",
      top_stock_quantile_buffer = 0.25,
      turnover_cap = 0.03
    )
  )


  expect_equal(
    classify_investment_universe(signals_m_d_ref = signals_m_d_ref_test, top_assets_quantile = 0.50,
                                 liquidity_constraint_policy = liquidity_constraint_policy, liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_d_ref = liquidity_m_d_ref_test,
                                 turnover_constraint_policy = turnover_constraint_policy, portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref_test,
                                 benchmark_weights_m_d_ref = benchmark_weights_m_d_ref_test, user_defined_OR_rules_list = user_defined_OR_rules_list),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule, buffer_rule, max_abs_active_weight_individual_rule and user_defined_OR_rules/user_defined_AND_rules", {

  #Create signals_m_d_ref_test
  signals_m_d_ref_test <- data.frame(
    tickers = c("Stock A", "Stock B", "Stock C"),
    signal_1 = c(1, -0.5, 0),
    signal_2 = c(-1, 0, 1),
    exp_ret_score = c(0.5, 0.25, 0.4)
  )

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 1000, presence = 97.5),
    small_caps = c(mean_volfin_3m = 5000, presence = 99),
    mid_caps = c(mean_volfin_3m = 25000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mega_caps = c(mean_volfin_3m = 500000, presence = 100)
  )

  #Create liquidity_m_d_ref_test
  liquidity_m_d_ref_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create old port weights test
  portfolio_weights_m_lstd_ref_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    old_portfolio_weights = c(0, 0.25, 0)
  )

  #Create selected_benchmark_weights_m_d_ref
  benchmark_weights_m_d_ref_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    IBOV = c(0.25, 0.50, 0.25)
  )

  #Create user_defined_OR_rules_list
  user_defined_OR_rules_list <- list(
    pretty_stocks = data.frame(tickers = c("Stock A", "Stock B", "Stock C"), beauty = c("pretty", "pretty", "ugly"), pretty_stocks = c(1,1,0)))

  #Create user_defined_AND_rules_list
  user_defined_AND_rules_list <- list(
    super_heroes = data.frame(tickers = c("Stock A", "Stock B", "Stock C"), heroes = c("penguim", "octopus", "joker"), super_heroes = c(0,0,0)))


  expected_results <- signals_m_d_ref_test
  top_quantile_buffer <- quantile(signals_m_d_ref_test$exp_ret_score, 0.50)
  expected_results$top_assets <- c(1,0,1)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref_test$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref_test$presence
  expected_results$liquidity_classification <- c("nano_caps", "small_caps", "small_caps")
  expected_results$liquidity_floor <- c(0,1,1)
  expected_results$IBOV_bench_weights <- c(0.25,0.50,0.25)
  expected_results$max_abs_aw_ind <- c(0,1,0)
  expected_results$old_portfolio_weights <- c(0, 0.25, 0)
  expected_results$buffer_zone_1 <- c(0,0,0)
  expected_results$beauty <- c("pretty", "pretty", "ugly")
  expected_results$pretty_stocks <- c(1,1,0)
  expected_results$heroes <- c("penguim", "octopus", "joker")
  expected_results$super_heroes <- c(0,0,0)

  expected_results$is_eligible <- c(0,0,0)

  liquidity_constraint_policy <- list(
    liquidity_floor_rule = "small_caps",
    liquidity_cap_rule = list(
      liquidity_classification = "small_caps",
      turnover_cap = 0.05
    )
  )

  turnover_constraint_policy <- list(
    buffer_zone_1 = list(
      liquidity_classification = "micro_caps",
      top_stock_quantile_buffer = 0.25,
      turnover_cap = 0.03
    )
  )

  concentration_constraint_policy <- list(
    benchmark = "IBOV",
    max_abs_active_individual_weight = 0.50
  )


  expect_equal(
    classify_investment_universe(signals_m_d_ref = signals_m_d_ref_test, top_assets_quantile = 0.50,
                                 liquidity_constraint_policy = liquidity_constraint_policy, liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_d_ref = liquidity_m_d_ref_test,
                                 turnover_constraint_policy = turnover_constraint_policy, portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref_test,
                                 benchmark_weights_m_d_ref = benchmark_weights_m_d_ref_test, concentration_constraint_policy = concentration_constraint_policy,
                                 user_defined_OR_rules_list = user_defined_OR_rules_list, user_defined_AND_rules_list = user_defined_AND_rules_list),
    expected_results
  )

})

test_that("classify_investment_universe works in metabacktest flow", {

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
  expected_results <- signal_results_list$stock_universe_m_d_ref
  expected_results$top_assets <- c(1,0,0,0)
  #Liquidity
  liquidity_floor_rule_m_d_ref <- classify_stock_liquidity(
    liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list,
    liquidity_m_df = liquidity_m_d_ref,
    liquidity_floor_rule = liquidity_constraint_policy$liquidity_floor_rule,
    apply_liquidity_floor_rule = TRUE
  )
  expected_results$mean_volfin_3m <- liquidity_m_d_ref$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref$presence

  expected_results$liquidity_classification <- liquidity_floor_rule_m_d_ref$liquidity_classification
  expected_results$liquidity_floor <- liquidity_floor_rule_m_d_ref$liquidity_floor
  #Bench weights
  expected_results$IBOV_bench_weights <- benchmark_weights_m_d_ref$IBOV
  expected_results$max_abs_aw_ind <- c(1,1,1,1)
  #Buffer Zones
  buffer_rule_m_d_ref <- apply_buffer_rule(signals_m_d_ref = expected_results,
                                           top_assets_quantile_buffer = turnover_constraint_policy$buffer_zone_1$top_stock_quantile_buffer,
                                           portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref,
                                           liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list,
                                           liquidity_m_d_ref = liquidity_m_d_ref,
                                           buffer_rule = turnover_constraint_policy$buffer_zone_1$liquidity_classification)

  expected_results$old_portfolio_weights <- c(0.20, 0.20, 0.20, 0.20)
  expected_results$buffer_zone_1 <- c(0,0,0,0)
  expected_results$buffer_zone_2 <- c(0,0,0,0)
  #Groups
  expected_results$Sector <- stocks_groups_m_d_ref$Sector
  expected_results$Subsector <- stocks_groups_m_d_ref$Subsector
  expected_results$is_eligible <- c(1,1,1,1)

  #Results
  results <- classify_investment_universe(
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

  rownames(results) <- NULL
  rownames(expected_results) <- NULL

  expect_equal(results, expected_results)


})


test_that("classify_investment_universe throws an error when no signals are significant", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #THEME SB
  signals_universe_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                         tickers = c("Alpha", "low_Beta", "Gamma"),
                                         dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                         mean_active_return = rnorm(3, 0, 1),
                                         tracking_error = runif(3, 0, 1),
                                         IR = rnorm(3,0,1),
                                         alpha = rnorm(3,0,1),
                                         alpha_t_stat = rnorm(3,0,1),
                                         beta = rnorm(3,0,1),
                                         treynor = rnorm(3,0,1),
                                         p_value = c(0.05,0.20,0.03)
  )

  signals_groups_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                       tickers = c("Alpha", "low_Beta", "Gamma"),
                                       dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                       theme = c("Value", "Momentum", "Value")
  )


  signals_universe_m_d_ref$adjusted_p_value <- p.adjust(signals_universe_m_d_ref$p_value, "BH")
  signals_universe_m_d_ref$exp_ret_score <- signal_transform(signals_universe_m_d_ref$alpha, 0.99, 0.01)

  expect_error(
    classify_investment_universe(signals_m_d_ref = signals_universe_m_d_ref, signal_significance_threshold = signal_selection_policy$signal_significance_threshold,
                                 groups_m_d_ref = signals_groups_m_d_ref,
                                 concentration_constraint_policy = list(
                                   benchmark = signal_selection_policy$sb_benchmark_weighting,
                                   max_abs_active_group_weight = signal_selection_policy$max_abs_active_group_weight
                                 ),
                                 asset_object = "signals"),
    "No signal was deemed significant."
  )

})
