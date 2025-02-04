#Signals
test_that("classify_investment_universe works with no additional rules for signals (frequentist), respecting group representativeness", {

  #THEME SB
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  signal_significance_threshold <- 0.1

  #Create signal_universe_m_d_ref
  set.seed(123)
  signal_universe_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
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

  signal_groups_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                      tickers = c("Alpha", "low_Beta", "Gamma"),
                                      dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                      theme = c("Value", "Momentum", "Value")
  )

  signal_universe_m_d_ref$adjusted_p_value <- p.adjust(signal_universe_m_d_ref$p_value, "none")
  signal_universe_m_d_ref$exp_ret_score <- signal_transform(signal_universe_m_d_ref$alpha, 0.01, 0.99)

  expected_results <- signal_universe_m_d_ref
  expected_results$pre_eligible_assets <- c(1,0,1)


  #GET SE BENCHMARKS
  se_benchmarks <- create_se_benchmarks(expected_results, signal_groups_m_d_ref)

  expected_results$theme_ss_bench_weights <- se_benchmarks$theme_ss
  expected_results$theme_sb_bench_weights <- se_benchmarks$theme_sb
  expected_results$theme = signal_groups_m_d_ref$theme


  expected_results$is_eligible <- c(1,1,1)

  expect_equal(
    classify_investment_universe(universe_m_d_ref = signal_universe_m_d_ref, signal_significance_threshold = signal_significance_threshold,
                                 groups_m_d_ref = signal_groups_m_d_ref,
                                 concentration_constraint_policy = list(
                                   benchmark = c("theme_ss", "theme_sb"),
                                   max_abs_active_group_weight = 0.1
                                 ),
                                 asset_object = "signals"),
    expected_results
  )


})

test_that("classify_investment_universe works with no additional rules for signals (frequentist) in presence of NAs", {

  #THEME SB
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  signal_significance_threshold <- 0.1

  #Create signal_universe_m_d_ref
  set.seed(123)
  signal_universe_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
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

  signal_universe_m_d_ref$alpha[2] <- NA
  signal_universe_m_d_ref$p_value[2] <- NA


  signal_groups_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                       tickers = c("Alpha", "low_Beta", "Gamma"),
                                       dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                       theme = c("Value", "Momentum", "Value")
  )

  expect_error(
    classify_investment_universe(universe_m_d_ref = signal_universe_m_d_ref, signal_significance_threshold = signal_significance_threshold,
                                 groups_m_d_ref = signal_groups_m_d_ref,
                                 concentration_constraint_policy = list(
                                   benchmark = c("theme_ss", "theme_sb"),
                                   max_abs_active_group_weight = NULL
                                 ),
                                 asset_object = "signals")
  )


})

test_that("classify_investment_universe works with no additional rules for signals (frequentist),respecting group representativeness when there are two competing unsignificant signals and
          when alpha is negative", {

            #THEME SB
            load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))
            signal_significance_threshold <- 0.05

            #Create signal_universe_m_d_ref
            set.seed(103)
            signal_universe_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15", "Delta-2001-07-15"),
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

            signal_groups_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15", "Delta-2001-07-15"),
                                                 tickers = c("Alpha", "low_Beta", "Gamma", "Delta"),
                                                 dates = c("2001-07-15", "2001-07-15", "2001-07-15", "2001-07-15"),
                                                 theme = c("Value", "Momentum", "Value", "Momentum")
            )


            signal_universe_m_d_ref$adjusted_p_value <- p.adjust(signal_universe_m_d_ref$p_value, "none")
            signal_universe_m_d_ref$exp_ret_score <- signal_transform(signal_universe_m_d_ref$alpha, 0.01, 0.99)

            expected_results <- signal_universe_m_d_ref
            expected_results$pre_eligible_assets <- c(0,0,1,0)

            #GET SE BENCHMARKS
            se_benchmarks <- create_se_benchmarks(expected_results, signal_groups_m_d_ref)

            expected_results$theme_ss_bench_weights <- se_benchmarks$theme_ss
            expected_results$theme_sb_bench_weights <- se_benchmarks$theme_sb
            expected_results$theme = signal_groups_m_d_ref$theme


            expected_results$is_eligible <- c(0,0,1,1)

            expect_equal(
              classify_investment_universe(universe_m_d_ref = signal_universe_m_d_ref, signal_significance_threshold = signal_significance_threshold,
                                           groups_m_d_ref = signal_groups_m_d_ref,
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
  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  signal_significance_threshold <- 0.05

  set.seed(123)
  signal_universe_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
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

  signal_groups_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                       tickers = c("Alpha", "low_Beta", "Gamma"),
                                       dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                       theme = c("Value", "Momentum", "Value")
  )


  signal_universe_m_d_ref$exp_ret_score <- signal_transform(signal_universe_m_d_ref$posterior_alpha, 0.01,  0.99)

  expected_results <- signal_universe_m_d_ref
  expected_results$pre_eligible_assets <- c(1,0,1)


  #GET SE BENCHMARK
  se_benchmarks <- create_se_benchmarks(expected_results, signal_groups_m_d_ref)

  expected_results$theme_sb_bench_weights <- se_benchmarks$theme_sb
  expected_results$theme_ss_bench_weights <- se_benchmarks$theme_ss

  expected_results$theme = signal_groups_m_d_ref$theme


  expected_results$is_eligible <- c(1,1,1)

  expect_equal(
    classify_investment_universe(universe_m_d_ref = signal_universe_m_d_ref, signal_significance_threshold = 0.05,
                                 groups_m_d_ref = signal_groups_m_d_ref,
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

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  current_date <- "2001-07-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date),]
  signals_m_d_ref$exp_ret_score <- c(1.2, 0.8, 0.3, 0.2)
  stock_universe_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates, exp_ret_score)

  expected_results <- stock_universe_m_d_ref
  eligibility_quantile_range <- c(0.50, 1.0)
  upper_bound_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 1.0)
  lower_bound_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.5)

  expected_results$pre_eligible_assets <- c(1,1,0,0)
  expected_results$is_eligible <- c(1,1,0,0)

  expect_equal(
    classify_investment_universe(universe_m_d_ref = stock_universe_m_d_ref, eligibility_quantile_range = c(0.50, 1.0)),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule", {

  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  current_date <- "2001-07-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date),]
  signals_m_d_ref$exp_ret_score <- c(1.2, 0.8, 0.3, 0.2)
  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date),]

  liquidity_floor_rule <- classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, liquidity_m_df = liquidity_m_d_ref,
                                                   liquidity_floor_rule = liquidity_constraint_policy$liquidity_floor_rule, apply_liquidity_floor_rule = TRUE)

  expected_results <- signals_m_d_ref
  eligibility_quantile_range <- c(0.50, 0.75)
  upper_bound_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.75)
  lower_bound_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.50)


  expected_results$pre_eligible_assets <- c(0,1,0,0)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref$presence
  expected_results$liquidity_classification <- c("mid_caps", "micro_caps", "mid_caps", "mid_caps")
  expected_results$liquidity_floor <- c(1,1,1,1)
  expected_results$is_eligible <- c(0,1,0,0)
  rownames(expected_results) <- c(1L,2L,3L,4L)

  expect_equal(
    classify_investment_universe(universe_m_d_ref = signals_m_d_ref, eligibility_quantile_range = eligibility_quantile_range, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
                                 liquidity_m_d_ref = liquidity_m_d_ref, liquidity_constraint_policy = liquidity_constraint_policy),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule and turnover_cap_rule and that turnover_cap_rule dominates liquidity_floor_rule", {

  #Create signals_m_d_ref
  signals_m_d_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = c("2020-05-15", "2020-05-15", "2020-05-15"),
    signal_1 = c(1, -0.5, 0),
    signal_2 = c(-1, 0, 1),
    exp_ret_score = c(0.5, 0.25, 0.4)
  )

  #Create cutoff
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  #Create liquidity_m_d_ref
  liquidity_m_d_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(1000, 6000, 24500),
    presence = c(100, 99, 100)
  )

  #Create old port weights test
  updated_port_weights_m_lstd_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    bop_port_weights = c(0.2, 0.25, 0)
  )


  stock_universe_m_d_ref <- signals_m_d_ref %>% dplyr::select(-signal_1, -signal_2)
  expected_results <- stock_universe_m_d_ref
  eligiblity_quantile_range <- c(0.50, 1.0)
  upper_bound_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 1.0)
  lower_bound_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.50)

  expected_results$pre_eligible_assets <- c(1,0,1)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref$presence
  expected_results$liquidity_classification <- c("micro_caps", "small_caps", "small_caps")
  expected_results$liquidity_floor <- c(0,1,1)
  expected_results$bop_port_weights <- c(0.2, 0.25, 0)
  expected_results$buffer_zone_1 <- c(1,0,0)
  expected_results$is_eligible <- c(1,0,1)

  liquidity_constraint_policy <- list(liquidity_floor_rule = "small_caps")
  turnover_constraint_policy <- list(quantile_range_buffer = 0.25, turnover_cap_rules = c(micro_caps = 0.01))


  expect_equal(
    classify_investment_universe(universe_m_d_ref = stock_universe_m_d_ref,
                                 eligibility_quantile_range = eligiblity_quantile_range,
                                 liquidity_constraint_policy = liquidity_constraint_policy, liquidity_floor_cutoffs = liquidity_floor_cutoffs, liquidity_m_d_ref = liquidity_m_d_ref,
                                 turnover_constraint_policy = turnover_constraint_policy, updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule and 2 buffer_zones", {

  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Create signals_m_d_ref
  current_date <- "2001-04-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date),]
  signals_m_d_ref$exp_ret_score <- c(1.2,0.2,0,.04,3)
  stock_universe_m_d_ref <- signals_m_d_ref %>% dplyr::select(-Alpha, -Beta, -Gamma)

  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date),]

  #Create old port weights test
  last_date <- "2001-03-15"
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == last_date),c("id","tickers","dates")]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.5, 0, 0.5)



  #Expected results
  expected_results <- stock_universe_m_d_ref
  eligibility_quantile_range <- c(0.5, 1)
  lower_range_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.50)

  expected_results$pre_eligible_assets <- c(1,1,0,0,1)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref$presence

  liquidity_classification_m_d_ref <-
    classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, liquidity_m_df = liquidity_m_d_ref,
                             liquidity_floor_rule = liquidity_constraint_policy$liquidity_floor_rule,apply_liquidity_floor_rule = TRUE)

  expected_results$liquidity_classification <- liquidity_classification_m_d_ref$liquidity_classification
  expected_results$liquidity_floor <- liquidity_classification_m_d_ref$liquidity_floor

  turnover_cap_rule_m_d_ref <- apply_turnover_cap_rule(
    stock_universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = turnover_constraint_policy$quantile_range_buffer,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, turnover_cap_rule = names(turnover_constraint_policy$turnover_cap_rules)[1])


  expected_results$bop_port_weights <- c(0, 0.5, 0, 0, 0.5)
  expected_results$buffer_zone_1 <- c(0,0,0,0,1)

  turnover_cap_rule_m_d_ref <- apply_turnover_cap_rule(
    stock_universe_m_d_ref = signals_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = turnover_constraint_policy$quantile_range_buffer,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, turnover_cap_rule = names(turnover_constraint_policy$turnover_cap_rules)[2])


  expected_results$buffer_zone_2 <- c(0,0,0,0,0)

  expected_results$is_eligible <- c(1,1,0,0,1)
  rownames(expected_results) <- c(1L, 2L, 3L, 4L, 5L)

  expect_equal(
    classify_investment_universe(universe_m_d_ref = stock_universe_m_d_ref,
                                 eligibility_quantile_range = eligibility_quantile_range,
                                 liquidity_constraint_policy = liquidity_constraint_policy, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
                                 liquidity_m_d_ref = liquidity_m_d_ref,
                                 turnover_constraint_policy = turnover_constraint_policy, updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule, 2 buffer_zones and max_max_abs_active_weight_individual_rule ", {

  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Create signals_m_d_ref
  current_date <- "2001-04-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date),]
  signals_m_d_ref$exp_ret_score <- c(1.2,0.2,0,.04,3)
  stock_universe_m_d_ref <- signals_m_d_ref %>% dplyr::select(-Alpha, -Beta, -Gamma)

  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date),]


  #Create old port weights test
  last_date <- "2001-03-15"
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == last_date),c("id","tickers","dates")]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.5, 0, 0.5)

  expected_results <- stock_universe_m_d_ref
  eligibility_quantile_range <- c(0.5, 1)
  lower_bound_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.50)

  expected_results$pre_eligible_assets <- c(1,1,0,0,1)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref$presence

  liquidity_classification_m_d_ref <-
    classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, liquidity_m_df = liquidity_m_d_ref,
                             liquidity_floor_rule = liquidity_constraint_policy$liquidity_floor_rule,apply_liquidity_floor_rule = TRUE)

  expected_results$liquidity_classification <- liquidity_classification_m_d_ref$liquidity_classification
  expected_results$liquidity_floor <- liquidity_classification_m_d_ref$liquidity_floor


  benchmark_weights_m_d_ref <- benchmark_weights_m_df[which(benchmark_weights_m_df$dates == current_date),]
  expected_results$IBOV_bench_weights <- benchmark_weights_m_d_ref$IBOV
  expected_results$max_abs_aw_ind <- c(1,1,1,1,1)


  turnover_cap_rule_m_d_ref <- apply_turnover_cap_rule(
    stock_universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = turnover_constraint_policy$quantile_range_buffer,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, turnover_cap_rule = names(turnover_constraint_policy$turnover_cap_rules)[1])


  expected_results$bop_port_weights <- c(0, 0.5, 0, 0, 0.5)
  expected_results$buffer_zone_1 <- c(0,0,0,0,1)

  turnover_cap_rule_m_d_ref <- apply_turnover_cap_rule(
    stock_universe_m_d_ref = signals_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = turnover_constraint_policy$quantile_range_buffer,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, turnover_cap_rule = names(turnover_constraint_policy$turnover_cap_rules)[2])


  expected_results$buffer_zone_2 <- c(0,0,0,0,0)

  expected_results$is_eligible <- c(1,1,1,1,1)
  rownames(expected_results) <- c(1L, 2L, 3L, 4L, 5L)



  expect_equal(
    classify_investment_universe(universe_m_d_ref = stock_universe_m_d_ref,
                                 eligibility_quantile_range = eligibility_quantile_range,
                                 liquidity_constraint_policy = liquidity_constraint_policy, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
                                 liquidity_m_d_ref = liquidity_m_d_ref,
                                 turnover_constraint_policy = turnover_constraint_policy, updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                 concentration_constraint_policy = concentration_constraint_policy, benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
    ),
    expected_results
  )

})

test_that("classify_investment_universe works when concentration_constraint_policy is set but not max_abs_active_individual_weight", {

  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Create signals_m_d_ref
  current_date <- "2001-04-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date),]
  signals_m_d_ref$exp_ret_score <- c(1.2,0.2,0,.04,3)
  stock_universe_m_d_ref <- signals_m_d_ref %>% dplyr::select(-Alpha, -Beta, -Gamma)

  expected_results <- signals_m_d_ref
  eligibility_quantile_range <- c(0.5, 1)
  lower_bound_quantile_range <- quantile(signals_m_d_ref$exp_ret_score, 0.50)
  expected_results$pre_eligible_assets <- c(1,1,0,0,1)

  benchmark_weights_m_d_ref <- benchmark_weights_m_df[which(benchmark_weights_m_df$dates == current_date),]
  expected_results$IBOV_bench_weights <- benchmark_weights_m_d_ref$IBOV


  expected_results$is_eligible <- c(1,1,0,0,1)
  rownames(expected_results) <- c(1L, 2L, 3L, 4L, 5L)

  concentration_constraint_policy <- concentration_constraint_policy
  concentration_constraint_policy$max_abs_active_individual_weight <- NULL

  expect_equal(
    classify_investment_universe(universe_m_d_ref = signals_m_d_ref,
                                 eligibility_quantile_range = eligibility_quantile_range,
                                 concentration_constraint_policy = concentration_constraint_policy, benchmark_weights_m_d_ref = benchmark_weights_m_d_ref
    ),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule, 2 buffer_zones and groups representativeness, picking the representative with highest exp_ret_score ", {

  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Create signals_m_d_ref
  current_date <- "2001-04-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date),]
  signals_m_d_ref$exp_ret_score <- c(1.2,0.2,0,.04,3)
  stock_universe_m_d_ref <- signals_m_d_ref %>% dplyr::select(-Alpha, -Beta, -Gamma)

  liquidity_m_d_ref <- liquidity_m_df[which(liquidity_m_df$dates == current_date),]
  benchmark_weights_m_d_ref <- benchmark_weights_m_df[which(benchmark_weights_m_df$dates == current_date),]

  #Create old port weights test
  last_date <- "2001-03-15"
  updated_port_weights_m_lstd_ref <- signals_m_df[which(signals_m_df$dates == last_date),c("id","tickers","dates")]
  updated_port_weights_m_lstd_ref$bop_port_weights <- c(0.5, 0, 0.5)

  expected_results <- stock_universe_m_d_ref
  eligibility_quantile_range <- c(0.5, 0.75)
  lower_range_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.50)
  upper_range_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.75)

  expected_results$pre_eligible_assets <- c(1,1,0,0,0)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref$presence

  liquidity_classification_m_d_ref <-
    classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, liquidity_m_df = liquidity_m_d_ref,
                             liquidity_floor_rule = liquidity_constraint_policy$liquidity_floor_rule,apply_liquidity_floor_rule = TRUE)


  expected_results$liquidity_classification <- liquidity_classification_m_d_ref$liquidity_classification
  expected_results$liquidity_floor <- liquidity_classification_m_d_ref$liquidity_floor

  expected_results$IBOV_bench_weights <- benchmark_weights_m_d_ref$IBOV

  turnover_cap_rule_m_d_ref <- apply_turnover_cap_rule(
    stock_universe_m_d_ref = stock_universe_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = turnover_constraint_policy$quantile_range_buffer,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, turnover_cap_rule = names(turnover_constraint_policy$turnover_cap_rules)[1])

  expected_results$bop_port_weights <- c(0, 0.5, 0, 0, 0.5)
  expected_results$buffer_zone_1 <- c(0,0,0,0,0)

  turnover_cap_rule_m_d_ref <- apply_turnover_cap_rule(
    stock_universe_m_d_ref = signals_m_d_ref,
    eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = turnover_constraint_policy$quantile_range_buffer,
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, liquidity_m_d_ref = liquidity_m_d_ref,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, turnover_cap_rule = names(turnover_constraint_policy$turnover_cap_rules)[2])


  expected_results$buffer_zone_2 <- c(0,0,0,0,0)

  stocks_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  stocks_groups_m_d_ref$Sector[4] <- "Financials"
  stocks_groups_m_d_ref$Subsector[4] <- "Insurance"


  expected_results$Sector <- stocks_groups_m_d_ref$Sector
  expected_results$Subsector <- stocks_groups_m_d_ref$Subsector

  expected_results$is_eligible <- c(1,1,0,1,1)
  rownames(expected_results) <- c(1L, 2L, 3L, 4L, 5L)

  concentration_constraint_policy <- concentration_constraint_policy
  concentration_constraint_policy$max_abs_active_individual_weight <- NULL

  expect_equal(
    classify_investment_universe(universe_m_d_ref = stock_universe_m_d_ref,
                                 eligibility_quantile_range = eligibility_quantile_range,
                                 liquidity_constraint_policy = liquidity_constraint_policy, liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
                                 liquidity_m_d_ref = liquidity_m_d_ref,
                                 turnover_constraint_policy = turnover_constraint_policy, updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                 concentration_constraint_policy = concentration_constraint_policy, benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
                                 groups_m_d_ref = stocks_groups_m_d_ref
    ),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule, turnover_cap_rule, max_abs_active_weight_individual_rule and user_defined_OR_rules", {

  #Create signals_m_d_ref
  signals_m_d_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15")),
    signal_1 = c(1, -0.5, 0),
    signal_2 = c(-1, 0, 1),
    exp_ret_score = c(0.5, 0.25, 0.4)
  )

  #Create cutoff
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  #Create liquidity_m_d_ref
  liquidity_m_d_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create old port weights test
  updated_port_weights_m_lstd_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    bop_port_weights = c(0, 0.25, 0)
  )

  #Create selected_benchmark_weights_m_d_ref
  benchmark_weights_m_d_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    IBOV = c(0.25, 0.50, 0.25)
  )

  #Create user_defined_OR_rules_m_df
  user_defined_OR_rules_m_df <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15")),
    beauty = c("pretty", "pretty", "ugly"),
    pretty_stocks = c(1,1,0)
  )


  stock_universe_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates, exp_ret_score)
  expected_results <- stock_universe_m_d_ref
  eligibility_quantile_range <- c(0.5, 1)
  lower_range_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.50)

  expected_results$pre_eligible_assets <- c(1,0,1)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref$presence
  expected_results$liquidity_classification <- c("nano_caps", "small_caps", "small_caps")
  expected_results$liquidity_floor <- c(0,1,1)
  expected_results$bop_port_weights <- c(0, 0.25, 0)
  expected_results$buffer_zone_1 <- c(0,0,0)
  expected_results$beauty <- c("pretty", "pretty", "ugly")
  expected_results$pretty_stocks <- c(1,1,0)

  expected_results$is_eligible <- c(1,1,1)

  liquidity_constraint_policy <- list(
    liquidity_floor_rule = "small_caps",
    liquidity_cap_rules =
      c(micro_caps = 0.01, small_caps = 0.02)
  )

  turnover_constraint_policy <- list(
    turnover_cap_rules = c(micro_caps = 0.03),
    quantile_range_buffer = 0.1
  )


  expect_equal(
    classify_investment_universe(universe_m_d_ref = stock_universe_m_d_ref,
                                 eligibility_quantile_range = eligibility_quantile_range,
                                 liquidity_constraint_policy = liquidity_constraint_policy, liquidity_floor_cutoffs = liquidity_floor_cutoffs, liquidity_m_d_ref = liquidity_m_d_ref,
                                 turnover_constraint_policy = turnover_constraint_policy, updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                 benchmark_weights_m_d_ref = benchmark_weights_m_d_ref, user_defined_OR_rules_m_df = user_defined_OR_rules_m_df),
    expected_results
  )

})

test_that("classify_investment_universe works with liquidity_floor_rule, turnover_cap_rule, max_abs_active_weight_individual_rule and user_defined_OR_rules/user_defined_AND_rules", {

  #Create signals_m_d_ref
  signals_m_d_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15")),
    signal_1 = c(1, -0.5, 0),
    signal_2 = c(-1, 0, 1),
    exp_ret_score = c(0.5, 0.25, 0.4)
  )

  #Create cutoff
  liquidity_floor_cutoffs <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  #Create liquidity_m_d_ref
  liquidity_m_d_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 24500),
    presence = c(95, 99, 100)
  )

  #Create old port weights test
  updated_port_weights_m_lstd_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    bop_port_weights = c(0, 0.25, 0)
  )

  #Create selected_benchmark_weights_m_d_ref
  benchmark_weights_m_d_ref <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15"), format = "%Y-%m-%d"),
    IBOV = c(0.25, 0.50, 0.25)
  )

  #Create user_defined_OR_rules_m_df
  user_defined_OR_rules_m_df <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15")),
    beauty = c("pretty", "pretty", "ugly"),
    pretty_stocks = c(1,1,0)
  )

  #Create user_defined_AND_rules_m_df
  user_defined_AND_rules_m_df <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15"),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15")),
    characters = c("penguim", "octopus", "joker"),
    super_heroes = c(0,0,0)
  )

  stock_universe_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates, exp_ret_score)

  expected_results <- stock_universe_m_d_ref
  eligibility_quantile_range <- c(0.50, 1)
  lower_range_quantile_buffer <- quantile(signals_m_d_ref$exp_ret_score, 0.50)

  expected_results$pre_eligible_assets <- c(1,0,1)
  expected_results$mean_volfin_3m <- liquidity_m_d_ref$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref$presence
  expected_results$liquidity_classification <- c("nano_caps", "small_caps", "small_caps")
  expected_results$liquidity_floor <- c(0,1,1)
  expected_results$IBOV_bench_weights <- c(0.25,0.50,0.25)
  expected_results$max_abs_aw_ind <- c(0,1,0)
  expected_results$bop_port_weights <- c(0, 0.25, 0)
  expected_results$buffer_zone_1 <- c(0,0,0)
  expected_results$beauty <- c("pretty", "pretty", "ugly")
  expected_results$pretty_stocks <- c(1,1,0)
  expected_results$characters <- c("penguim", "octopus", "joker")
  expected_results$super_heroes <- c(0,0,0)

  expected_results$is_eligible <- c(0,0,0)


  liquidity_constraint_policy <- list(
    liquidity_floor_rule = "small_caps",
    liquidity_cap_rules =
      c(micro_caps = 0.01, small_caps = 0.02)
  )

  turnover_constraint_policy <- list(
    turnover_cap_rules = c(micro_caps = 0.03),
    quantile_range_buffer = 0.1
  )

  concentration_constraint_policy <- list(
    benchmark = "IBOV",
    max_abs_active_individual_weight = 0.50
  )


  expect_equal(
    classify_investment_universe(universe_m_d_ref = stock_universe_m_d_ref, eligibility_quantile_range = eligibility_quantile_range,
                                 liquidity_constraint_policy = liquidity_constraint_policy, liquidity_floor_cutoffs = liquidity_floor_cutoffs, liquidity_m_d_ref = liquidity_m_d_ref,
                                 turnover_constraint_policy = turnover_constraint_policy, updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                 benchmark_weights_m_d_ref = benchmark_weights_m_d_ref, concentration_constraint_policy = concentration_constraint_policy,
                                 user_defined_OR_rules_m_df = user_defined_OR_rules_m_df, user_defined_AND_rules_m_df = user_defined_AND_rules_m_df),
    expected_results
  )

})

test_that("classify_investment_universe works inside run_port_backtest flow - artificial_port_obj.RData", {

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
  expected_results <- stock_universe_m_d_ref
  expected_results$pre_eligible_assets <- c(1,0,0,0)

  #Liquidity
  liquidity_floor_rule_m_d_ref <- classify_stock_liquidity(
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
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
  turnover_cap_rule_m_d_ref <- apply_turnover_cap_rule(stock_universe_m_d_ref = stock_universe_m_d_ref,
                                                       eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = turnover_constraint_policy$quantile_range_buffer,
                                                       updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                       liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
                                                       liquidity_m_d_ref = liquidity_m_d_ref,
                                                       turnover_cap_rule = names(turnover_constraint_policy$turnover_cap_rules)[1]
                                                       )

  expected_results$bop_port_weights <- c(0.20, 0.20, 0.20, 0.20)
  expected_results$buffer_zone_1 <- c(0,1,0,0)

  turnover_cap_rule_m_d_ref <- apply_turnover_cap_rule(stock_universe_m_d_ref = stock_universe_m_d_ref,
                                                       eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = turnover_constraint_policy$quantile_range_buffer,
                                                       updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                       liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
                                                       liquidity_m_d_ref = liquidity_m_d_ref,
                                                       turnover_cap_rule = names(turnover_constraint_policy$turnover_cap_rules)[2]
  )

  expected_results$buffer_zone_2 <- c(0,0,0,0)

  #Groups
  expected_results$Sector <- stock_groups_m_d_ref$Sector
  expected_results$Subsector <- stock_groups_m_d_ref$Subsector
  expected_results$is_eligible <- c(1,1,1,1)

  #Results
  results <- classify_investment_universe(
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

  rownames(results) <- NULL
  rownames(expected_results) <- NULL

  expect_equal(results, expected_results)


})

test_that("classify_investment_universe works inside run_port_backtest flow - toy_preprocessed_port_obj.RData", {

  #Create signals_m_d_ref
  load(paste(test_path(),"/testdata/","toy_preprocessed_port_obj.RData", sep =""))

  #Quantile Range
  eligibility_quantile_range <- c(0.67, 1)

  #Current date
  current_date <- "2022-12-15"

  #Initial Preps
  signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
  liquidity_m_d_ref <- liquidity_m_df %>% dplyr::filter(dates == current_date)
  benchmark_weights_m_d_ref <- benchmark_weights_m_df %>% dplyr::filter(dates == current_date)
  stock_groups_m_d_ref <- stock_groups_m_df %>% dplyr::filter(dates == current_date)
  updated_port_weights_m_lstd_ref <- signals_m_df %>% dplyr::filter(dates == "2022-11-15") %>% dplyr::select(id, tickers, dates) %>%
    dplyr::mutate(bop_port_weights = 0)
  #Include some arbitrary weights to enable turnover_constraint_policy
  updated_port_weights_m_lstd_ref <- updated_port_weights_m_lstd_ref %>% dplyr::mutate(bop_port_weights = dplyr::if_else(tickers == "MDNE3", 0.2, bop_port_weights))


  #Derive Stock Universe
  stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(signals_m_d_ref = signals_m_d_ref, chosen_score_metric_and_position = c(roe_3m = "long"),
                                                          upper_quantile_winsorization = upper_quantile_winsorization,
                                                          lower_quantile_winsorization = lower_quantile_winsorization)

  #Classify stock universe
  expected_results <- stock_universe_m_d_ref
  expected_results <- expected_results %>% dplyr::mutate(pre_eligible_assets = dplyr::if_else(exp_ret_score >= quantile(exp_ret_score, 0.67), 1 ,0))

  #Liquidity
  liquidity_floor_rule_m_d_ref <- classify_stock_liquidity(
    liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
    liquidity_m_df = liquidity_m_d_ref,
    liquidity_floor_rule = liquidity_constraint_policy$liquidity_floor_rule,
    apply_liquidity_floor_rule = TRUE
  )

  #Check if mega-caps are as expected
  expect_true(all(c("ITUB4", "PETR4", "VALE3", "BBDC4", "ELET3") %in%
              dplyr::pull(dplyr::filter(liquidity_floor_rule_m_d_ref, liquidity_classification == "mega_caps"), tickers)))

  #Check if nano-caps are as expected
  expect_equal(unique(liquidity_floor_rule_m_d_ref %>% dplyr::filter(presence < 97.5) %>% dplyr::pull(liquidity_classification)), "nano_caps")

  #Check if micro_caps are as expected
  expect_equal(liquidity_floor_rule_m_d_ref %>% dplyr::filter(presence > 99 & presence <= 100 & mean_volfin_3m > 1e+02 & mean_volfin_3m < 5e+03) %>%
    dplyr::pull(liquidity_classification) %>% unique(), "small_caps")

  #Check if liquidity_floor_rule is as expected
  expect_equal(unique(liquidity_floor_rule_m_d_ref %>% dplyr::filter(liquidity_classification %in% c("nano_caps")) %>% dplyr::pull(liquidity_floor)),
               0)

  expected_results$mean_volfin_3m <- liquidity_m_d_ref$mean_volfin_3m
  expected_results$presence <- liquidity_m_d_ref$presence

  expected_results$liquidity_classification <- liquidity_floor_rule_m_d_ref$liquidity_classification
  expected_results$liquidity_floor <- liquidity_floor_rule_m_d_ref$liquidity_floor

  #Bench weights
  expected_results <- expected_results %>%
    dplyr::left_join(benchmark_weights_m_d_ref %>% dplyr::select(id, ibov), by = "id")
  colnames(expected_results)[10] <- "ibov_bench_weights"

  expected_results <- expected_results %>% dplyr::mutate(max_abs_aw_ind =
                                                           dplyr::if_else(ibov_bench_weights > concentration_constraint_policy$max_abs_active_individual_weight, 1L, 0L))

  #Buffer Zones
  turnover_cap_rule_m_d_ref <- apply_turnover_cap_rule(stock_universe_m_d_ref = stock_universe_m_d_ref,
                                                       eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = turnover_constraint_policy$quantile_range_buffer,
                                                       updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                       liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
                                                       liquidity_m_d_ref = liquidity_m_d_ref,
                                                       turnover_cap_rule = names(turnover_constraint_policy$turnover_cap_rules)[1]
  )

  expected_results$bop_port_weights <- 0
  expected_results$buffer_zone_1 <- 0

  expected_results <- expected_results %>% dplyr::mutate(bop_port_weights = dplyr::if_else(tickers == "MDNE3", 0.2, 0),
                                                         buffer_zone_1 = dplyr::if_else(tickers == "MDNE3", 1, 0))

  turnover_cap_rule_m_d_ref <- apply_turnover_cap_rule(stock_universe_m_d_ref = stock_universe_m_d_ref,
                                                       eligibility_quantile_range = eligibility_quantile_range, quantile_range_buffer = turnover_constraint_policy$quantile_range_buffer,
                                                       updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
                                                       liquidity_floor_cutoffs = liquidity_floor_cutoffs_df,
                                                       liquidity_m_d_ref = liquidity_m_d_ref,
                                                       turnover_cap_rule = names(turnover_constraint_policy$turnover_cap_rules)[2]
  )

  expected_results$buffer_zone_2 <- 0

  #Groups
  expected_results$sectors <- stock_groups_m_d_ref$sectors
  expected_results$macro_sector <- stock_groups_m_d_ref$macro_sector
  expected_results <- expected_results %>% dplyr::mutate(is_eligible =( pre_eligible_assets * liquidity_floor) +
                                                           max_abs_aw_ind + buffer_zone_1 + buffer_zone_2) %>%
    dplyr::mutate(is_eligible = dplyr::if_else(is_eligible >= 1, 1, 0))

    #Results
  results <- classify_investment_universe(
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

  rownames(results) <- NULL
  rownames(expected_results) <- NULL

  expect_equal(results, expected_results)


})

test_that("classify_investment_universe throws an error when no signals are significant", {

  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #THEME SB
  signal_universe_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
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

  signal_groups_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                       tickers = c("Alpha", "low_Beta", "Gamma"),
                                       dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                       theme = c("Value", "Momentum", "Value")
  )


  signal_universe_m_d_ref$adjusted_p_value <- p.adjust(signal_universe_m_d_ref$p_value, "BH")
  signal_universe_m_d_ref$exp_ret_score <- signal_transform(signal_universe_m_d_ref$alpha, 0.01, 0.99)

  expect_error(
    classify_investment_universe(universe_m_d_ref = signal_universe_m_d_ref, signal_significance_threshold = 0.05,
                                 groups_m_d_ref = signal_groups_m_d_ref,
                                 asset_object = "signals"),
    "No signal was deemed significant."
  )

})
