test_that("create_performance_m_df works with no NAs and active returns", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"


  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions,
    signal_themes_m_df = signal_themes_m_df,
    signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, "IBOV"]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  current_date <- "2001-06-15"

  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:4), ]

  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:4),]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]


  #Create base_signal_universe_m_d_ref
  base_signal_universe_m_d_ref <- create_performance_m_df(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    active_returns = TRUE
  )

  #Check 1st
  expect_equal(base_signal_universe_m_d_ref$act_arith_mean_ret[1], -0.505126, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_geom_mean_ret[1], -0.5149917, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_ann_ret[1], -6.007827, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$track_err[1], 1.6192, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_track_err[1], 5.6091, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$info_ratio[1], -0.31196, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_info_ratio[1], -1.0710921, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_exp_short[1], 3.1, tolerance = 1e-1) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$info_ratio_exp_short[1], -0.1594, tolerance = 1e-1) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$act_pain[1], 0.987964, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_ulcer[1], 1.34454, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_ann_calmar_ratio[1], -2.52234, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_ann_burke_ratio[1], -4.51, tolerance = 1e-1) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$act_dd_dev[1], 1.33093, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_ann_pain_ratio[1], -6.08, tolerance = 1e-2) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$act_ann_martin_ratio[1], -4.4683, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_semi_dev[1], 0.9526, tolerance = 1e-2) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$act_down_dev[1], 1.27227, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_sortino_ratio[1], -0.39703, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_inv_d_ratio[1], 0.14485, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_down_freq[1], 0.75)
  expect_equal(base_signal_universe_m_d_ref$info_ratio_semi_dev[1], -0.53, tolerance = 1e-1)
  expect_equal(base_signal_universe_m_d_ref$act_modigliani[1], -0.427301, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_omega[1], 0.43456, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_ann_modigliani[1], -5.08219, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_max_dd[1], 2.3818, tolerance = 1e-3)


  #Check 2nd
  expect_equal(base_signal_universe_m_d_ref$act_arith_mean_ret[2], -0.8743979, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$track_err[2], 1.2632, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_info_ratio[2], -2.300906, tolerance = 1e-3)


})

test_that("create_performance_m_df works with NAs and active returns", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"


  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions,
    signal_themes_m_df = signal_themes_m_df,
    signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, "IBOV"]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  current_date <- "2001-06-15"

  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:4), ]

  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:4),]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]

  #Include NAs
  selected_backtest_returns_corrected_positions_xts_upd_ref$Alpha[c(1:2)] <- NA

  #Create base_signal_universe_m_d_ref
  base_signal_universe_m_d_ref <- create_performance_m_df(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    active_returns = TRUE
  )

  #Check 1st
  expect_equal(base_signal_universe_m_d_ref$act_arith_mean_ret[1], -0.414507, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_geom_mean_ret[1], -0.4339416, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_ann_ret[1], -5.084798, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$track_err[1], 2.7822, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_track_err[1], 9.6380, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$info_ratio[1], -0.148983, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_info_ratio[1], -0.52758, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_exp_short[1],
               as.numeric(PerformanceAnalytics::ETL(
                 (selected_backtest_returns_corrected_positions_xts_upd_ref$Alpha[3:4]/100+1)/
                  (selected_market_factor_proxy_xts_upd_ref[3:4]/100+1) - 1))*100*-1,
                 tolerance = 1e-1) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$info_ratio_exp_short[1], -0.109926, tolerance = 1e-1) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$act_pain[1], 1.19092, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_ulcer[1], 1.68422, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_ann_calmar_ratio[1], -2.1348132, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_ann_burke_ratio[1], -3.019085, tolerance = 1e-1) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$act_dd_dev[1], 1.68422, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_ann_pain_ratio[1], -4.2696, tolerance = 1e-2) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$act_ann_martin_ratio[1], -3.019082, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_semi_dev[1], 1.39112, tolerance = 1e-2) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$act_down_dev[1], 1.68422, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_sortino_ratio[1], -0.24611, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_inv_d_ratio[1], 0.651944, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_down_freq[1], 0.5)
  expect_equal(base_signal_universe_m_d_ref$info_ratio_semi_dev[1], -0.2979, tolerance = 1e-1)
  expect_equal(base_signal_universe_m_d_ref$act_modigliani[1], -0.27914, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_omega[1], 0.651944, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_ann_modigliani[1], -3.4242672, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_max_dd[1], 2.3818, tolerance = 1e-3)


  #Check 2nd
  expect_equal(base_signal_universe_m_d_ref$act_arith_mean_ret[2], -0.8743979, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$track_err[2], 1.2632, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_info_ratio[2], -2.300906, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_modigliani[2], -1.052399, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$act_ann_modigliani[2], -12.11782, tolerance = 1e-3)



})

test_that("create_performance_m_df works with NAs and raw returns", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"


  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions,
    signal_themes_m_df = signal_themes_m_df,
    signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, "IBOV"]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  current_date <- "2001-06-15"

  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:4), ]

  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:4),]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]

  #Include NAs
  selected_backtest_returns_corrected_positions_xts_upd_ref$Alpha[c(1:2)] <- NA

  #Create base_signal_universe_m_d_ref
  base_signal_universe_m_d_ref <- create_performance_m_df(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    active_returns = FALSE
  )

  #Check 1st
  expect_equal(base_signal_universe_m_d_ref$arith_mean_ret[1], -0.1024225, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$geom_mean_ret[1], -0.1082708, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_ret[1], -1.29154, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$std_dev[1], 1.52867, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_std_dev[1], 5.2955, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$sharpe_ratio[1], -0.06700, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_sharpe_ratio[1], -0.2438949, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$exp_short[1],
               as.numeric(PerformanceAnalytics::ETL(
                 (selected_backtest_returns_corrected_positions_xts_upd_ref$Alpha[3:4]/100)))*100*-1,
               tolerance = 1e-1) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$sharpe_ratio_exp_short[1], -0.05261938, tolerance = 1e-1) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$pain[1], 0.59168, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ulcer[1], 0.83676, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_calmar_ratio[1], -1.09142, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_burke_ratio[1], -1.543505, tolerance = 1e-1) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$dd_dev[1], 0.83676, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_pain_ratio[1], -2.18284, tolerance = 1e-2) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$ann_martin_ratio[1], -1.543501, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$semi_dev[1], 0.7643365, tolerance = 1e-2) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$down_dev[1], 0.837, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$sortino_ratio[1], -0.1224, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$inv_d_ratio[1], 0.8268, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$down_freq[1], 0.5)
  expect_equal(base_signal_universe_m_d_ref$sharpe_ratio_semi_dev[1], -0.13400, tolerance = 1e-1)
  expect_equal(base_signal_universe_m_d_ref$modigliani[1], -0.040967, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$omega[1], 0.826895, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_modigliani[1], -0.51658, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$max_dd[1], 1.18336, tolerance = 1e-3)


  #Check 2nd
  expect_equal(base_signal_universe_m_d_ref$arith_mean_ret[3], 0.2064, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$std_dev[3], 1.428239, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_sharpe_ratio[3], 0.4873869, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$modigliani[3], 0.148327, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_modigliani[3], 1.7326, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$prob_sharpe_ratio[3],
  as.numeric(PerformanceAnalytics::ProbSharpeRatio(selected_backtest_returns_corrected_positions_xts_upd_ref$low_Beta, refSR = 0)$sr_prob)
  )
  expect_equal(base_signal_universe_m_d_ref$min_track_record[3],
               PerformanceAnalytics::MinTrackRecord(selected_backtest_returns_corrected_positions_xts_upd_ref$low_Beta, refSR = 0)$num_of_extra_obs_needed)


})

test_that("create_performance_m_df works with NAs (one column only NAs)", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"


  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions,
    signal_themes_m_df = signal_themes_m_df,
    signals_m_df = signals_m_df, backtest_returns_xts = backtest_returns_xts)

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts
  selected_market_factor_proxy_xts <- benchmark_returns_xts[, "IBOV"]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  current_date <- "2001-06-15"

  selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[c(1:4), ]

  selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[c(1:4),]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date),]

  #Include NAs
  selected_backtest_returns_corrected_positions_xts_upd_ref$Alpha[c(1:2)] <- NA
  selected_backtest_returns_corrected_positions_xts_upd_ref$Gamma[c(1:4)] <- NA

  #Create base_signal_universe_m_d_ref
  base_signal_universe_m_d_ref <- suppressWarnings(create_performance_m_df(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    active_returns = FALSE
  ))

  #Check 1st
  expect_equal(base_signal_universe_m_d_ref$arith_mean_ret[1], -0.1024225, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$geom_mean_ret[1], -0.1082708, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_ret[1], -1.29154, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$std_dev[1], 1.52867, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_std_dev[1], 5.2955, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$sharpe_ratio[1], -0.06700, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_sharpe_ratio[1], -0.2438949, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$exp_short[1],
               as.numeric(PerformanceAnalytics::ETL(
                 (selected_backtest_returns_corrected_positions_xts_upd_ref$Alpha[3:4]/100)))*100*-1,
               tolerance = 1e-1) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$sharpe_ratio_exp_short[1], -0.05261938, tolerance = 1e-1) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$pain[1], 0.59168, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ulcer[1], 0.83676, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_calmar_ratio[1], -1.09142, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_burke_ratio[1], -1.543505, tolerance = 1e-1) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$dd_dev[1], 0.83676, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_pain_ratio[1], -2.18284, tolerance = 1e-2) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$ann_martin_ratio[1], -1.543501, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$semi_dev[1], 0.7643365, tolerance = 1e-2) #Small method difference
  expect_equal(base_signal_universe_m_d_ref$down_dev[1], 0.837, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$sortino_ratio[1], -0.1224, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$inv_d_ratio[1], 0.8268, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$down_freq[1], 0.5)
  expect_equal(base_signal_universe_m_d_ref$sharpe_ratio_semi_dev[1], -0.13400, tolerance = 1e-1)
  expect_equal(base_signal_universe_m_d_ref$modigliani[1], -0.050492, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$omega[1], 0.826895, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_modigliani[1], -0.636706, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$max_dd[1], 1.18336, tolerance = 1e-3)


  #Check 2nd
  expect_equal(base_signal_universe_m_d_ref$arith_mean_ret[3], 0.2064, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$std_dev[3], 1.428239, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_sharpe_ratio[3], 0.4873869, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$modigliani[3], 0.169204, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_modigliani[3], 1.976513, tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$prob_sharpe_ratio[3],
               as.numeric(PerformanceAnalytics::ProbSharpeRatio(selected_backtest_returns_corrected_positions_xts_upd_ref$low_Beta, refSR = 0)$sr_prob)
  )
  expect_equal(base_signal_universe_m_d_ref$min_track_record[3],
               PerformanceAnalytics::MinTrackRecord(selected_backtest_returns_corrected_positions_xts_upd_ref$low_Beta, refSR = 0)$num_of_extra_obs_needed)


  #Check 3rd
  expect_equal(base_signal_universe_m_d_ref$arith_mean_ret[2], as.numeric(NA), tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$std_dev[2], as.numeric(NA), tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_sharpe_ratio[2], as.numeric(NA), tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$modigliani[2], as.numeric(NA), tolerance = 1e-3)
  expect_equal(base_signal_universe_m_d_ref$ann_modigliani[2], as.numeric(NA), tolerance = 1e-3)



})


test_that("create_performance_m_df throws a warning for backtests with only positive values", {

  set.seed(123)
  backtest_returns_xts <- xts::xts(data.frame(A = rlnorm(100, 0, 1), B = rnorm(100, 0, 1)),
                                   order.by = seq.Date(from = as.Date("2000-01-01"), by = "months", length.out = 100)
  )

  selected_market_factor_proxy_xts <- xts::xts(data.frame(Bench = rnorm(100, 0, 1)),
                                               order.by = seq.Date(from = as.Date("2000-01-01"), by = "months", length.out = 100)
  )
  # Capture all warnings
  warnings <- testthat::capture_warnings({
    performance_df <- create_performance_m_df(
      backtest_returns_xts,
      selected_market_factor_proxy_xts,
      active_returns = FALSE
    )
  })

  # Check that the specific warning is among the captured warnings
  expect_true(any(grepl("The following raw return columns only contains positive values, compromising some calculations: A", warnings)))


})

test_that("create_performance_m_df works for Prob Sharpe Ratio and Min Track Record", {

  set.seed(123)
  backtest_returns_xts <- xts::xts(data.frame(A = rnorm(100, 0, 1), B = rnorm(100, 0, 1)),
                                   order.by = seq.Date(from = as.Date("2000-01-01"), by = "months", length.out = 100)
                                   )

  selected_market_factor_proxy_xts <- xts::xts(data.frame(Bench = rnorm(100, 0, 1)),
                                               order.by = seq.Date(from = as.Date("2000-01-01"), by = "months", length.out = 100)
  )

  result <- create_performance_m_df(backtest_returns_xts, selected_market_factor_proxy_xts, active_returns = FALSE)

  #ProbSharpeRatio
  expect_equal(result$prob_sharpe_ratio[1], as.numeric(PerformanceAnalytics::ProbSharpeRatio(backtest_returns_xts$A, refSR = 0)$sr_prob))
  expect_equal(result$prob_sharpe_ratio[2], as.numeric(NA))
  #MinTrackRecord
  expect_equal(result$min_track_record[1], as.numeric(PerformanceAnalytics::MinTrackRecord(backtest_returns_xts$A, refSR = 0)$num_of_extra_obs_needed))
  expect_equal(result$min_track_record[2], as.numeric(NA))


  #Insert NA's
  backtest_returns_xts$A[1:25] <- NA
  result <- create_performance_m_df(backtest_returns_xts, selected_market_factor_proxy_xts, active_returns = TRUE)

  #ProbSharpeRatio
  A <- ((backtest_returns_xts$A[26:100]/100+1)/(selected_market_factor_proxy_xts$Bench[26:100]/100+1)-1)*100
  expect_equal(result$prob_info_ratio[1], as.numeric(PerformanceAnalytics::ProbSharpeRatio(A, refSR = 0)$sr_prob))
  expect_equal(result$prob_info_ratio[2], as.numeric(NA))
  #MinTrackRecord
  expect_equal(result$act_min_track_record[1], as.numeric(PerformanceAnalytics::MinTrackRecord(A, refSR = 0)$num_of_extra_obs_needed))
  expect_equal(result$act_min_track_record[2], as.numeric(NA))

})
