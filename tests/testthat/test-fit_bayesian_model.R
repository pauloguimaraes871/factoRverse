test_that("fit_bayesian_model adequately fits a bayesian hierarchical model", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #current info
  current_date <- "2001-07-15"
  priors_m_upd_ref_list <- list(jkp_emerging = priors_m_df_list$jkp_emerging[which(priors_m_df_list$jkp_emerging$dates <= current_date), ])

  signals_groups_m_df <- groups_m_df_list$signals
  signals_groups_m_d_ref <- signals_groups_m_df[which(signals_groups_m_df$dates == current_date), ]

  current_theme <- "value"

  backtest_returns_upd_ref <- backtest_returns_df[which(backtest_returns_df$dates <= current_date), ]
  selected_benchmark_returns_upd_ref <- benchmark_returns_df[which(benchmark_returns_df$dates <= current_date), c("dates", "IBOV")]

  #get selected info
  selected_signals_and_backtest_list <- select_and_correct_signals(
    signal_selection_policy = signal_selection_policy,
    signals_m_upd_ref = signals_m_df[which(signals_m_df$dates <= current_date), ],
    backtest_returns_upd_ref = backtest_returns_upd_ref
  )

  #current theme data
  signals_in_current_theme <- c("Alpha", "Gamma")
  current_theme_backtest_returns_upd_ref <- reshape2::melt(backtest_returns_upd_ref[, c("Alpha", "Gamma")])
  current_theme_backtest_returns_upd_ref$bench_return <- selected_benchmark_returns_upd_ref$IBOV
  colnames(current_theme_backtest_returns_upd_ref) <- c("signal", "active_return", "bench_return")

  #get priors
  priors <- set_priors(priors_data = priors_m_upd_ref_list$jkp_emerging)

  set.seed(123)
  expected_result <- brms::brm(
    brms::brmsformula(
      active_return ~ bench_return + (bench_return | signal),
      sigma ~ 1 + (1 | signal)),
    prior = priors$value,
    data = current_theme_backtest_returns_upd_ref
  )

  set.seed(123)
  results <- fit_bayesian_model(theme = current_theme, groups = signals_groups_m_d_ref, backtest_returns = backtest_returns_upd_ref, bench_returns = selected_benchmark_returns_upd_ref$IBOV ,priors = priors)


  #check if brm model was fit
  expect_equal(class(results), "brmsfit")
  expect_equal(results$fit, expected_result$fit)



})


