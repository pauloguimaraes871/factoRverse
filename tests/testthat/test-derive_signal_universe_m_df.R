test_that("derive_signal_universe_m_df works when ss_results is provided", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  set.seed(123)
  #Backtest Returns
  mocked_backtest_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    asset_turnover_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 5, sd = 3.5),
    book_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1, sd = 5),
    dps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 15, sd = 0.4),
    eps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.0005, sd = 0.3),
    mom_res_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 3.15, sd = 3.5),
    roe_3m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.1, sd = 2),
    sharpe_6m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 2.5, sd = 5),
    low_idio_vol_mrkt_ewma = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.05, sd = 7.5)
  ), order.by = unique(toy_preprocessed_features$dates)))

  #Benchmark Returns xts
  suppressWarnings(
    mocked_benchmark_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
      IBOV = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.01, sd = 0.035),
      SMLL = rnorm(length(unique(toy_preprocessed_features$dates)), mean = -0.01, sd = 0.025)
    ),  order.by = unique(toy_preprocessed_features$dates))
    ))

  #Chosen Signals and Positions
  chosen_signals_and_positions <- c(asset_turnover_12m = "long", book_yield = "long", dps_yield = "long", eps_yield = "long",
                                    idio_vol_mrkt_ewma = "short", sharpe_6m = "long")

  #Mocked Signal Themes
  mocked_signal_themes_m_df <- expand.grid(
    tickers = names(mocked_backtest_returns_m_xts@data),
    dates = unique(toy_preprocessed_features$dates),
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(id = paste0(tickers,"-",dates),
                      theme = dplyr::case_when(
                        tickers %in% c("mom_res_12m", "sharpe_6m") ~ "momentum",
                        tickers %in% c("dy_med_36m", "eps_yield", "book_yield", "asset_turnover_12m", "dps_yield") ~ "value",
                        tickers %in% c("roe_3m", "low_idio_vol_mrkt_ewma") ~ "defensive"
                      )
  ) %>%  dplyr::arrange(id) %>% dplyr::select(id, tickers, dates, theme)

  signal_themes_m_df <- create_meta_dataframe(mocked_signal_themes_m_df, "st_11", type = "groups")

  ##SS Config
  frequentist_ss_config <- create_ss_backtest_config(initial_sample_size = 3, rebalancing_months = 6,
                                                     split_method = "expanding", config_name = "frequentist_ss", active_returns = TRUE,
                                                     chosen_signals_and_positions = chosen_signals_and_positions
  ) %>%
    add_alpha_test_strategy(model_structure = "no_pooled",
                            signal_significance_threshold = 0.15, p_correction_method = "none",
                            market_factor_proxy = "IBOV", enable_theme_representativeness = TRUE)

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, "feats_123")

  ss_results <- suppressWarnings( #This is for NA warning of NAs at the end of run_ss_backtest
    run_ss_backtest(frequentist_ss_config,
                    signals_m_df = features_m_df, backtest_returns_m_xts = mocked_backtest_returns_m_xts, benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                    signal_themes_m_df = signal_themes_m_df,
                    verbose = TRUE
    )
  )

  ##RP Config
  rp_config <- create_sb_backtest_config(sb_algorithm = "rp", rebalancing_months = 7, training_sample_size = 5, target_fwd_name = "fwd_premium_3m",
                                         chosen_signals_and_positions = "all") %>%
    add_cov_est_method(cov_matrix_benchmark = "IBOV")

  #Derive Signal Universe
  derived_signal_universe_m_df <- derive_signal_universe_m_df(config = rp_config, features_m_df = features_m_df,
                                                              ss_backtest_results = ss_results,
                                                              backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                                              benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                                              signal_themes_m_df = signal_themes_m_df,
                                                              priors_m_df = NULL,
                                                              custom_signal_universe_metrics_m_df = NULL,
                                                              verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95))

  #Check signal_universe
  expect_equal(derived_signal_universe_m_df$signal_universe_m_df,
               ss_results@signal_universe_m_df@data
               )

  #Check chosen signals
  expect_equal(derived_signal_universe_m_df$chosen_signals_and_positions,
               ss_results@ss_backtest_workflow$`2023-07-15`$chosen_signals_and_positions)

  expect_equal(derived_signal_universe_m_df$chosen_signals_and_positions,
               chosen_signals_and_positions)

})

test_that("derive_signal_universe_m_df works when signal_universe must be created for 'all' chosen_signals_and_positions", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  set.seed(123)
  #Backtest Returns
  mocked_backtest_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    asset_turnover_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 5, sd = 3.5),
    book_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1, sd = 5),
    dps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 15, sd = 0.4),
    eps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.0005, sd = 0.3),
    mom_res_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 3.15, sd = 3.5),
    roe_3m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.1, sd = 2),
    sharpe_6m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 2.5, sd = 5),
    low_idio_vol_mrkt_ewma = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.05, sd = 7.5)
  ), order.by = unique(toy_preprocessed_features$dates)))

  #Benchmark Returns xts
  suppressWarnings(
    mocked_benchmark_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
      IBOV = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.01, sd = 0.035),
      SMLL = rnorm(length(unique(toy_preprocessed_features$dates)), mean = -0.01, sd = 0.025)
    ),  order.by = unique(toy_preprocessed_features$dates))
    ))


  #Chosen Signals and Positions
  chosen_signals_and_positions <- c(asset_turnover_12m = "long", book_yield = "long", dps_yield = "long", eps_yield = "long",
                                    idio_vol_mrkt_ewma = "short", sharpe_6m = "long")

  #Mocked Signal Themes
  mocked_signal_themes_m_df <- expand.grid(
    tickers = names(mocked_backtest_returns_m_xts@data),
    dates = unique(toy_preprocessed_features$dates),
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(id = paste0(tickers,"-",dates),
                      theme = dplyr::case_when(
                        tickers %in% c("mom_res_12m", "sharpe_6m") ~ "momentum",
                        tickers %in% c("dy_med_36m", "eps_yield", "book_yield", "asset_turnover_12m", "dps_yield") ~ "value",
                        tickers %in% c("roe_3m", "low_idio_vol_mrkt_ewma") ~ "defensive"
                      )
  ) %>%  dplyr::arrange(id) %>% dplyr::select(id, tickers, dates, theme)

  signal_themes_m_df <- create_meta_dataframe(mocked_signal_themes_m_df, "st_11", type = "groups")

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, "feats_123")

  ##RP Config
  rp_config <- create_sb_backtest_config(sb_algorithm = "rp", rebalancing_months = 7, training_sample_size = 5, target_fwd_name = "fwd_premium_3m",
                                         chosen_signals_and_positions = "all") %>%
    add_cov_est_method(cov_matrix_benchmark = "IBOV")

  #Derive Signal Universe
  expect_message(
  expect_message(
    derived_signal_universe_m_df <- derive_signal_universe_m_df(config = rp_config, features_m_df = features_m_df,
                                                                ss_backtest_results = NULL,
                                                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                                                signal_themes_m_df = signal_themes_m_df,
                                                                priors_m_df = NULL,
                                                                custom_signal_universe_metrics_m_df = NULL,
                                                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "According to user choice, SB backtest will contemplate all signals in features_m_df, assuming a 'long' position."
  ),
  "signal_themes_m_df does not contain all the id's in signal_universe_m_df.")

  #Check signal_universe
  expected_tickers <- colnames(features_m_df@data)[-c(1:3)][order(colnames(features_m_df@data)[-c(1:3)])]
  expect_equal(unique(derived_signal_universe_m_df$signal_universe_m_df$tickers), expected_tickers)
  expect_equal(unique(derived_signal_universe_m_df$signal_universe_m_df$dates), unique(features_m_df@data$dates))
  expect_equal(nrow(derived_signal_universe_m_df$signal_universe_m_df), length(expected_tickers) * length(unique(features_m_df@data$dates)))
  expect_equal(unique(derived_signal_universe_m_df$signal_universe_m_df$is_eligible), 1)
  expect_true(is_coercible_to_meta_dataframe(derived_signal_universe_m_df$signal_universe_m_df))


  #Check chosen signals
  expect_equal(names(derived_signal_universe_m_df$chosen_signals_and_positions),  colnames(features_m_df@data)[-c(1:3)])
  expect_equal(unname(derived_signal_universe_m_df$chosen_signals_and_positions),  rep("long", length(colnames(features_m_df@data)[-c(1:3)])))
  expect_false(any(!colnames(features_m_df@data)[-c(1:3)] %in% names(derived_signal_universe_m_df$chosen_signals_and_positions)))


})

test_that("derive_signal_universe_m_df works when signal_universe must be created for chosen_signals_and_positions different from 'all'", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  set.seed(123)
  #Backtest Returns
  mocked_backtest_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    asset_turnover_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 5, sd = 3.5),
    book_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1, sd = 5),
    dps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 15, sd = 0.4),
    eps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.0005, sd = 0.3),
    mom_res_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 3.15, sd = 3.5),
    roe_3m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.1, sd = 2),
    sharpe_6m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 2.5, sd = 5),
    low_idio_vol_mrkt_ewma = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.05, sd = 7.5)
  ), order.by = unique(toy_preprocessed_features$dates)))

  #Benchmark Returns xts
  suppressWarnings(
    mocked_benchmark_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
      IBOV = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.01, sd = 0.035),
      SMLL = rnorm(length(unique(toy_preprocessed_features$dates)), mean = -0.01, sd = 0.025)
    ),  order.by = unique(toy_preprocessed_features$dates))
    ))


  #Chosen Signals and Positions
  chosen_signals_and_positions <- c(asset_turnover_12m = "long", book_yield = "long", dps_yield = "long", eps_yield = "long",
                                    idio_vol_mrkt_ewma = "short", sharpe_6m = "long")

  #Mocked Signal Themes
  mocked_signal_themes_m_df <- expand.grid(
    tickers = colnames(toy_preprocessed_features)[-c(1:3)],
    dates = unique(toy_preprocessed_features$dates),
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(tickers = dplyr::if_else(
    tickers == "idio_vol_mrkt_ewma", "low_idio_vol_mrkt_ewma", tickers
  )) %>% dplyr::mutate(id = paste0(tickers,"-",dates),
                      theme = dplyr::case_when(
                        tickers %in% c("mom_res_12m", "sharpe_6m") ~ "momentum",
                        tickers %in% c("dy_med_36m", "eps_yield", "book_yield", "asset_turnover_12m", "dps_yield") ~ "value",
                        tickers %in% c("roe_3m", "low_idio_vol_mrkt_ewma") ~ "defensive"
                      )
  ) %>%  dplyr::arrange(id) %>% dplyr::select(id, tickers, dates, theme)

  signal_themes_m_df <- create_meta_dataframe(mocked_signal_themes_m_df, "st_11", type = "groups")

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, "feats_123")

  ##RP Config
  rp_config <- create_sb_backtest_config(sb_algorithm = "rp", rebalancing_months = 7, training_sample_size = 5, target_fwd_name = "fwd_premium_3m",
                                         chosen_signals_and_positions = chosen_signals_and_positions) %>%
    add_cov_est_method(cov_matrix_benchmark = "IBOV")

  #Derive Signal Universe
  derived_signal_universe_m_df <- derive_signal_universe_m_df(config = rp_config, features_m_df = features_m_df,
                                                                ss_backtest_results = NULL,
                                                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                                                signal_themes_m_df = signal_themes_m_df,
                                                                priors_m_df = NULL,
                                                                custom_signal_universe_metrics_m_df = NULL,
                                                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95))

   #Check signal_universe
  expected_tickers <- colnames(features_m_df@data)[-c(1:3)]
  expected_tickers[6] <- paste0("low_", expected_tickers[6])
  expected_tickers <- expected_tickers[order(expected_tickers)]
  corrected_chosen_signals_and_positions_names <- names(chosen_signals_and_positions)
  corrected_chosen_signals_and_positions_names[5] <- paste0("low_", corrected_chosen_signals_and_positions_names[5])

  expect_equal(unique(derived_signal_universe_m_df$signal_universe_m_df$tickers), expected_tickers)
  expect_equal(unique(derived_signal_universe_m_df$signal_universe_m_df$dates), unique(features_m_df@data$dates))
  expect_equal(unique(derived_signal_universe_m_df$signal_universe_m_df %>% dplyr::filter(tickers %in% corrected_chosen_signals_and_positions_names) %>% dplyr::pull(is_eligible)), 1)
  expect_equal(unique(derived_signal_universe_m_df$signal_universe_m_df %>% dplyr::filter(!tickers %in% corrected_chosen_signals_and_positions_names) %>% dplyr::pull(is_eligible)), 0)
  expect_equal(nrow(derived_signal_universe_m_df$signal_universe_m_df), length(expected_tickers) * length(unique(features_m_df@data$dates)))
  expect_true(is_coercible_to_meta_dataframe(derived_signal_universe_m_df$signal_universe_m_df))


  #Check chosen signals
  expect_equal(derived_signal_universe_m_df$chosen_signals_and_positions,  chosen_signals_and_positions)
  expect_true(any(!colnames(features_m_df@data)[-c(1:3)] %in% names(derived_signal_universe_m_df$chosen_signals_and_positions)))

  #Check themes
  expect_equal(
  derived_signal_universe_m_df$signal_universe_m_df %>% dplyr::pull(theme),
  signal_themes_m_df@data %>% dplyr::filter(id %in% derived_signal_universe_m_df$signal_universe_m_df$id) %>% dplyr::pull(theme)
  )




})

test_that("derive_signal_universe_m_df works for short_custom_signal_universe_metrics passed directly", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  set.seed(123)
  #Backtest Returns
  mocked_backtest_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    asset_turnover_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 5, sd = 3.5),
    book_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1, sd = 5),
    dps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 15, sd = 0.4),
    eps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.0005, sd = 0.3),
    mom_res_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 3.15, sd = 3.5),
    roe_3m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.1, sd = 2),
    sharpe_6m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 2.5, sd = 5),
    low_idio_vol_mrkt_ewma = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.05, sd = 7.5)
  ), order.by = unique(toy_preprocessed_features$dates)))

  #Benchmark Returns xts
  suppressWarnings(
    mocked_benchmark_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
      IBOV = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.01, sd = 0.035),
      SMLL = rnorm(length(unique(toy_preprocessed_features$dates)), mean = -0.01, sd = 0.025)
    ),  order.by = unique(toy_preprocessed_features$dates))
    ))

  #Chosen Signals and Positions
  chosen_signals_and_positions <- c(asset_turnover_12m = "long", book_yield = "long", dps_yield = "long", eps_yield = "long",
                                    idio_vol_mrkt_ewma = "short", sharpe_6m = "long")

  #Mocked Signal Themes
  mocked_signal_themes_m_df <- expand.grid(
    tickers = colnames(toy_preprocessed_features)[-c(1:3)],
    dates = unique(toy_preprocessed_features$dates),
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(tickers = dplyr::if_else(tickers == "idio_vol_mrkt_ewma", "low_idio_vol_mrkt_ewma", tickers)) %>%
     dplyr::mutate(id = paste0(tickers,"-",dates),
                      theme = dplyr::case_when(
                        tickers %in% c("mom_res_12m", "sharpe_6m") ~ "momentum",
                        tickers %in% c("dy_med_36m", "eps_yield", "book_yield", "asset_turnover_12m", "dps_yield") ~ "value",
                        tickers %in% c("roe_3m", "low_idio_vol_mrkt_ewma") ~ "defensive"
                      )
  ) %>%  dplyr::arrange(id) %>% dplyr::select(id, tickers, dates, theme)

  signal_themes_m_df <- create_meta_dataframe(mocked_signal_themes_m_df, "st_11", type = "groups")

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, "feats_123")


  #Custom Signal Universe Metrics
  tickers <- colnames(features_m_df@data)[-c(1:3)]
  corrected_tickers <- tickers
  corrected_tickers[6] <- paste0("low_", tickers[6])
  dates <- unique(features_m_df@data$dates)
  less_dates <- dates[c(1,2,3,6,7)] #Check for less dates than features_m_df

  suppressWarnings(
    short_custom_signal_universe_metrics_m_df <- expand.grid(corrected_tickers, less_dates, stringsAsFactors = FALSE) %>% dplyr::rename(tickers = Var1, dates = Var2) %>%
      dplyr::mutate(id = paste0(tickers,"-",dates)) %>%
      dplyr::select(id, tickers, dates) %>%
      dplyr::mutate(pe = runif(dplyr::n(), 0, 100), pb = runif(dplyr::n(), 0, 100), roe = runif(dplyr::n(), 0, 100),
                    div_yield = runif(dplyr::n(), 0, 100), market_cap = runif(dplyr::n(), 0, 100)) %>% dplyr::arrange(id) %>%
      create_meta_dataframe()
  )

  ##SW Config
  expect_warning(
  sw_config <- create_sb_backtest_config(sb_algorithm = "sw", custom_objective = "min_pe", rebalancing_months = 7, training_sample_size = 5, target_fwd_name = "fwd_premium_3m",
                                         chosen_signals_and_positions = chosen_signals_and_positions),
  "custom_objective is not one of typical valid heuristic performance. Please be sure that the metric is present in signal_universe_m_df"
  )
  #Derive Signal Universe
  expect_message(
    derived_signal_universe_m_df <- derive_signal_universe_m_df(config = sw_config, features_m_df = features_m_df,
                                                                ss_backtest_results = NULL,
                                                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                                                signal_themes_m_df = signal_themes_m_df,
                                                                priors_m_df = NULL,
                                                                custom_signal_universe_metrics_m_df = short_custom_signal_universe_metrics_m_df,
                                                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "custom_signal_universe_metrics_m_df does not contain data for all dates in features_m_df."
  )

  #Check signal_universe
  expect_equal(derived_signal_universe_m_df$signal_universe_m_df %>% dplyr::pull(pe), short_custom_signal_universe_metrics_m_df@data %>% dplyr::pull(pe))
  expect_equal(derived_signal_universe_m_df$signal_universe_m_df %>% dplyr::pull(pb), short_custom_signal_universe_metrics_m_df@data %>% dplyr::pull(pb))
  expect_equal(derived_signal_universe_m_df$signal_universe_m_df %>% dplyr::pull(div_yield), short_custom_signal_universe_metrics_m_df@data %>% dplyr::pull(div_yield))
  expect_equal(derived_signal_universe_m_df$signal_universe_m_df %>% dplyr::pull(market_cap), short_custom_signal_universe_metrics_m_df@data %>% dplyr::pull(market_cap))
  expect_equal(derived_signal_universe_m_df$signal_universe_m_df %>% nrow(), short_custom_signal_universe_metrics_m_df@data %>% nrow())
  expect_true(identical(derived_signal_universe_m_df$signal_universe_m_df$id, short_custom_signal_universe_metrics_m_df@data$id))
  #dates of features m df not present in short custom signal universe are not present
  expect_equal(nrow(derived_signal_universe_m_df$signal_universe_m_df %>%
                    dplyr::filter(dates %in%
                                    (features_m_df@data %>%
                                       dplyr::filter(!dates %in% short_custom_signal_universe_metrics_m_df@data$dates) %>%
                                       dplyr::pull(dates)))),
              0)
  expect_equal(nrow(derived_signal_universe_m_df$signal_universe_m_df %>%
                      dplyr::filter(tickers %in%
                                      colnames(features_m_df@data)[which(!colnames(features_m_df@data) %in%
                                                                           short_custom_signal_universe_metrics_m_df@data$tickers)]
                      )),
               0)

  #Check chosen signals
  expect_equal(derived_signal_universe_m_df$chosen_signals_and_positions,
               chosen_signals_and_positions)

  expect_equal(derived_signal_universe_m_df$chosen_signals_and_positions,
               sw_config@chosen_signals_and_positions)

  #Check that NAs in signal_themes column does not cause a problem
  expect_equal(
    derived_signal_universe_m_df$signal_universe_m_df %>%
      dplyr::filter(id %in% (signal_themes_m_df@data %>% dplyr::filter(is.na(theme)) %>% dplyr::pull(id))) %>%
      dplyr::pull(theme) %>% unique(), NA_character_)

})

test_that("derive_signal_universe_m_df error checking works regarding objects", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  set.seed(123)
  #Backtest Returns
  mocked_backtest_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    asset_turnover_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 5, sd = 3.5),
    book_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1, sd = 5),
    dps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 15, sd = 0.4),
    eps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.0005, sd = 0.3),
    mom_res_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 3.15, sd = 3.5),
    roe_3m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.1, sd = 2),
    sharpe_6m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 2.5, sd = 5),
    low_idio_vol_mrkt_ewma = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.05, sd = 7.5)
  ), order.by = unique(toy_preprocessed_features$dates)))

  #Benchmark Returns xts
  suppressWarnings(
    mocked_benchmark_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
      IBOV = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.01, sd = 0.035),
      SMLL = rnorm(length(unique(toy_preprocessed_features$dates)), mean = -0.01, sd = 0.025)
    ),  order.by = unique(toy_preprocessed_features$dates))
    ))

  #Chosen Signals and Positions
  chosen_signals_and_positions <- c(asset_turnover_12m = "long", book_yield = "long", dps_yield = "long", eps_yield = "long",
                                    idio_vol_mrkt_ewma = "short", sharpe_6m = "long")

  #Mocked Signal Themes
  mocked_signal_themes_m_df <- expand.grid(
    tickers = names(mocked_backtest_returns_m_xts@data),
    dates = unique(toy_preprocessed_features$dates),
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(id = paste0(tickers,"-",dates),
                      theme = dplyr::case_when(
                        tickers %in% c("mom_res_12m", "sharpe_6m") ~ "momentum",
                        tickers %in% c("dy_med_36m", "eps_yield", "book_yield", "asset_turnover_12m", "dps_yield") ~ "value",
                        tickers %in% c("roe_3m", "low_idio_vol_mrkt_ewma") ~ "defensive"
                      )
  ) %>%  dplyr::arrange(id) %>% dplyr::select(id, tickers, dates, theme)

  signal_themes_m_df <- create_meta_dataframe(mocked_signal_themes_m_df, "st_11", type = "groups")

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, "feats_123")

  ##SS Config
  frequentist_ss_config <- create_ss_backtest_config(initial_sample_size = 3, rebalancing_months = 6,
                                                     split_method = "expanding", config_name = "frequentist_ss", active_returns = TRUE,
                                                     chosen_signals_and_positions = chosen_signals_and_positions
  ) %>%
    add_alpha_test_strategy(model_structure = "no_pooled",
                            signal_significance_threshold = 0.15, p_correction_method = "none",
                            market_factor_proxy = "IBOV", enable_theme_representativeness = TRUE)

  ss_results <- suppressWarnings( #This is for NA warning of NAs at the end of run_ss_backtest
    run_ss_backtest(frequentist_ss_config,
                    signals_m_df = features_m_df, backtest_returns_m_xts = mocked_backtest_returns_m_xts, benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                    signal_themes_m_df = signal_themes_m_df,
                    verbose = TRUE
    )
  )

  ##RP Config
  rp_config <- create_sb_backtest_config(sb_algorithm = "rp", rebalancing_months = 7, training_sample_size = 5, target_fwd_name = "fwd_premium_3m",
                                         chosen_signals_and_positions = "all") %>%
    add_cov_est_method(cov_matrix_benchmark = "IBOV")


  #Chosen signals do not match feat
  wrong_chosen_signals_and_positions <- c(skew = "long", sharpe_6m = "long")
  wrong_rp_config <- create_sb_backtest_config(sb_algorithm = "rp", rebalancing_months = 7, training_sample_size = 5, target_fwd_name = "fwd_premium_3m",
                                         chosen_signals_and_positions = wrong_chosen_signals_and_positions) %>%
    add_cov_est_method(cov_matrix_benchmark = "IBOV")

  expect_error(
    derive_signal_universe_m_df(config = wrong_rp_config, features_m_df = features_m_df,
                                ss_backtest_results = NULL,
                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = NULL,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "chosen_signals_and_positions must be match the columns of features_m_df"
  )

  #Objs mismatch
  rp_config <- create_sb_backtest_config(sb_algorithm = "rp", rebalancing_months = 7, training_sample_size = 5, target_fwd_name = "fwd_premium_3m",
                                               chosen_signals_and_positions = wrong_chosen_signals_and_positions) %>%
    add_cov_est_method(cov_matrix_benchmark = "IBOV")

  wrong_mocked_backtest_returns_m_xts <- create_meta_xts(mocked_backtest_returns_m_xts@data, meta_xts_name = "wrong")

  expect_error(
    derive_signal_universe_m_df(config = rp_config, features_m_df = features_m_df,
                                backtest_returns_m_xts = wrong_mocked_backtest_returns_m_xts,
                                ss_backtest_results = ss_results,
                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = NULL,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "Object names of backtest_returns_m_xts differ accross ss_backtest_results and sb_backtest."
  )

  wrong_features_m_df <- create_meta_dataframe(features_m_df@data, meta_dataframe_name = "wrong")

  expect_error(
    derive_signal_universe_m_df(config = rp_config, features_m_df = wrong_features_m_df,
                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                ss_backtest_results = ss_results,
                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = NULL,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "Object names of signals_m_df \\(ss_backtest\\) and of features_m_df \\(sb_backtest\\) differ."
  )

  wrong_signal_themes_m_df <- create_meta_dataframe(signal_themes_m_df@data, meta_dataframe_name = "wrong")

  expect_error(
    derive_signal_universe_m_df(config = rp_config, features_m_df = features_m_df,
                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                ss_backtest_results = ss_results,
                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = wrong_signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = NULL,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "Object names of signal_themes_m_df differ accross ss_backtest_results and sb_backtest."
  )

  wrong_rp_config <- create_sb_backtest_config(sb_algorithm = "rp", rebalancing_months = 7, training_sample_size = 5, target_fwd_name = "fwd_premium_3m",
                                         chosen_signals_and_positions = wrong_chosen_signals_and_positions) %>%
    add_cov_est_method(cov_matrix_benchmark = "smll")

  expect_error(
    derive_signal_universe_m_df(config = wrong_rp_config, features_m_df = features_m_df,
                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                ss_backtest_results = ss_results,
                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = wrong_signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = NULL,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "market_factor_proxy and cov_matrix_benchmark in ss_backtest_results differ."
  )

  suppressWarnings(
  wrong_mocked_benchmark_returns_m_xts <- create_meta_xts(mocked_benchmark_returns_m_xts@data, meta_xts_name = "wrong")
  )

  expect_error(
    derive_signal_universe_m_df(config = rp_config, features_m_df = features_m_df,
                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                ss_backtest_results = ss_results,
                                benchmark_returns_m_xts = wrong_mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = wrong_signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = NULL,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "Object names of benchmark_returns_m_xts differ accross ss_backtest_results and sb_backtest."
  )




  expect_warning(
    derive_signal_universe_m_df(config = rp_config, features_m_df = features_m_df,
                                ss_backtest_results = ss_results,
                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = mocked_benchmark_returns_m_xts,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "custom_signal_universe_metrics_m_df should only be provided when a ss_backtest_results is not given. Ignoring custom_signal_universe_metrics_m_df."
  )


})

test_that("derive_signal_universe_m_df error checking works regarding custom_signal_universe_metrics", {

  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  set.seed(123)
  #Backtest Returns
  mocked_backtest_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
    asset_turnover_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 5, sd = 3.5),
    book_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1, sd = 5),
    dps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 15, sd = 0.4),
    eps_yield = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.0005, sd = 0.3),
    mom_res_12m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 3.15, sd = 3.5),
    roe_3m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.1, sd = 2),
    sharpe_6m = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 2.5, sd = 5),
    low_idio_vol_mrkt_ewma = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 1.05, sd = 7.5)
  ), order.by = unique(toy_preprocessed_features$dates)))

  #Benchmark Returns xts
  suppressWarnings(
    mocked_benchmark_returns_m_xts <- create_meta_xts(xts::as.xts(data.frame(
      IBOV = rnorm(length(unique(toy_preprocessed_features$dates)), mean = 0.01, sd = 0.035),
      SMLL = rnorm(length(unique(toy_preprocessed_features$dates)), mean = -0.01, sd = 0.025)
    ),  order.by = unique(toy_preprocessed_features$dates))
    ))

  #Chosen Signals and Positions
  chosen_signals_and_positions <- c(asset_turnover_12m = "long", book_yield = "long", dps_yield = "long", eps_yield = "long",
                                    idio_vol_mrkt_ewma = "short", sharpe_6m = "long")

  #Mocked Signal Themes
  mocked_signal_themes_m_df <- expand.grid(
    tickers = names(mocked_backtest_returns_m_xts@data),
    dates = unique(toy_preprocessed_features$dates),
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(id = paste0(tickers,"-",dates),
                      theme = dplyr::case_when(
                        tickers %in% c("mom_res_12m", "sharpe_6m") ~ "momentum",
                        tickers %in% c("dy_med_36m", "eps_yield", "book_yield", "asset_turnover_12m", "dps_yield") ~ "value",
                        tickers %in% c("roe_3m", "low_idio_vol_mrkt_ewma") ~ "defensive"
                      )
  ) %>%  dplyr::arrange(id) %>% dplyr::select(id, tickers, dates, theme)

  signal_themes_m_df <- create_meta_dataframe(mocked_signal_themes_m_df, "st_11", type = "groups")

  features_m_df <- create_meta_dataframe(toy_preprocessed_features, "feats_123")


  #Custom Signal Universe Metrics
  tickers <- colnames(features_m_df@data)[-c(1:3)]
  corrected_tickers <- tickers
  corrected_tickers[6] <- paste0("low_", tickers[6])
  dates <- unique(features_m_df@data$dates)
  less_dates <- dates[c(1,2,3,6,7)] #Check for less dates than features_m_df

  suppressWarnings(
    short_custom_signal_universe_metrics_m_df <- expand.grid(corrected_tickers, less_dates, stringsAsFactors = FALSE) %>% dplyr::rename(tickers = Var1, dates = Var2) %>%
      dplyr::mutate(id = paste0(tickers,"-",dates)) %>%
      dplyr::select(id, tickers, dates) %>%
      dplyr::mutate(pe = runif(dplyr::n(), 0, 100), pb = runif(dplyr::n(), 0, 100), roe = runif(dplyr::n(), 0, 100),
                    div_yield = runif(dplyr::n(), 0, 100), market_cap = runif(dplyr::n(), 0, 100)) %>% dplyr::arrange(id) %>%
      create_meta_dataframe()
  )

  ##SW Config
  expect_warning(
    sw_config <- create_sb_backtest_config(sb_algorithm = "sw", custom_objective = "min_pe", rebalancing_months = 7, training_sample_size = 5, target_fwd_name = "fwd_premium_3m",
                                           chosen_signals_and_positions = chosen_signals_and_positions),
    "custom_objective is not one of typical valid heuristic performance. Please be sure that the metric is present in signal_universe_m_df"
  )


  #Non numeric data
  wrong_custom_signal_universe_metrics_m_df <- short_custom_signal_universe_metrics_m_df
  wrong_custom_signal_universe_metrics_m_df@data <- short_custom_signal_universe_metrics_m_df@data %>% dplyr::mutate(wrong_col = rep("A", dplyr::n()))

  expect_error(
    derive_signal_universe_m_df(config = sw_config, features_m_df = features_m_df,
                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                ss_backtest_results = NULL,
                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = wrong_custom_signal_universe_metrics_m_df,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "custom_signal_universe_metrics_m_df should only contain numeric values."
  )

  #custom_signal_universe does not have all tickers
  wrong_custom_signal_universe_metrics_m_df <- short_custom_signal_universe_metrics_m_df
  wrong_custom_signal_universe_metrics_m_df@data <- short_custom_signal_universe_metrics_m_df@data %>% dplyr::filter(!tickers == "asset_turnover_12m")

  expect_error(
    derive_signal_universe_m_df(config = sw_config, features_m_df = features_m_df,
                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                ss_backtest_results = NULL,
                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = wrong_custom_signal_universe_metrics_m_df,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "all signal_universe_m_df tickers should be contemplated in custom_signal_universe_metrics_m_df"
  )

  #NAs
  wrong_custom_signal_universe_metrics_m_df <- short_custom_signal_universe_metrics_m_df
  wrong_custom_signal_universe_metrics_m_df@data$pb[3] <- NA

  expect_error(
    derive_signal_universe_m_df(config = sw_config, features_m_df = features_m_df,
                                ss_backtest_results = NULL,
                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = wrong_custom_signal_universe_metrics_m_df,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "custom_signal_universe_metrics_m_df should not contain NA's"
  )

  #At least one date before first training date
  wrong_custom_signal_universe_metrics_m_df <- short_custom_signal_universe_metrics_m_df
  wrong_custom_signal_universe_metrics_m_df@data <- wrong_custom_signal_universe_metrics_m_df@data %>%
    dplyr::filter(!dates %in% c("2022-07-15", "2022-08-15", "2022-09-15", "2022-10-15", "2022-11-15"))

  expect_error(
    derive_signal_universe_m_df(config = sw_config, features_m_df = features_m_df,
                                ss_backtest_results = NULL,
                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = wrong_custom_signal_universe_metrics_m_df,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "custom_signal_universe_metrics_m_df should have at least one date before first_training_date"
  )

  #wrong custom obj
  wrong_custom_signal_universe_metrics_m_df <- short_custom_signal_universe_metrics_m_df
  wrong_custom_signal_universe_metrics_m_df@data <- wrong_custom_signal_universe_metrics_m_df@data %>% dplyr::arrange(desc(id))

  expect_error(
    derive_signal_universe_m_df(config = sw_config, features_m_df = features_m_df,
                                ss_backtest_results = NULL,
                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = wrong_custom_signal_universe_metrics_m_df,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "custom_signal_universe_metrics_m_df not coercible to meta_dataframe."
  )


  #wrong custom obj
  wrong_custom_signal_universe_metrics_m_df <- short_custom_signal_universe_metrics_m_df
  wrong_custom_signal_universe_metrics_m_df@data <- wrong_custom_signal_universe_metrics_m_df@data[-2,]

  expect_error(
    derive_signal_universe_m_df(config = sw_config, features_m_df = features_m_df,
                                ss_backtest_results = NULL,
                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = wrong_custom_signal_universe_metrics_m_df,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "custom_signal_universe_metrics_m_df should have nrows equal to tickers \\* dates"
  )

  wrong_custom_signal_universe_metrics_m_df <- short_custom_signal_universe_metrics_m_df
  wrong_custom_signal_universe_metrics_m_df@data <- wrong_custom_signal_universe_metrics_m_df@data %>% dplyr::select(-pe)


  expect_error(
    derive_signal_universe_m_df(config = sw_config, features_m_df = features_m_df,
                                ss_backtest_results = NULL,
                                backtest_returns_m_xts = mocked_backtest_returns_m_xts,
                                benchmark_returns_m_xts = mocked_benchmark_returns_m_xts,
                                signal_themes_m_df = signal_themes_m_df,
                                priors_m_df = NULL,
                                custom_signal_universe_metrics_m_df = wrong_custom_signal_universe_metrics_m_df,
                                verbose = TRUE, parallel = FALSE, winsorization_probs = c(0.05, 0.95)),
    "custom_objective not contemplated in custom_signal_universe_metrics_m_df"
  )


})
