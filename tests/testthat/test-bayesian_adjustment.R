test_that("bayesian model correctly shrinks alpha based on conservative priors",{

  #DGP 1
  ##############################
  # DGP adapted for lme4::lmer(return ~ 0 + theme + theme:market_factor_proxy + (1 + market_factor_proxy | theme:tickers))
  set.seed(123)  # For reproducibility

  # Define themes and tickers
  themes <- c("value", "growth", "momentum", "defensive", "size")
  theme_ticker_map <- list(
    value = c("book_yield", "sales_yield", "dividend_yield", "asset_yield", "eps_yield"),
    growth = c("g_eps", "g_sps", "g_dps", "sur", "g_roe", "g_roic", "g_fcfe", "g_fcf", "g_fcff"),
    momentum = c("sharpe_6m", "sharpe_12m", "alpha_6m", "alpha_12m", "sharpe_ewma", "alpha_ewma"),
    defensive = c("low_vol", "low_beta", "roe", "roic", "net_margin", "gross_margin", "low_vol_roe"),
    size = c("market_cap", "turnover_1m", "trading_volume_3m", "turnover_3m", "trading_volume_1m")
  )

  # Expand theme-signal combinations into a data frame
  theme_signal_combinations <- do.call(
    rbind,
    lapply(names(theme_ticker_map), function(theme) {
      data.frame(theme = theme, signal = theme_ticker_map[[theme]])
    })
  )

  # Define priors based on the model specification
  theme_effects_means <- c(0.05, 0.04, 0.07, 0.5, -0.03)  # Fixed intercepts for themes
  names(theme_effects_means) <- themes
  theme_effects_sds <- c(0.5, 0.6, 0.6, 0.5, 0.4)     # SD for theme intercept variability
  names(theme_effects_sds) <- themes
  theme_slopes_means <- c(0.002, -0.001, 0.03, 0.0005, -0.0005)  # Fixed slopes for market_factor_proxy
  names(theme_slopes_means) <- themes
  theme_slopes_sds <- c(0.002, 0, 0.005, 0.007, 0.011)   # SD for theme slope variability
  names(theme_slopes_sds) <- themes

  # Covariance matrix for random effects (intercept and slope) for theme:tickers
  random_intercept_tickers_sd <- 0.01    # SD for random intercepts at theme:tickers level
  random_slope_tickers_sd <- 0.003      # SD for random slopes at theme:tickers level
  correlation <- 0.2                    # Correlation between random intercept and slope
  residual_sd <- 0.0450                 # SD for residual error

  cov_matrix <- matrix(
    c(random_intercept_tickers_sd^2,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      random_slope_tickers_sd^2),
    nrow = 2
  )

  # Generate random effects for tickers
  theme_ticker_combinations <- do.call(rbind, lapply(names(theme_ticker_map), function(theme) {
    data.frame(theme = theme, ticker = theme_ticker_map[[theme]])
  }))
  n_tickers <- nrow(theme_ticker_combinations)
  random_effects_tickers <- MASS::mvrnorm(
    n = n_tickers,
    mu = c(0, 0),  # Mean of random intercepts and slopes
    Sigma = cov_matrix
  )
  random_intercepts_tickers <- random_effects_tickers[, 1]
  random_slopes_tickers <- random_effects_tickers[, 2]

  # Generate data
  n_obs_per_ticker <- 100
  return <- numeric(n_obs_per_ticker * n_tickers)

  # Predictor: market_factor_proxy
  market_factor_proxy <- rnorm(n_obs_per_ticker * n_tickers, mean = 0, sd = 1)

  # Generate monthly dates for the observations
  dates <- rep(
    seq.Date(as.Date("1980-01-01"), by = "month", length.out = n_obs_per_ticker),
    times = n_tickers
  )

  # Loop to calculate return for each observation
  for (i in seq_along(return)) {
    ticker_idx <- ((i - 1) %/% n_obs_per_ticker) + 1  # Identify signal index
    theme <- theme_ticker_combinations$theme[ticker_idx]
    ticker <- theme_ticker_combinations$ticker[ticker_idx]

    # Combine fixed effects, random effects, and residual noise
    return[i] <- rnorm(1, mean = theme_effects_means[theme], sd = theme_effects_sds[theme]) +  # Fixed intercept with variability
      rnorm(1, mean = theme_slopes_means[theme], sd = theme_slopes_sds[theme]) * market_factor_proxy[i] +  # Fixed slope with variability
      random_intercepts_tickers[ticker_idx] +                                                          # Random intercept for theme:tickers
      random_slopes_tickers[ticker_idx] * market_factor_proxy[i] +                                     # Random slope for theme:tickers
      rnorm(1, mean = 0, sd = residual_sd)                                                             # Residual noise
  }

  # Assign signal names to each observation
  signal_names <- rep(theme_signal_combinations$signal, each = n_obs_per_ticker)
  theme_names <- rep(theme_signal_combinations$theme, each = n_obs_per_ticker)

  # Create the final data frame
  simulated_data <- data.frame(
    id = paste0(signal_names, "-", dates),  # Unique ID combining signal and date
    dates = dates,                          # Monthly dates
    theme = theme_names,                    # Theme names
    tickers = signal_names,                 # Signal names
    return = return,          # Response variable
    market_factor_proxy = market_factor_proxy  # Predictor variable
  )

  # Reorder columns as requested
  simulated_data <- simulated_data[, c("id", "tickers", "dates", "return", "market_factor_proxy", "theme")]


  ##############################

  set.seed(123)
  # Define tickers
  tickers <- c("Stock A", "Stock B", "Stock C")
  # Define date sequence from "1980-01-01" to "2063-04-01" monthly
  dates <- seq(as.Date("1980-01-01"), as.Date("1988-04-01"), by = "month")
  # Define signal column names
  signal_columns <- simulated_data$tickers %>% unique()
  # Number of tickers and dates
  num_tickers <- length(tickers)
  num_dates <- length(dates)

  # Create a data frame with all combinations of tickers and dates
  signals_m_df <- expand.grid(tickers = tickers, dates = dates) %>%
    dplyr::arrange(tickers, dates) %>%
    dplyr::mutate(
      id = paste(tickers, format(dates, "%Y-%m-%d"), sep = "-")
    ) %>%
    dplyr::select(id, tickers, dates)

  # Function to generate simulated data for signals
  generate_signals <- function(n, mean = 0, sd = 1) {
    rnorm(n, mean, sd)
  }

  # Add simulated signal data to the data frame
  for (signal in signal_columns) {
    # Customize mean and sd based on signal type if needed
    # For simplicity, using mean = 0 and sd = 1 for all signals
    signals_m_df[[signal]] <- generate_signals(n = nrow(signals_m_df))
  }

  chosen_signals_and_positions <- rep("long", length(signal_columns))
  names(chosen_signals_and_positions) <- signal_columns

  backtest_returns_m_xts <- simulated_data %>% tidyr::pivot_wider(id_cols = dates, names_from = tickers, values_from = return)
  backtest_returns_m_xts <- xts::as.xts(backtest_returns_m_xts[, -1], order.by = backtest_returns_m_xts$dates)

  correct_names <-   colnames(backtest_returns_m_xts)
  correct_names[chosen_signals_and_positions == "short"] <- paste0("low_", names(chosen_signals_and_positions)[chosen_signals_and_positions])

  colnames(backtest_returns_m_xts) <- correct_names


  #get selected info
  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    signal_themes_m_df = NULL,
    chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_m_xts = backtest_returns_m_xts
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_market_factor_proxy_m_xts <- xts::as.xts(data.frame(IBOV = rnorm(num_dates, 0, 1)), order.by = dates)
  #selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df


  #current info
  current_date <- "1987-11-01"
  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:95), ]

  selected_market_factor_proxy_m_xts_upd_ref <- selected_market_factor_proxy_m_xts[c(1:95), "IBOV"]

  selected_signal_themes_m_d_ref <- tibble::enframe(theme_ticker_map, name = "theme", value = "tickers") %>%
    tidyr::unnest(tickers) %>%
    dplyr::arrange(theme, tickers) %>% as.data.frame()
  selected_signal_themes_m_d_ref$dates <- current_date
  selected_signal_themes_m_d_ref$id <- paste0(selected_signal_themes_m_d_ref$tickers,"-",selected_signal_themes_m_d_ref$dates)

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_d_ref[, c("id", "tickers", "dates", "theme")]


  #Regularizer
  ##############################
  # DGP adapted for lme4::lmer(return ~ 0 + theme + theme:market_factor_proxy + (1 + market_factor_proxy | theme:tickers))
  set.seed(123)  # For reproducibility

  # Define themes and tickers
  themes <- c("value", "growth", "momentum", "defensive", "size")
  theme_ticker_map <- list(
    value = c("book_yield", "sales_yield", "dividend_yield", "asset_yield", "eps_yield",
              "ev_ebitda", "ev_ebit", "fcf_yield", "fcfe_yield", "ev_fcff"),
    growth = c("g_eps", "g_sps", "g_dps", "sur", "g_roe", "g_roic", "g_fcfe", "g_fcf", "g_fcff",
               "g_eps_36m", "g_sps_36m", "g_dps_36m", "sur_36m", "g_roe_36m", "g_roic_36m",
               "g_fcfe_36m", "g_fcf_36m", "g_fcff_36m"),
    momentum = c("sharpe_6m", "sharpe_12m", "alpha_6m", "alpha_12m", "sharpe_ewma", "alpha_ewma",
                 "return_6m", "return_12m", "sharpe_3m", "alpha_3m", "return_3m"),
    defensive = c("low_vol", "low_beta", "roe", "roic", "net_margin", "gross_margin", "low_vol_roe",
                  "low_vol_roic", "low_leverage", "fscore", "fcff_at", "fcfe_bv"),
    size = c("market_cap", "turnover_1m", "trading_volume_3m", "turnover_3m", "trading_volume_1m")
  )

  # Expand theme-signal combinations into a data frame
  theme_signal_combinations <- do.call(
    rbind,
    lapply(names(theme_ticker_map), function(theme) {
      data.frame(theme = theme, signal = theme_ticker_map[[theme]])
    })
  )

  # Define priors based on the model specification
  theme_effects_means <- c(0, 0, 0, 0, 2.5)  # Fixed intercepts for themes
  names(theme_effects_means) <- themes
  theme_effects_sds <- c(0.000001, 0.0000001, 0.0000001, 0.00001, 0.000001)     # SD for theme intercept variability
  names(theme_effects_sds) <- themes
  theme_slopes_means <- c(0.002, -0.001, 0.03, 0.0005, -0.0005)  # Fixed slopes for market_factor_proxy
  names(theme_slopes_means) <- themes
  theme_slopes_sds <- c(0.002, 0, 0.005, 0.007, 0.011)   # SD for theme slope variability
  names(theme_slopes_sds) <- themes

  # Covariance matrix for random effects (intercept and slope) for theme:tickers
  random_intercept_tickers_sd <- 0.001    # SD for random intercepts at theme:tickers level
  random_slope_tickers_sd <- 0.003      # SD for random slopes at theme:tickers level
  correlation <- 0.2                    # Correlation between random intercept and slope
  residual_sd <- 0.00450                 # SD for residual error

  cov_matrix <- matrix(
    c(random_intercept_tickers_sd^2,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      random_slope_tickers_sd^2),
    nrow = 2
  )

  # Generate random effects for tickers
  theme_ticker_combinations <- do.call(rbind, lapply(names(theme_ticker_map), function(theme) {
    data.frame(theme = theme, ticker = theme_ticker_map[[theme]])
  }))
  n_tickers <- nrow(theme_ticker_combinations)
  random_effects_tickers <- MASS::mvrnorm(
    n = n_tickers,
    mu = c(0, 0),  # Mean of random intercepts and slopes
    Sigma = cov_matrix
  )
  random_intercepts_tickers <- random_effects_tickers[, 1]
  random_slopes_tickers <- random_effects_tickers[, 2]

  # Generate data
  n_obs_per_ticker <- 3000
  return <- numeric(n_obs_per_ticker * n_tickers)

  # Predictor: market_factor_proxy
  market_factor_proxy <- rnorm(n_obs_per_ticker * n_tickers, mean = 0, sd = 1)

  # Generate monthly dates for the observations
  dates <- rep(
    seq.Date(as.Date("1980-01-01"), by = "month", length.out = n_obs_per_ticker),
    times = n_tickers
  )

  # Loop to calculate return for each observation
  for (i in seq_along(return)) {
    ticker_idx <- ((i - 1) %/% n_obs_per_ticker) + 1  # Identify signal index
    theme <- theme_ticker_combinations$theme[ticker_idx]
    ticker <- theme_ticker_combinations$ticker[ticker_idx]

    # Combine fixed effects, random effects, and residual noise
    return[i] <- rnorm(1, mean = theme_effects_means[theme], sd = theme_effects_sds[theme]) +  # Fixed intercept with variability
      rnorm(1, mean = theme_slopes_means[theme], sd = theme_slopes_sds[theme]) * market_factor_proxy[i] +  # Fixed slope with variability
      random_intercepts_tickers[ticker_idx] +                                                          # Random intercept for theme:tickers
      random_slopes_tickers[ticker_idx] * market_factor_proxy[i] +                                     # Random slope for theme:tickers
      rnorm(1, mean = 0, sd = residual_sd)                                                             # Residual noise
  }

  # Assign signal names to each observation
  signal_names <- rep(theme_signal_combinations$signal, each = n_obs_per_ticker)
  theme_names <- rep(theme_signal_combinations$theme, each = n_obs_per_ticker)

  # Create the final data frame
  simulated_data <- data.frame(
    id = paste0(signal_names, "-", dates),  # Unique ID combining signal and date
    dates = dates,                          # Monthly dates
    theme = theme_names,                    # Theme names
    tickers = signal_names,                 # Signal names
    return = return,          # Response variable
    market_factor_proxy = market_factor_proxy  # Predictor variable
  )

  # Reorder columns as requested
  simulated_data <- simulated_data[, c("id", "tickers", "dates", "return", "market_factor_proxy", "theme")]


  ##############################

  priors_m_df <- simulated_data
  priors_m_upd_ref <- priors_m_df[priors_m_df$dates <= current_date,]

  #expected results
  expected_result <- summarize_performance(
    model_structure = "partial_pooled", model_spec_theme_level = "theme_specific_intercept_fixed_slope",
    lmer_control = list(lmer_optimizer = "Nelder_Mead", lmer_optimization_objective = "REML", hierarchical_p_value_method = "Satterthwaite"),
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
    selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref
  )


  future::plan("multisession")
  suppressWarnings(
  result <- bayesian_adjustment(signal_universe_m_d_ref = expected_result$signal_universe_m_d_ref,
                                selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
                                priors_m_upd_ref = priors_m_upd_ref, v = 30, lmer_optimizer = "Nelder_Mead", user_priors = NULL,
                                model_spec_theme_level = "theme_specific_intercept_fixed_slope",
                                selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
                                iter = 2000, warmup = 1000
                                )
  )

  comparison <- result$posterior_signal_universe_m_d_ref[,c("id", "tickers", "theme_alpha", "individual_alpha", "alpha_t_stat",
                                                           "posterior_theme_alpha", "posterior_individual_alpha", "posterior_alpha_t_stat")]

  comparison_value <- comparison[which(comparison$tickers %in%
                                 theme_signal_combinations$signal[which(theme_signal_combinations$theme == "value")]
                                 ),]

  expect_lt(mean(comparison_value$posterior_alpha_t_stat), mean(comparison_value$alpha_t_stat))
  expect_lt(mean(comparison_value$posterior_individual_alpha), mean(comparison_value$individual_alpha))


  comparison_growth <- comparison[which(comparison$tickers %in%
                                         theme_signal_combinations$signal[which(theme_signal_combinations$theme == "growth")]
  ),]

  expect_lt(mean(comparison_growth$posterior_alpha_t_stat), mean(comparison_growth$alpha_t_stat))
  expect_lt(mean(comparison_growth$posterior_individual_alpha), mean(comparison_growth$individual_alpha))



  comparison_momentum <- comparison[which(comparison$tickers %in%
                                          theme_signal_combinations$signal[which(theme_signal_combinations$theme == "momentum")]
  ),]

  expect_lt(mean(comparison_momentum$posterior_alpha_t_stat), mean(comparison_momentum$alpha_t_stat))
  expect_lt(mean(comparison_momentum$posterior_individual_alpha), mean(comparison_momentum$individual_alpha))


  comparison_defensive <- comparison[which(comparison$tickers %in%
                                            theme_signal_combinations$signal[which(theme_signal_combinations$theme == "defensive")]
  ),]

  expect_lt(mean(comparison_defensive$posterior_alpha_t_stat), mean(comparison_defensive$alpha_t_stat))
  expect_lt(mean(comparison_defensive$posterior_individual_alpha), mean(comparison_defensive$individual_alpha))



  comparison_size <- comparison[which(comparison$tickers %in%
                                      theme_signal_combinations$signal[which(theme_signal_combinations$theme == "size")]
  ),]

  expect_gt(mean(comparison_size$posterior_alpha_t_stat), mean(comparison_size$alpha_t_stat))
  expect_gt(mean(comparison_size$posterior_individual_alpha), mean(comparison_size$individual_alpha))


})



