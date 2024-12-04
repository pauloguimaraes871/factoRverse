test_that("derive_informative_priors_from_data works for model spec 1", {

  #DGP 1
  ##############################
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
  # Each row represents a theme and its corresponding signal
  theme_signal_combinations <- do.call(
    rbind,
    lapply(names(theme_ticker_map), function(theme) {
      data.frame(theme = theme, signal = theme_ticker_map[[theme]])
    })
  )

  # Define priors based on the model specification
  fixed_intercept_mean <- 0.000  # Mean of the intercept (fixed effect)
  fixed_intercept_sd <- 0.00250    # SD of the intercept (fixed effect)
  fixed_slope_mean <- 0.000      # Mean of the slope for market_factor_proxy (fixed effect)
  fixed_slope_sd <- 0.005        # SD of the slope for market_factor_proxy (fixed effect)

  random_intercept_theme_sd <- 0.005     # SD for random intercept at theme level
  random_intercept_tickers_sd  <- 0.01    # SD for random intercept at theme:tickers level
  random_slope_tickers_sd  <- 0.002        # SD for random slope at theme:tickers level
  residual_sd <- 0.0250                   # SD for residual error

  # Correlation between random intercept and slope for theme:tickers level
  correlation <- 0.2  # Approximate correlation from LKJ prior

  # Covariance matrix for random effects (intercept and slope) for theme:tickers
  cov_matrix <- matrix(
    c(random_intercept_tickers_sd^2,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      random_slope_tickers_sd^2),
    nrow = 2
  )

  # Generate data
  n_obs_per_ticker <- 2000

  # Expand the theme-ticker combinations
  theme_ticker_combinations <- do.call(rbind, lapply(names(theme_ticker_map), function(theme) {
    data.frame(theme = theme, ticker = theme_ticker_map[[theme]])
  }))

  # Generate random effects
  n_tickers <- nrow(theme_ticker_combinations)
  random_effects_tickers <- MASS::mvrnorm(n_tickers, mu = c(0, 0), Sigma = cov_matrix)

  # Random intercepts for themes
  random_intercepts_theme <- rnorm(length(themes), mean = 0, sd = random_intercept_theme_sd)
  names(random_intercepts_theme) <- themes

  # Predictor: market_factor_proxy
  market_factor_proxy <- rnorm(n_obs_per_ticker * n_tickers, mean = 0, sd = 1)

  # Generate monthly dates for the observations
  dates <- rep(
    seq.Date(as.Date("1980-01-01"), by = "month", length.out = n_obs_per_ticker),
    times = n_tickers
  )

  # Initialize the response variable (active_return)
  active_return <- numeric(length(market_factor_proxy))

  # Loop to calculate active_return for each observation
  for (i in seq_along(active_return)) {
    ticker_idx <- ((i - 1) %/% n_obs_per_ticker) + 1  # Identify signal index
    theme <- theme_ticker_combinations$theme[ticker_idx]
    ticker <- theme_ticker_combinations$ticker[ticker_idx]

    # Combine fixed effects, random effects, and residual noise
    active_return[i] <- rnorm(1, mean = fixed_intercept_mean, sd = fixed_intercept_sd) +  # Fixed intercept
      rnorm(1, mean = fixed_slope_mean, sd = fixed_slope_sd) * market_factor_proxy[i] +  # Fixed slope
      random_intercepts_theme[theme] +                                                               # Random intercept for theme
      random_effects_tickers[ticker_idx, 1] +                                                    # Random intercept for theme:tickers
      random_effects_tickers[ticker_idx, 2] * market_factor_proxy[i] +                           # Random slope for theme:tickers
      rnorm(1, mean = 0, sd = residual_sd)                                               # Residual noise
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
    active_return = active_return,          # Response variable
    market_factor_proxy = market_factor_proxy  # Predictor variable
  )

  # Reorder columns as requested
  simulated_data <- simulated_data[, c("id", "tickers", "dates", "active_return", "market_factor_proxy", "theme")]

  ##############################

  #Get priors for dgp 1
  results_dgp_1 <- derive_informative_priors_from_data(priors_m_upd_ref = simulated_data,
                                                 model_spec_theme_level = "random_intercept",
                                                 half_t_distribution = 30)

  expect_s3_class(results_dgp_1$priors, "brmsprior")
  expect_equal(results_dgp_1$priors$class, c("Intercept", "b", "sd", "sd", "sd", "sigma", "cor"))

  #Check if estimates are somewhat as expected
  #Fixed Intercept
  expect_equal(as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[1])),
               fixed_intercept_mean, tolerance = 0.0025)
  expect_equal(as.numeric(sub("normal\\(-?[0-9.]+, ([0-9.]+)\\)", "\\1", results_dgp_1$priors$prior[1])),
               fixed_intercept_sd, tolerance = 0.0025)


  #Random Intercept Tickers
  # Extract degrees of freedom
  degrees_of_freedom <- as.numeric(sub("student_t\\(([0-9.]+),.*", "\\1", results_dgp_1$priors$prior[3]))
  expect_equal(degrees_of_freedom, 30)

  # Extract scale
  scale <- as.numeric(sub("student_t\\([0-9.]+,-?[0-9.]+,([0-9.]+)\\)", "\\1", results_dgp_1$priors$prior[3]))
  expect_lt(abs(scale - random_intercept_tickers_sd), 0.0025)

  #Random Intercept Theme
  # Extract degrees of freedom
  degrees_of_freedom <- as.numeric(sub("student_t\\(([0-9.]+),.*", "\\1", results_dgp_1$priors$prior[5]))
  expect_equal(degrees_of_freedom, 30)

  # Extract scale
  scale <- as.numeric(sub("student_t\\([0-9.]+,-?[0-9.]+,([0-9.]+)\\)", "\\1", results_dgp_1$priors$prior[5]))
  expect_lt(abs(scale - random_intercept_theme_sd), 0.0025)

  # Residual SD
  degrees_of_freedom <- as.numeric(sub("student_t\\(([0-9.]+),.*", "\\1", results_dgp_1$priors$prior[6]))
  expect_equal(degrees_of_freedom, 30)

  scale <- as.numeric(sub("student_t\\([0-9.]+,-?[0-9.]+,([0-9.]+)\\)", "\\1", results_dgp_1$priors$prior[6]))
  expect_lt(abs(scale - residual_sd), 0.0025)


  #Compare LME model
  lme_model_1 <- lme4::lmer(active_return ~ market_factor_proxy + (1 | theme) + (1 + market_factor_proxy | theme:tickers),
                        data = simulated_data)
  expect_equal(coef(results_dgp_1$model), coef(lme_model_1))
  expect_equal(lme4::fixef(results_dgp_1$model), lme4::fixef(lme_model_1))
  expect_equal(lme4::ranef(results_dgp_1$model), lme4::ranef(lme_model_1))


  #DGP 2
  ##############################
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
  # Each row represents a theme and its corresponding signal
  theme_signal_combinations <- do.call(
    rbind,
    lapply(names(theme_ticker_map), function(theme) {
      data.frame(theme = theme, signal = theme_ticker_map[[theme]])
    })
  )

  # Define priors based on the model specification
  fixed_intercept_mean <- 0.005  # Mean of the intercept (fixed effect)
  fixed_intercept_sd <- 0.001    # SD of the intercept (fixed effect)
  fixed_slope_mean <- 0.003      # Mean of the slope for market_factor_proxy (fixed effect)
  fixed_slope_sd <- 0.002        # SD of the slope for market_factor_proxy (fixed effect)

  random_intercept_theme_sd <- 0.003     # SD for random intercept at theme level
  random_intercept_tickers_sd  <- 0.003    # SD for random intercept at theme:tickers level
  random_slope_tickers_sd  <- 0.05        # SD for random slope at theme:tickers level
  residual_sd <- 0.0150                   # SD for residual error

  # Correlation between random intercept and slope for theme:tickers level
  correlation <- 0.2  # Approximate correlation from LKJ prior

  # Covariance matrix for random effects (intercept and slope) for theme:tickers
  cov_matrix <- matrix(
    c(random_intercept_tickers_sd^2,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      random_slope_tickers_sd^2),
    nrow = 2
  )

  # Generate data
  n_obs_per_ticker <- 2000

  # Expand the theme-ticker combinations
  theme_ticker_combinations <- do.call(rbind, lapply(names(theme_ticker_map), function(theme) {
    data.frame(theme = theme, ticker = theme_ticker_map[[theme]])
  }))

  # Generate random effects
  n_tickers <- nrow(theme_ticker_combinations)
  random_effects_tickers <- MASS::mvrnorm(n_tickers, mu = c(0, 0), Sigma = cov_matrix)

  # Random intercepts for themes
  random_intercepts_theme <- rnorm(length(themes), mean = 0, sd = random_intercept_theme_sd)
  names(random_intercepts_theme) <- themes

  # Predictor: market_factor_proxy
  market_factor_proxy <- rnorm(n_obs_per_ticker * n_tickers, mean = 0, sd = 1)

  # Generate monthly dates for the observations
  dates <- rep(
    seq.Date(as.Date("1980-01-01"), by = "month", length.out = n_obs_per_ticker),
    times = n_tickers
  )

  # Initialize the response variable (active_return)
  active_return <- numeric(length(market_factor_proxy))

  # Loop to calculate active_return for each observation
  for (i in seq_along(active_return)) {
    ticker_idx <- ((i - 1) %/% n_obs_per_ticker) + 1  # Identify signal index
    theme <- theme_ticker_combinations$theme[ticker_idx]
    ticker <- theme_ticker_combinations$ticker[ticker_idx]

    # Combine fixed effects, random effects, and residual noise
    active_return[i] <- rnorm(1, mean = fixed_intercept_mean, sd = fixed_intercept_sd) +  # Fixed intercept
      rnorm(1, mean = fixed_slope_mean, sd = fixed_slope_sd) * market_factor_proxy[i] +  # Fixed slope
      random_intercepts_theme[theme] +                                                               # Random intercept for theme
      random_effects_tickers[ticker_idx, 1] +                                                    # Random intercept for theme:tickers
      random_effects_tickers[ticker_idx, 2] * market_factor_proxy[i] +                           # Random slope for theme:tickers
      rnorm(1, mean = 0, sd = residual_sd)                                               # Residual noise
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
    active_return = active_return,          # Response variable
    market_factor_proxy = market_factor_proxy  # Predictor variable
  )

  # Reorder columns as requested
  simulated_data <- simulated_data[, c("id", "tickers", "dates", "active_return", "market_factor_proxy", "theme")]

  ##############################

  #Get priors for dgp 2
  results_dgp_2 <- derive_informative_priors_from_data(priors_m_upd_ref = simulated_data,
                                                       model_spec_theme_level = "random_intercept",
                                                       half_t_distribution = 30)

  #Fixed Intercept
  expect_lt(as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[1])),
            as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_2$priors$prior[1]))
            )

  expect_gt(as.numeric(sub("normal\\(-?[0-9.]+, ([0-9.]+)\\)", "\\1", results_dgp_1$priors$prior[1])),
            as.numeric(sub("normal\\(-?[0-9.]+, ([0-9.]+)\\)", "\\1", results_dgp_2$priors$prior[1]))
  )

  #Random Intercept Tickers
  expect_gt(
    as.numeric(sub("student_t\\([0-9.]+,-?[0-9.]+,([0-9.]+)\\)", "\\1", results_dgp_1$priors$prior[3])),
    as.numeric(sub("student_t\\([0-9.]+,-?[0-9.]+,([0-9.]+)\\)", "\\1", results_dgp_2$priors$prior[3]))
  )

  #Random Intercept Theme
  expect_gt(
    as.numeric(sub("student_t\\([0-9.]+,-?[0-9.]+,([0-9.]+)\\)", "\\1", results_dgp_1$priors$prior[5])),
    as.numeric(sub("student_t\\([0-9.]+,-?[0-9.]+,([0-9.]+)\\)", "\\1", results_dgp_2$priors$prior[5]))
  )




})

test_that("derive_informative_priors_from_data works for model spec 2", {

  #DGP 1
  ##############################
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
  # Each row represents a theme and its corresponding signal
  theme_signal_combinations <- do.call(
    rbind,
    lapply(names(theme_ticker_map), function(theme) {
      data.frame(theme = theme, signal = theme_ticker_map[[theme]])
    })
  )

  # Define priors based on the model specification
  theme_effects_means <- c(0.0125, 0.005, 0.01, 0.003, -0.003)  # Fixed effects for themes
  names(theme_effects_means) <- themes
  theme_effects_sds <- c(0.003, 0.002, 0.002, 0.002, 0.003)  # SD for random effects at theme level
  names(theme_effects_sds) <- themes

  fixed_slope_mean <- 0.000      # Mean of the slope for market_factor_proxy (fixed effect)
  fixed_slope_sd <- 0.005        # SD of the slope for market_factor_proxy (fixed effect)

  random_intercept_tickers_sd  <- 0.01    # SD for random intercept at theme:tickers level
  random_slope_tickers_sd  <- 0.003        # SD for random slope at theme:tickers level
  residual_sd <- 0.0450                   # SD for residual error

  # Correlation between random intercept and slope for theme:tickers level
  correlation <- 0.2  # Approximate correlation from LKJ prior

  # Covariance matrix for random effects (intercept and slope) for theme:tickers
  cov_matrix <- matrix(
    c(random_intercept_tickers_sd^2,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      random_slope_tickers_sd^2),
    nrow = 2
  )

  # Generate data
  n_obs_per_ticker <- 3000

  # Expand the theme-ticker combinations
  theme_ticker_combinations <- do.call(rbind, lapply(names(theme_ticker_map), function(theme) {
    data.frame(theme = theme, ticker = theme_ticker_map[[theme]])
  }))

  # Generate random effects
  n_tickers <- nrow(theme_ticker_combinations)
  random_effects_tickers <- MASS::mvrnorm(n_tickers, mu = c(0, 0), Sigma = cov_matrix)

  # Predictor: market_factor_proxy
  market_factor_proxy <- rnorm(n_obs_per_ticker * n_tickers, mean = 0, sd = 1)

  # Generate monthly dates for the observations
  dates <- rep(
    seq.Date(as.Date("1980-01-01"), by = "month", length.out = n_obs_per_ticker),
    times = n_tickers
  )

  # Initialize the response variable (active_return)
  active_return <- numeric(length(market_factor_proxy))

  # Loop to calculate active_return for each observation
  for (i in seq_along(active_return)) {
    ticker_idx <- ((i - 1) %/% n_obs_per_ticker) + 1  # Identify signal index
    theme <- theme_ticker_combinations$theme[ticker_idx]
    ticker <- theme_ticker_combinations$ticker[ticker_idx]

    # Combine fixed effects, random effects, and residual noise
    active_return[i] <- rnorm(1, mean = theme_effects_means[theme], sd = theme_effects_sds[theme]) +   # Fixed intercept
      rnorm(1, mean = fixed_slope_mean, sd = fixed_slope_sd) * market_factor_proxy[i] +  # Fixed slope
      random_effects_tickers[ticker_idx, 1] +                                                    # Random intercept for theme:tickers
      random_effects_tickers[ticker_idx, 2] * market_factor_proxy[i] +                           # Random slope for theme:tickers
      rnorm(1, mean = 0, sd = residual_sd)                                               # Residual noise
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
    active_return = active_return,          # Response variable
    market_factor_proxy = market_factor_proxy  # Predictor variable
  )

  # Reorder columns as requested
  simulated_data <- simulated_data[, c("id", "tickers", "dates", "active_return", "market_factor_proxy", "theme")]

  ##############################

  #Get priors for dgp 1
  results_dgp_1 <- derive_informative_priors_from_data(priors_m_upd_ref = simulated_data,
                                                       model_spec_theme_level = "fixed_intercepts",
                                                       half_t_distribution = 30)

  expect_s3_class(results_dgp_1$priors, "brmsprior")
  expect_equal(results_dgp_1$priors$class, c(rep("b", length(unique(theme_names))), "b", "sd", "sd", "sigma", "cor"))

  #Check if estimates are somewhat as expected
  #Fixed Intercepts
  defensive_mean_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[1]))
  growth_mean_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[2]))
  momentum_mean_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[3]))
  size_mean_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[4]))
  value_mean_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[5]))

  #Check if order is expected
  themes_means_ests <- c(defensive_mean_est, growth_mean_est, momentum_mean_est, size_mean_est, value_mean_est)
  names(themes_means_ests) <- c("defensive", "growth", "momentum", "size", "value")

  expect_equal(names(themes_means_ests)[order(themes_means_ests)],
               names(theme_effects_means)[order(theme_effects_means)])


  #Random Intercept Tickers
  # Extract degrees of freedom
  degrees_of_freedom <- as.numeric(sub("student_t\\(([0-9.]+),.*", "\\1", results_dgp_1$priors$prior[7]))
  expect_equal(degrees_of_freedom, 30)

  # Extract scale
  scale <- as.numeric(sub("student_t\\([0-9.]+,-?[0-9.]+,([0-9.]+)\\)", "\\1", results_dgp_1$priors$prior[7]))
  expect_lt(abs(scale - random_intercept_tickers_sd), 0.0025)


  # Residual SD
  degrees_of_freedom <- as.numeric(sub("student_t\\(([0-9.]+),.*", "\\1", results_dgp_1$priors$prior[9]))
  expect_equal(degrees_of_freedom, 30)

  scale <- as.numeric(sub("student_t\\([0-9.]+,-?[0-9.]+,([0-9.]+)\\)", "\\1", results_dgp_1$priors$prior[9]))
  expect_lt(abs(scale - residual_sd), 0.0025)


  #Compare LME model
  lme_model_1 <- lme4::lmer(active_return ~ 0 + theme + market_factor_proxy + (1 + market_factor_proxy | theme:tickers),
                            data = simulated_data)

  expect_equal(coef(results_dgp_1$model), coef(lme_model_1))
  expect_equal(lme4::fixef(results_dgp_1$model), lme4::fixef(lme_model_1))
  expect_equal(lme4::ranef(results_dgp_1$model), lme4::ranef(lme_model_1))


  #DGP 2
  ##############################
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
  # Each row represents a theme and its corresponding signal
  theme_signal_combinations <- do.call(
    rbind,
    lapply(names(theme_ticker_map), function(theme) {
      data.frame(theme = theme, signal = theme_ticker_map[[theme]])
    })
  )

  # Define priors based on the model specification
  theme_effects_means <- c(0.001, 0.005, -0.04, 0.03, 0.04)  # Fixed effects for themes
  names(theme_effects_means) <- themes
  theme_effects_sds <- c(0.003, 0.002, 0.002, 0.002, 0.003)  # SD for random effects at theme level
  names(theme_effects_sds) <- themes

  fixed_slope_mean <- 0.000      # Mean of the slope for market_factor_proxy (fixed effect)
  fixed_slope_sd <- 0.005        # SD of the slope for market_factor_proxy (fixed effect)

  random_intercept_tickers_sd  <- 0.01    # SD for random intercept at theme:tickers level
  random_slope_tickers_sd  <- 0.003        # SD for random slope at theme:tickers level
  residual_sd <- 0.0450                   # SD for residual error

  # Correlation between random intercept and slope for theme:tickers level
  correlation <- 0.2  # Approximate correlation from LKJ prior

  # Covariance matrix for random effects (intercept and slope) for theme:tickers
  cov_matrix <- matrix(
    c(random_intercept_tickers_sd^2,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      correlation * random_intercept_tickers_sd * random_slope_tickers_sd,
      random_slope_tickers_sd^2),
    nrow = 2
  )

  # Generate data
  n_obs_per_ticker <- 3000

  # Expand the theme-ticker combinations
  theme_ticker_combinations <- do.call(rbind, lapply(names(theme_ticker_map), function(theme) {
    data.frame(theme = theme, ticker = theme_ticker_map[[theme]])
  }))

  # Generate random effects
  n_tickers <- nrow(theme_ticker_combinations)
  random_effects_tickers <- MASS::mvrnorm(n_tickers, mu = c(0, 0), Sigma = cov_matrix)

  # Predictor: market_factor_proxy
  market_factor_proxy <- rnorm(n_obs_per_ticker * n_tickers, mean = 0, sd = 1)

  # Generate monthly dates for the observations
  dates <- rep(
    seq.Date(as.Date("1980-01-01"), by = "month", length.out = n_obs_per_ticker),
    times = n_tickers
  )

  # Initialize the response variable (active_return)
  active_return <- numeric(length(market_factor_proxy))

  # Loop to calculate active_return for each observation
  for (i in seq_along(active_return)) {
    ticker_idx <- ((i - 1) %/% n_obs_per_ticker) + 1  # Identify signal index
    theme <- theme_ticker_combinations$theme[ticker_idx]
    ticker <- theme_ticker_combinations$ticker[ticker_idx]

    # Combine fixed effects, random effects, and residual noise
    active_return[i] <- rnorm(1, mean = theme_effects_means[theme], sd = theme_effects_sds[theme]) +   # Fixed intercept
      rnorm(1, mean = fixed_slope_mean, sd = fixed_slope_sd) * market_factor_proxy[i] +  # Fixed slope
      random_effects_tickers[ticker_idx, 1] +                                                    # Random intercept for theme:tickers
      random_effects_tickers[ticker_idx, 2] * market_factor_proxy[i] +                           # Random slope for theme:tickers
      rnorm(1, mean = 0, sd = residual_sd)                                               # Residual noise
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
    active_return = active_return,          # Response variable
    market_factor_proxy = market_factor_proxy  # Predictor variable
  )

  # Reorder columns as requested
  simulated_data <- simulated_data[, c("id", "tickers", "dates", "active_return", "market_factor_proxy", "theme")]

  ##############################

  #Get priors for dgp 2
  results_dgp_2 <- derive_informative_priors_from_data(priors_m_upd_ref = simulated_data,
                                                       model_spec_theme_level = "fixed_intercepts",
                                                       half_t_distribution = 30)

  #Fixed Intercepts
  #Defensive
  expect_lt(as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[1])),
            as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_2$priors$prior[1]))
  )

  #Growth
  expect_equal(as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[2])),
               as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_2$priors$prior[2]))
  )

  #Momentum
  expect_gt(as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[3])),
               as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_2$priors$prior[3]))
  )

  #Size
  expect_lt(as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[4])),
            as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_2$priors$prior[4]))
  )

  #Value
  expect_gt(as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[5])),
            as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_2$priors$prior[5]))
  )


})

test_that("derive_informative_priors_from_data works for model spec 3", {

  #DGP 1
  ##############################
  # DGP adapted for lme4::lmer(active_return ~ 0 + theme + theme:market_factor_proxy + (1 + market_factor_proxy | theme:tickers))
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
  theme_effects_means <- c(0.015, 0.004, 0.012, 0.003, -0.003)  # Fixed intercepts for themes
  names(theme_effects_means) <- themes
  theme_effects_sds <- c(0.003, 0.002, 0.002, 0.002, 0.003)     # SD for theme intercept variability
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
  n_obs_per_ticker <- 3000
  active_return <- numeric(n_obs_per_ticker * n_tickers)

  # Predictor: market_factor_proxy
  market_factor_proxy <- rnorm(n_obs_per_ticker * n_tickers, mean = 0, sd = 1)

  # Generate monthly dates for the observations
  dates <- rep(
    seq.Date(as.Date("1980-01-01"), by = "month", length.out = n_obs_per_ticker),
    times = n_tickers
  )

  # Loop to calculate active_return for each observation
  for (i in seq_along(active_return)) {
    ticker_idx <- ((i - 1) %/% n_obs_per_ticker) + 1  # Identify signal index
    theme <- theme_ticker_combinations$theme[ticker_idx]
    ticker <- theme_ticker_combinations$ticker[ticker_idx]

    # Combine fixed effects, random effects, and residual noise
    active_return[i] <- rnorm(1, mean = theme_effects_means[theme], sd = theme_effects_sds[theme]) +  # Fixed intercept with variability
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
    active_return = active_return,          # Response variable
    market_factor_proxy = market_factor_proxy  # Predictor variable
  )

  # Reorder columns as requested
  simulated_data <- simulated_data[, c("id", "tickers", "dates", "active_return", "market_factor_proxy", "theme")]


  ##############################

  #Get priors for dgp 1
  results_dgp_1 <- derive_informative_priors_from_data(priors_m_upd_ref = simulated_data,
                                                       model_spec_theme_level = "fixed_intercepts_and_slopes",
                                                       half_t_distribution = 30, lmer_optimizer = "Nelder_Mead")

  expect_s3_class(results_dgp_1$priors, "brmsprior")
  expect_equal(results_dgp_1$priors$class, c(rep("b", length(unique(theme_names))),
                                             rep("b", length(unique(theme_names))),
                                             "sd", "sd", "sigma", "cor"))

  #Check if estimates are somewhat as expected
  #Fixed Intercepts
  defensive_mean_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[1]))
  growth_mean_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[2]))
  momentum_mean_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[3]))
  size_mean_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[4]))
  value_mean_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[5]))

  #Check if order is expected
  themes_means_ests <- c(defensive_mean_est, growth_mean_est, momentum_mean_est, size_mean_est, value_mean_est)
  names(themes_means_ests) <- c("defensive", "growth", "momentum", "size", "value")

  expect_equal(names(themes_means_ests)[order(themes_means_ests)],
               names(theme_effects_means)[order(theme_effects_means)])

  #Fixed Slopes
  defensive_slope_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[6]))
  growth_slope_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[7]))
  momentum_slope_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[8]))
  size_slope_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[9]))
  value_slope_est <- as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[10]))

  #Check if order is expected
  themes_slopes_ests <- c(defensive_slope_est, growth_slope_est, momentum_slope_est,
                          size_slope_est, value_slope_est)
  names(themes_slopes_ests) <- c("defensive", "growth", "momentum", "size", "value")

  expect_equal(names(themes_slopes_ests)[order(themes_slopes_ests)],
               names(theme_slopes_means)[order(theme_slopes_means)])

  #Random Intercept Tickers
  # Extract degrees of freedom
  degrees_of_freedom <- as.numeric(sub("student_t\\(([0-9.]+),.*", "\\1", results_dgp_1$priors$prior[11]))
  expect_equal(degrees_of_freedom, 30)

  # Extract scale
  scale <- as.numeric(sub("student_t\\([0-9.]+,-?[0-9.]+,([0-9.]+)\\)", "\\1", results_dgp_1$priors$prior[11]))
  expect_lt(abs(scale - random_intercept_tickers_sd), 0.0025)


  # Residual SD
  degrees_of_freedom <- as.numeric(sub("student_t\\(([0-9.]+),.*", "\\1", results_dgp_1$priors$prior[13]))
  expect_equal(degrees_of_freedom, 30)

  scale <- as.numeric(sub("student_t\\([0-9.]+,-?[0-9.]+,([0-9.]+)\\)", "\\1", results_dgp_1$priors$prior[13]))
  expect_lt(abs(scale - residual_sd), 0.0025)


  #Compare LME model
  lme_model_1 <- lme4::lmer(active_return ~ 0 + theme + theme:market_factor_proxy + (1 + market_factor_proxy | theme:tickers),
                            data = simulated_data, control = lme4::lmerControl(optimizer = "Nelder_Mead"))

  expect_equal(coef(results_dgp_1$model), coef(lme_model_1))
  expect_equal(lme4::fixef(results_dgp_1$model), lme4::fixef(lme_model_1))
  expect_equal(lme4::ranef(results_dgp_1$model), lme4::ranef(lme_model_1))

})

test_that("derive_informative_priors_from_data works for model spec 4", {

  #DGP 1
  ##############################
  # Adjusted DGP for lme4::lmer(active_return ~ market_factor_proxy + (1 + market_factor_proxy | theme:tickers))
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

  # Global fixed effects (intercept and slope)
  fixed_intercept_mean <- 0.01   # Global fixed intercept
  fixed_intercept_sd <- 0.002    # Variability in global fixed intercept
  fixed_slope_mean <- 0.002      # Global fixed slope for market_factor_proxy
  fixed_slope_sd <- 0.001        # Variability in global fixed slope

  # Random effects for theme:tickers
  random_intercept_tickers_sd <- 0.01    # SD for random intercepts
  random_slope_tickers_sd <- 0.003      # SD for random slopes
  correlation <- 0.2                    # Correlation between random intercept and slope
  residual_sd <- 0.045                  # Residual noise SD

  # Covariance matrix for random effects
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
  active_return <- numeric(n_obs_per_ticker * n_tickers)

  # Predictor: market_factor_proxy
  market_factor_proxy <- rnorm(n_obs_per_ticker * n_tickers, mean = 0, sd = 1)

  # Generate monthly dates for the observations
  dates <- rep(
    seq.Date(as.Date("1980-01-01"), by = "month", length.out = n_obs_per_ticker),
    times = n_tickers
  )

  # Loop to calculate active_return for each observation
  for (i in seq_along(active_return)) {
    ticker_idx <- ((i - 1) %/% n_obs_per_ticker) + 1  # Identify signal index

    # Combine fixed effects, random effects, and residual noise
    active_return[i] <- rnorm(1, mean = fixed_intercept_mean, sd = fixed_intercept_sd) +           # Global fixed intercept
      rnorm(1, mean = fixed_slope_mean, sd = fixed_slope_sd) * market_factor_proxy[i] +           # Global fixed slope
      random_intercepts_tickers[ticker_idx] +                                                    # Random intercept for theme:tickers
      random_slopes_tickers[ticker_idx] * market_factor_proxy[i] +                               # Random slope for theme:tickers
      rnorm(1, mean = 0, sd = residual_sd)                                                       # Residual noise
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
    active_return = active_return,          # Response variable
    market_factor_proxy = market_factor_proxy  # Predictor variable
  )

  # Reorder columns as requested
  simulated_data <- simulated_data[, c("id", "tickers", "dates", "active_return", "market_factor_proxy", "theme")]


  ##############################

  #Get priors for dgp 1
  results_dgp_1 <- derive_informative_priors_from_data(priors_m_upd_ref = simulated_data,
                                                       model_spec_theme_level = "none",
                                                       half_t_distribution = 30, lmer_optimizer = "Nelder_Mead")

  expect_s3_class(results_dgp_1$priors, "brmsprior")
  expect_equal(results_dgp_1$priors$class, c("Intercept", "b", "sd", "sd", "sigma", "cor"))

  #Check if estimates are somewhat as expected
  expect_equal(as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[1])),
               fixed_intercept_mean, tolerance = 0.01)

  expect_equal(as.numeric(sub("normal\\((-?[0-9.]+),.*", "\\1", results_dgp_1$priors$prior[2])),
               fixed_slope_mean, tolerance = 0.01)

  #Random Intercept Tickers
  # Extract degrees of freedom
  degrees_of_freedom <- as.numeric(sub("student_t\\(([0-9.]+),.*", "\\1", results_dgp_1$priors$prior[3]))
  expect_equal(degrees_of_freedom, 30)

  # Extract scale
  scale <- as.numeric(sub("student_t\\([0-9.]+,-?[0-9.]+,([0-9.]+)\\)", "\\1", results_dgp_1$priors$prior[3]))
  expect_lt(abs(scale - random_intercept_tickers_sd), 0.0025)


  # Residual SD
  degrees_of_freedom <- as.numeric(sub("student_t\\(([0-9.]+),.*", "\\1", results_dgp_1$priors$prior[4]))
  expect_equal(degrees_of_freedom, 30)

  scale <- as.numeric(sub("student_t\\([0-9.]+,-?[0-9.]+,([0-9.]+)\\)", "\\1", results_dgp_1$priors$prior[5]))
  expect_lt(abs(scale - residual_sd), 0.0025)


  #Compare LME model
  lme_model_1 <- lme4::lmer(active_return ~ market_factor_proxy + (1 + market_factor_proxy | theme:tickers),
                            data = simulated_data, control = lme4::lmerControl(optimizer = "Nelder_Mead"))

  expect_equal(coef(results_dgp_1$model), coef(lme_model_1))
  expect_equal(lme4::fixef(results_dgp_1$model), lme4::fixef(lme_model_1))
  expect_equal(lme4::ranef(results_dgp_1$model), lme4::ranef(lme_model_1))

})
