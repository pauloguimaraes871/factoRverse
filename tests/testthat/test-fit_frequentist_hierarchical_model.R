test_that("fit_frequentist_hierarchical_model works for random_intercept_fixed_slope", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"
  data_availability_cutoff <- 3

  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions, signal_themes_m_df = signal_themes_m_df,
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

  #Hierarchical Data
  selected_backtest_returns_corrected_positions_m_upd_ref <- data.frame(
    id = paste0(rep(c("Alpha", "Gamma", "low_Beta"), 4), "-", rep(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))),
    tickers = rep(c("Alpha", "Gamma", "low_Beta"),  4),
    dates =  rep(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"), each = 3),
    return = c(selected_backtest_returns_corrected_positions_xts_upd_ref$Alpha, selected_backtest_returns_corrected_positions_xts_upd_ref$Gamma,
               selected_backtest_returns_corrected_positions_xts_upd_ref$low_Beta),
    market_factor_proxy = rep(selected_market_factor_proxy_xts_upd_ref, each = 3),
    theme = rep(c("value", "value", "momentum"), 4),
    row.names = NULL
  )
  colnames(selected_backtest_returns_corrected_positions_m_upd_ref) <- c("id", "tickers", "dates", "return", "market_factor_proxy", "theme")

  lmer_model <- suppressWarnings(lmerTest::lmer(formula =" return ~ market_factor_proxy + (1 | theme) + (1 + market_factor_proxy | theme:tickers)",
                               data = selected_backtest_returns_corrected_positions_m_upd_ref,
                               REML = FALSE
                               ))
  coefs <- summary(lmer_model)$coefficients
  re <- lme4::ranef(lmer_model)

  pooled_capm_m_d_ref <- base_signal_universe_m_d_ref[, c("id", "tickers", "dates")]
  pooled_capm_m_d_ref$theme_alpha <- coefs["(Intercept)", "Estimate"]
  pooled_capm_m_d_ref$individual_alpha <-   pooled_capm_m_d_ref$theme_alpha + re$`theme:tickers`[c(2,3,1), "(Intercept)"]
  pooled_capm_m_d_ref$alpha_se <- coefs["(Intercept)", "Std. Error"]
  pooled_capm_m_d_ref$theme_beta <- coefs["market_factor_proxy", "Estimate"]
  pooled_capm_m_d_ref$individual_beta <-   pooled_capm_m_d_ref$theme_beta + re$`theme:tickers`[c(2,3,1), "market_factor_proxy"]
  pooled_capm_m_d_ref$specific_risk <- sigma(lmer_model)
  pooled_capm_m_d_ref$alpha_t_stat <- coefs["(Intercept)", "t value"]
  pooled_capm_m_d_ref$treynor_ratio <-
    as.numeric(PerformanceAnalytics::mean.geometric(selected_backtest_returns_corrected_positions_xts_upd_ref/100))*100/pooled_capm_m_d_ref$individual_beta
  pooled_capm_m_d_ref$appraisal_ratio <- pooled_capm_m_d_ref$individual_alpha/pooled_capm_m_d_ref$specific_risk
  pooled_capm_m_d_ref$p_value <- coefs["(Intercept)", "Pr(>|t|)"]/2

  #result
  result <- suppressWarnings(fit_frequentist_hierarchical_model(
    signal_universe_m_d_ref = base_signal_universe_m_d_ref,
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_spec_theme_level = "random_intercept_fixed_slope",
    lmer_optimizer = "nloptwrap", lmer_optimization_objective = FALSE, hierarchical_p_value_method = "Satterthwaite"
 ))

  expect_equal(tibble::tibble(result$pooled_CAPM_metrics_m_d_ref),
               tibble::tibble(pooled_capm_m_d_ref[c(1,3,2),]))

})

test_that("fit_frequentist_hierarchical_model works for theme_specific_intercept_fixed_slope", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"
  data_availability_cutoff <- 3

  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions, signal_themes_m_df = signal_themes_m_df,
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

  #Hierarchical Data
  selected_backtest_returns_corrected_positions_m_upd_ref <- data.frame(
    id = paste0(rep(c("Alpha", "Gamma", "low_Beta"), 4), "-", rep(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))),
    tickers = rep(c("Alpha", "Gamma", "low_Beta"),  4),
    dates =  rep(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"), each = 3),
    return = c(selected_backtest_returns_corrected_positions_xts_upd_ref$Alpha, selected_backtest_returns_corrected_positions_xts_upd_ref$Gamma,
               selected_backtest_returns_corrected_positions_xts_upd_ref$low_Beta),
    market_factor_proxy = rep(selected_market_factor_proxy_xts_upd_ref, each = 3),
    theme = rep(c("value", "value", "momentum"), 4),
    row.names = NULL
  )
  colnames(selected_backtest_returns_corrected_positions_m_upd_ref) <- c("id", "tickers", "dates", "return", "market_factor_proxy", "theme")

  lmer_model <- suppressWarnings(lmerTest::lmer(formula ="return ~ 0 + theme + market_factor_proxy + (1 + market_factor_proxy | theme:tickers)",
                               data = selected_backtest_returns_corrected_positions_m_upd_ref,
                               REML = TRUE)
  )
  coefs <- summary(lmer_model, ddf = "Kenward-Roger")$coefficients
  re <- lme4::ranef(lmer_model)

  pooled_capm_m_d_ref <- base_signal_universe_m_d_ref[, c("id", "tickers", "dates")]
  pooled_capm_m_d_ref$theme_alpha <- coefs[c(2,2,1), "Estimate"]
  pooled_capm_m_d_ref$individual_alpha <-   pooled_capm_m_d_ref$theme_alpha + re$`theme:tickers`[c(2,3,1), "(Intercept)"]
  pooled_capm_m_d_ref$alpha_se <- coefs[c(2,2,1), "Std. Error"]
  pooled_capm_m_d_ref$theme_beta <- coefs["market_factor_proxy", "Estimate"]
  pooled_capm_m_d_ref$individual_beta <-   pooled_capm_m_d_ref$theme_beta + re$`theme:tickers`[c(2,3,1), "market_factor_proxy"]
  pooled_capm_m_d_ref$specific_risk <- sigma(lmer_model)
  pooled_capm_m_d_ref$alpha_t_stat <- coefs[c(2,2,1), "t value"]
  pooled_capm_m_d_ref$treynor_ratio <-
    as.numeric(PerformanceAnalytics::mean.geometric(selected_backtest_returns_corrected_positions_xts_upd_ref/100))*100/pooled_capm_m_d_ref$individual_beta
  pooled_capm_m_d_ref$appraisal_ratio <- pooled_capm_m_d_ref$individual_alpha/pooled_capm_m_d_ref$specific_risk
  pooled_capm_m_d_ref$p_value <- coefs[c(2,2,1), "Pr(>|t|)"]/2

  #result
  result <- suppressWarnings(fit_frequentist_hierarchical_model(
    signal_universe_m_d_ref = base_signal_universe_m_d_ref,
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_spec_theme_level = "theme_specific_intercept_fixed_slope",
    lmer_optimizer = "nloptwrap", lmer_optimization_objective = TRUE, hierarchical_p_value_method = "Kenward-Roger"
  ))

  expect_equal(tibble::tibble(result$pooled_CAPM_metrics_m_d_ref),
               tibble::tibble(pooled_capm_m_d_ref[c(1,3,2),]))

})

test_that("fit_frequentist_hierarchical_model works for theme_specific_intercept_theme_specific_slope", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"
  data_availability_cutoff <- 3

  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions, signal_themes_m_df = signal_themes_m_df,
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

  #Hierarchical Data
  selected_backtest_returns_corrected_positions_m_upd_ref <- data.frame(
    id = paste0(rep(c("Alpha", "Gamma", "low_Beta"), 4), "-", rep(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))),
    tickers = rep(c("Alpha", "Gamma", "low_Beta"),  4),
    dates =  rep(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"), each = 3),
    return = c(selected_backtest_returns_corrected_positions_xts_upd_ref$Alpha, selected_backtest_returns_corrected_positions_xts_upd_ref$Gamma,
               selected_backtest_returns_corrected_positions_xts_upd_ref$low_Beta),
    market_factor_proxy = rep(selected_market_factor_proxy_xts_upd_ref, each = 3),
    theme = rep(c("value", "value", "momentum"), 4),
    row.names = NULL
  )
  colnames(selected_backtest_returns_corrected_positions_m_upd_ref) <- c("id", "tickers", "dates", "return", "market_factor_proxy", "theme")

  lmer_model <- suppressWarnings(lmerTest::lmer(formula = "return ~ 0 + theme + theme:market_factor_proxy + (1 + market_factor_proxy | theme:tickers)",
                               data = selected_backtest_returns_corrected_positions_m_upd_ref,
                               REML = TRUE)
  )
  coefs <- summary(lmer_model, ddf = "Kenward-Roger")$coefficients
  re <- lme4::ranef(lmer_model)

  pooled_capm_m_d_ref <- base_signal_universe_m_d_ref[, c("id", "tickers", "dates")]
  pooled_capm_m_d_ref$theme_alpha <- coefs[c(2,2,1), "Estimate"]
  pooled_capm_m_d_ref$individual_alpha <-   pooled_capm_m_d_ref$theme_alpha + re$`theme:tickers`[c(2,3,1), "(Intercept)"]
  pooled_capm_m_d_ref$alpha_se <- coefs[c(2,2,1), "Std. Error"]
  pooled_capm_m_d_ref$theme_beta <- coefs[c(4,4,3), "Estimate"]
  pooled_capm_m_d_ref$individual_beta <-   pooled_capm_m_d_ref$theme_beta + re$`theme:tickers`[c(2,3,1), "market_factor_proxy"]
  pooled_capm_m_d_ref$specific_risk <- sigma(lmer_model)
  pooled_capm_m_d_ref$alpha_t_stat <- coefs[c(2,2,1), "t value"]
  pooled_capm_m_d_ref$treynor_ratio <-
    as.numeric(PerformanceAnalytics::mean.geometric(selected_backtest_returns_corrected_positions_xts_upd_ref/100))*100/pooled_capm_m_d_ref$individual_beta
  pooled_capm_m_d_ref$appraisal_ratio <- pooled_capm_m_d_ref$individual_alpha/pooled_capm_m_d_ref$specific_risk
  pooled_capm_m_d_ref$p_value <- coefs[c(2,2,1), "Pr(>|t|)"]/2

  #result
  result <- suppressWarnings(fit_frequentist_hierarchical_model(
    signal_universe_m_d_ref = base_signal_universe_m_d_ref,
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_spec_theme_level = "theme_specific_intercept_theme_specific_slope",
    lmer_optimizer = "nloptwrap", lmer_optimization_objective = TRUE, hierarchical_p_value_method = "Kenward-Roger"
  ))

  expect_equal(tibble::tibble(result$pooled_CAPM_metrics_m_d_ref),
               tibble::tibble(pooled_capm_m_d_ref[c(1,3,2),]))

})

test_that("fit_frequentist_hierarchical_model works for fixed_intercept_fixed_slope", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #Get arguments
  chosen_signals_and_positions <- c(Alpha = "long", Gamma = "long", Beta = "short")
  signal_significance_threshold <- 0.05
  p_correction_method <- "none"
  data_availability_cutoff <- 3

  #Select signals based on user choice
  selected_signals_and_backtest_list <- select_and_correct_signals(
    chosen_signals_and_positions = chosen_signals_and_positions, signal_themes_m_df = signal_themes_m_df,
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

  #Hierarchical Data
  selected_backtest_returns_corrected_positions_m_upd_ref <- data.frame(
    id = paste0(rep(c("Alpha", "Gamma", "low_Beta"), 4), "-", rep(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"))),
    tickers = rep(c("Alpha", "Gamma", "low_Beta"),  4),
    dates =  rep(c("2001-03-15", "2001-04-15", "2001-05-15", "2001-06-15"), each = 3),
    return = c(selected_backtest_returns_corrected_positions_xts_upd_ref$Alpha, selected_backtest_returns_corrected_positions_xts_upd_ref$Gamma,
               selected_backtest_returns_corrected_positions_xts_upd_ref$low_Beta),
    market_factor_proxy = rep(selected_market_factor_proxy_xts_upd_ref, each = 3),
    theme = rep(c("value", "value", "momentum"), 4),
    row.names = NULL
  )
  colnames(selected_backtest_returns_corrected_positions_m_upd_ref) <- c("id", "tickers", "dates", "return", "market_factor_proxy", "theme")

  lmer_model <- suppressWarnings(lmerTest::lmer(formula = "return ~ market_factor_proxy + (1 + market_factor_proxy | theme:tickers)",
                               data = selected_backtest_returns_corrected_positions_m_upd_ref,
                               REML = TRUE, lme4::lmerControl(optimizer = "Nelder_Mead"))
  )
  coefs <- summary(lmer_model, ddf = "Kenward-Roger")$coefficients
  re <- lme4::ranef(lmer_model)

  pooled_capm_m_d_ref <- base_signal_universe_m_d_ref[, c("id", "tickers", "dates")]
  pooled_capm_m_d_ref$theme_alpha <- coefs[c(1,1,1), "Estimate"]
  pooled_capm_m_d_ref$individual_alpha <-   pooled_capm_m_d_ref$theme_alpha + re$`theme:tickers`[c(2,3,1), "(Intercept)"]
  pooled_capm_m_d_ref$alpha_se <- coefs[c(1,1,1), "Std. Error"]
  pooled_capm_m_d_ref$theme_beta <- coefs[c(2,2,2), "Estimate"]
  pooled_capm_m_d_ref$individual_beta <-   pooled_capm_m_d_ref$theme_beta + re$`theme:tickers`[c(2,3,1), "market_factor_proxy"]
  pooled_capm_m_d_ref$specific_risk <- sigma(lmer_model)
  pooled_capm_m_d_ref$alpha_t_stat <- coefs[c(1,1,1), "t value"]
  pooled_capm_m_d_ref$treynor_ratio <-
    as.numeric(PerformanceAnalytics::mean.geometric(selected_backtest_returns_corrected_positions_xts_upd_ref/100))*100/pooled_capm_m_d_ref$individual_beta
  pooled_capm_m_d_ref$appraisal_ratio <- pooled_capm_m_d_ref$individual_alpha/pooled_capm_m_d_ref$specific_risk
  pooled_capm_m_d_ref$p_value <- coefs[c(1,1,1), "Pr(>|t|)"]/2

  #result
  result <- suppressWarnings(fit_frequentist_hierarchical_model(
    signal_universe_m_d_ref = base_signal_universe_m_d_ref,
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_spec_theme_level = "fixed_intercept_fixed_slope",
    lmer_optimizer = "Nelder_Mead", lmer_optimization_objective = TRUE, hierarchical_p_value_method = "Kenward-Roger"
  ))

  expect_equal(tibble::tibble(result$pooled_CAPM_metrics_m_d_ref),
               tibble::tibble(pooled_capm_m_d_ref[c(1,3,2),]))

})
