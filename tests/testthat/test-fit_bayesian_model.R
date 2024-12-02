test_that("fit_bayesian_model adequately fits a bayesian hierarchical model for model spec 1", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #get selected info
  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    chosen_signals = signal_selection_policy$chosen_signals,
    signal_positions = signal_selection_policy$signal_positions,
    backtest_returns_df = backtest_returns_df
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_df <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_df
  selected_market_factor_proxy_df <- benchmark_returns_df[, c("dates", "IBOV")]

  #current info
  current_date <- "2001-07-15"
  selected_backtest_returns_corrected_positions_upd_ref <-
    selected_backtest_returns_corrected_positions_df[which(selected_backtest_returns_corrected_positions_df$dates <= current_date), ]

  selected_market_factor_proxy_vector_upd_ref <-
    selected_market_factor_proxy_df[which(selected_market_factor_proxy_df$dates <= current_date), "IBOV"]

  signal_themes_m_d_ref <- signal_themes_m_df[which(signal_themes_m_df$dates == current_date), ]

  priors_m_upd_ref <- priors_m_df[priors_m_df$dates <= current_date,]

  #expected results
  expected_result <- data.frame(id = paste0(colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1],"-",current_date),
                                tickers = colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1], dates = current_date)
  expected_result$dates <- as.Date(expected_result$dates, format = "%Y-%m-%d")
  expected_result$mean_active_return <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) mean(x))
  expected_result$tracking_error <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) sd(x))
  expected_result$IR <- expected_result$mean_active_return/expected_result$tracking_error

  lm_model_summary_list <- purrr::map(lapply(selected_backtest_returns_corrected_positions_upd_ref[,-1], as.vector),
                                      ~ summary(lm(.x ~ selected_market_factor_proxy_vector_upd_ref)))

  expected_result$alpha <- sapply(lm_model_summary_list, function(x) x$coefficients[1])
  expected_result$alpha_t_stat <- sapply(lm_model_summary_list, function(x) x$coefficients[5])
  expected_result$beta <- sapply(lm_model_summary_list, function(x) x$coefficients[2])
  expected_result$treynor <- expected_result$mean_active_return/expected_result$beta
  expected_result$p_value <- sapply(lm_model_summary_list, function(x) x$coefficients[7])

  #Inside Bayesian Adjustment

  #Create priors for model
  elected_priors <- c(
    # Prior for Intercept
    brms::set_prior("normal(0.0012, 0.0016)", class = "Intercept"),

    # Prior for market_factor_proxy coefficient
    brms::set_prior("normal(0.0003, 0.0003)", class = "b", coef = "market_factor_proxy"),

    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"),

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"),

    # Prior for sd of Intercept at theme level
    brms::set_prior("student_t(30, 0, 0.0011)", class = "sd", group = "theme", coef = "Intercept"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )

  future::plan("multisession")
  set.seed(123)
  results <- fit_bayesian_model(selected_backtest_returns_corrected_positions_upd_ref = selected_backtest_returns_corrected_positions_upd_ref,
                                selected_market_factor_proxy_vector_upd_ref = selected_market_factor_proxy_vector_upd_ref,
                                signal_universe_m_d_ref = expected_result,
                                signal_themes_m_d_ref = signal_themes_m_d_ref,
                                elected_priors = elected_priors,
                                model_spec_theme_level = signal_selection_policy$model_spec_theme_level,
                                parallel = TRUE,
                                chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = NA, adapt_delta = 0.99, #MCMC parameters
                                verbose = TRUE
  )

  brm_model <- results$bayesian_model

  #check if brm model was fit correctly
  expect_equal(class(brm_model), "brmsfit")
  expect_equal(brm_model$family$family, "gaussian")

  expect_true(all(brm_model$basis$levels$theme %in% unique(signal_themes_m_d_ref$theme)))
  expect_true(all(brm_model$basis$levels$`theme:tickers` %in% paste0(unique(signal_themes_m_df$theme), "_" ,unique(signal_themes_m_df$tickers))))

  expect_equal(as.character(brm_model$formula$formula)[c(2,1,3)], c("active_return", "~", "market_factor_proxy + (1 | theme) + (1 + market_factor_proxy | theme:tickers)"))

  #Construct data
  selected_backtest_returns_corrected_positions_upd_ref$market_factor_proxy <- selected_market_factor_proxy_vector_upd_ref
  selected_backtest_returns_corrected_positions_upd_ref_long <- reshape2::melt(selected_backtest_returns_corrected_positions_upd_ref,
                                                                               id.vars = c("dates", "market_factor_proxy"),
                                                                               variable.name = "tickers", value.name = "active_return")

  selected_backtest_returns_corrected_positions_upd_ref_long <-
    selected_backtest_returns_corrected_positions_upd_ref_long[order(selected_backtest_returns_corrected_positions_upd_ref_long$dates),]
  selected_backtest_returns_corrected_positions_upd_ref_long <- dplyr::left_join(selected_backtest_returns_corrected_positions_upd_ref_long,
                                                                                 signal_themes_m_d_ref %>% dplyr::select(tickers, theme), by = "tickers")
  selected_backtest_returns_corrected_positions_upd_ref_long$`theme:tickers` <- paste0(selected_backtest_returns_corrected_positions_upd_ref_long$theme, "_", selected_backtest_returns_corrected_positions_upd_ref_long$tickers)

  selected_backtest_returns_corrected_positions_upd_ref_long <-
    selected_backtest_returns_corrected_positions_upd_ref_long[, c("active_return", "market_factor_proxy", "theme", "tickers", "theme:tickers")]

  # Copy the data frame
  data_only <- brm_model$data
  # Remove unwanted attributes
  attr(data_only, "terms") <- NULL
  attr(data_only, "drop_unused_levels") <- NULL
  attr(data_only, "data_name") <- NULL
  expect_equal(data_only, selected_backtest_returns_corrected_positions_upd_ref_long)

  # Extract priors set by the user in brm_model
  user_priors_in_model <- subset(brm_model$prior, source == "user")

  # Select relevant columns for comparison
  cols_to_compare <- c("prior", "class", "coef", "group", "source")

  # Sort the data frames
  user_priors_in_model_sorted <- user_priors_in_model[, cols_to_compare]

  user_priors_in_model_sorted$class[3] <- "cor"
  user_priors_in_model_sorted$prior[3] <- "lkj(2)"
  user_priors_in_model_sorted <- user_priors_in_model_sorted[c(2,1,5,6,4,7,3),]
  rownames(user_priors_in_model_sorted) <- NULL


  elected_priors_sorted <- elected_priors[, cols_to_compare]

  rownames(elected_priors_sorted) <- NULL

  expect_equal(user_priors_in_model_sorted, elected_priors_sorted)

  #Check if MCMC parameters are right
  expect_equal(brm_model$stan_args$control$adapt_delta, 0.99)

  #Check number of rows in predicted_summary
  n_draws <- nrow(results$posterior_draws_summaries$predicted_summary) %>% as.numeric()
  expected_draws <- length(selected_market_factor_proxy_vector_upd_ref)*(ncol(selected_backtest_returns_corrected_positions_upd_ref) - 2)
  expect_true(n_draws == expected_draws)

  #Check number of rows in posterior_draws
  expect_equal((ncol(results$posterior_draws_summaries$intercept_summary) - 1)/3, 2) #Posterior theme and individual
  expect_equal((ncol(results$posterior_draws_summaries$slope_summary) - 1)/3, 2) #Posterior theme and individual
  expect_equal(ncol(results$posterior_draws_summaries$sd_summary)/3, 5) #Posterior theme and individual

  #Check tidydraws
  expected_results <- insight::get_parameters(brm_model, effects = "all")

  #Check theme alpha and beta
  #theme alpha
  expect_equal(c(
    median(expected_results$b_Intercept + expected_results$`r_theme[momentum,Intercept]`),
    median(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]`),
    median(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]`)),
    results$posterior_draws_summaries$intercept_summary$posterior_theme_alpha)


  expect_equal(c(
    median(expected_results$b_Intercept + expected_results$`r_theme[momentum,Intercept]`),
    median(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]`),
    median(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]`)),
    results$signal_universe_m_d_ref$posterior_theme_alpha[c(2,1,3)])


  #pd theme alpha
  expect_equal(c(
    mean(expected_results$b_Intercept + expected_results$`r_theme[momentum,Intercept]` > 0),
    mean(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]` > 0),
    mean(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]` > 0)),
    results$signal_universe_m_d_ref$pd_theme_alpha[c(2,1,3)])


  #indi alpha
  expect_equal(c(
    median(expected_results$b_Intercept + expected_results$`r_theme[momentum,Intercept]` + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`),
    median(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]` + expected_results$`r_theme:tickers[value_Alpha,Intercept]`),
    median(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]` + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)),
    results$posterior_draws_summaries$intercept_summary$posterior_individual_alpha)


  expect_equal(c(
    median(expected_results$b_Intercept + expected_results$`r_theme[momentum,Intercept]` + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`),
    median(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]` + expected_results$`r_theme:tickers[value_Alpha,Intercept]`),
    median(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]` + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)),
    results$signal_universe_m_d_ref$posterior_individual_alpha[c(2,1,3)])

  #pd alpha
  expect_equal(c(
    mean(expected_results$b_Intercept + expected_results$`r_theme[momentum,Intercept]` + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]` > 0),
    mean(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]` + expected_results$`r_theme:tickers[value_Alpha,Intercept]` > 0),
    mean(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]` + expected_results$`r_theme:tickers[value_Gamma,Intercept]` > 0)),
    results$signal_universe_m_d_ref$pd_alpha[c(2,1,3)])

  #alpha t stat




  expect_equal(c(mean(expected_results$b_Intercept + expected_results$`r_theme[momentum,Intercept]` + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`)/
                   sd(expected_results$b_Intercept + expected_results$`r_theme[momentum,Intercept]` + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`),

                 mean(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]` + expected_results$`r_theme:tickers[value_Alpha,Intercept]`)/
                   sd(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]` + expected_results$`r_theme:tickers[value_Alpha,Intercept]`),

                 mean(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]` + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)/
                   sd(expected_results$b_Intercept + expected_results$`r_theme[value,Intercept]` + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)
  ),
  results$signal_universe_m_d_ref$posterior_alpha_t_stat[c(2,1,3)]
  )


  #Check theme beta
  expect_equal(rep(median(expected_results$b_market_factor_proxy), 3), results$posterior_draws_summaries$slope_summary$posterior_theme_beta)

  expect_equal(rep(median(expected_results$b_market_factor_proxy), 3),
               results$signal_universe_m_d_ref$posterior_theme_beta)

  #Check ind beta
  expect_equal(c(median(expected_results$b_market_factor_proxy + expected_results$`r_theme:tickers[momentum_low_Beta,market_factor_proxy]`),
                 median(expected_results$b_market_factor_proxy + expected_results$`r_theme:tickers[value_Alpha,market_factor_proxy]`),
                 median(expected_results$b_market_factor_proxy + expected_results$`r_theme:tickers[value_Gamma,market_factor_proxy]`)),
               results$signal_universe_m_d_ref$posterior_individual_beta[c(2,1,3)])


  #Check sd
  expect_equal(c(median(expected_results$sd_theme__Intercept),
                 median(expected_results$`sd_theme:tickers__Intercept`), median(expected_results$`sd_theme:tickers__market_factor_proxy`),
                 median(expected_results$sigma), median(expected_results$`cor_theme:tickers__Intercept__market_factor_proxy`)),
               unname(unlist(results$posterior_draws_summaries$sd_summary[,c(1,4,7,10,13)]))
  )
  #Check posterior_predict
  expect_equal(brms::posterior_epred(brm_model) %>% apply(2, function(x) median(x)),
               results$posterior_draws_summaries$epred_summary$.epred[order(results$posterior_draws_summaries$epred_summary$.row)])

})

test_that("fit_bayesian_model adequately fits a bayesian hierarchical model for model spec 2", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #get selected info
  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    chosen_signals = signal_selection_policy$chosen_signals,
    signal_positions = signal_selection_policy$signal_positions,
    backtest_returns_df = backtest_returns_df
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_df <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_df
  selected_market_factor_proxy_df <- benchmark_returns_df[, c("dates", "IBOV")]

  #current info
  current_date <- "2001-07-15"
  selected_backtest_returns_corrected_positions_upd_ref <-
    selected_backtest_returns_corrected_positions_df[which(selected_backtest_returns_corrected_positions_df$dates <= current_date), ]

  selected_market_factor_proxy_vector_upd_ref <-
    selected_market_factor_proxy_df[which(selected_market_factor_proxy_df$dates <= current_date), "IBOV"]

  signal_themes_m_d_ref <- signal_themes_m_df[which(signal_themes_m_df$dates == current_date), ]

  priors_m_upd_ref <- priors_m_df[priors_m_df$dates <= current_date,]

  #expected results
  expected_result <- data.frame(id = paste0(colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1],"-",current_date),
                                tickers = colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1], dates = current_date)
  expected_result$dates <- as.Date(expected_result$dates, format = "%Y-%m-%d")
  expected_result$mean_active_return <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) mean(x))
  expected_result$tracking_error <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) sd(x))
  expected_result$IR <- expected_result$mean_active_return/expected_result$tracking_error

  lm_model_summary_list <- purrr::map(lapply(selected_backtest_returns_corrected_positions_upd_ref[,-1], as.vector),
                                      ~ summary(lm(.x ~ selected_market_factor_proxy_vector_upd_ref)))

  expected_result$alpha <- sapply(lm_model_summary_list, function(x) x$coefficients[1])
  expected_result$alpha_t_stat <- sapply(lm_model_summary_list, function(x) x$coefficients[5])
  expected_result$beta <- sapply(lm_model_summary_list, function(x) x$coefficients[2])
  expected_result$treynor <- expected_result$mean_active_return/expected_result$beta
  expected_result$p_value <- sapply(lm_model_summary_list, function(x) x$coefficients[7])

  #Inside Bayesian Adjustment

  #Create priors for model
  elected_priors <- c(
    # Prior for Value and Mom
    brms::set_prior("normal(0.0012, 0.0016)", class = "b", coef = "themevalue"),
    brms::set_prior("normal(0.0025, 0.0016)", class = "b", coef = "thememomentum"),

    # Prior for market_factor_proxy coefficient
    brms::set_prior("normal(0.0003, 0.0003)", class = "b", coef = "market_factor_proxy"),

    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"),

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )

  future::plan("multisession")
  set.seed(123)
  results <- fit_bayesian_model(selected_backtest_returns_corrected_positions_upd_ref = selected_backtest_returns_corrected_positions_upd_ref,
                                selected_market_factor_proxy_vector_upd_ref = selected_market_factor_proxy_vector_upd_ref,
                                signal_universe_m_d_ref = expected_result,
                                signal_themes_m_d_ref = signal_themes_m_d_ref,
                                elected_priors = elected_priors,
                                model_spec_theme_level = "fixed_intercepts",
                                parallel = TRUE,
                                chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = NA, adapt_delta = 0.99, #MCMC parameters
                                verbose = TRUE
  )

  brm_model <- results$bayesian_model

  #check if brm model was fit correctly
  expect_equal(class(brm_model), "brmsfit")
  expect_equal(brm_model$family$family, "gaussian")

  expect_true(all(brm_model$basis$levels$`theme:tickers` %in% paste0(unique(signal_themes_m_df$theme), "_" ,unique(signal_themes_m_df$tickers))))

  expect_equal(as.character(brm_model$formula$formula)[c(2,1,3)], c("active_return", "~", "0 + theme + market_factor_proxy + (1 + market_factor_proxy | theme:tickers)"))

  #Construct data
  selected_backtest_returns_corrected_positions_upd_ref$market_factor_proxy <- selected_market_factor_proxy_vector_upd_ref
  selected_backtest_returns_corrected_positions_upd_ref_long <- reshape2::melt(selected_backtest_returns_corrected_positions_upd_ref,
                                                                               id.vars = c("dates", "market_factor_proxy"),
                                                                               variable.name = "tickers", value.name = "active_return")

  selected_backtest_returns_corrected_positions_upd_ref_long <-
    selected_backtest_returns_corrected_positions_upd_ref_long[order(selected_backtest_returns_corrected_positions_upd_ref_long$dates),]
  selected_backtest_returns_corrected_positions_upd_ref_long <- dplyr::left_join(selected_backtest_returns_corrected_positions_upd_ref_long,
                                                                                 signal_themes_m_d_ref %>% dplyr::select(tickers, theme), by = "tickers")
  selected_backtest_returns_corrected_positions_upd_ref_long$`theme:tickers` <- paste0(selected_backtest_returns_corrected_positions_upd_ref_long$theme, "_", selected_backtest_returns_corrected_positions_upd_ref_long$tickers)

  selected_backtest_returns_corrected_positions_upd_ref_long <-
    selected_backtest_returns_corrected_positions_upd_ref_long[, c("active_return", "theme",  "market_factor_proxy", "tickers", "theme:tickers")]

  # Copy the data frame
  data_only <- brm_model$data
  # Remove unwanted attributes
  attr(data_only, "terms") <- NULL
  attr(data_only, "drop_unused_levels") <- NULL
  attr(data_only, "data_name") <- NULL
  expect_equal(data_only, selected_backtest_returns_corrected_positions_upd_ref_long)

  # Extract priors set by the user in brm_model
  user_priors_in_model <- subset(brm_model$prior, source == "user")

  # Select relevant columns for comparison
  cols_to_compare <- c("prior", "class", "coef", "group", "source")

  # Sort the data frames
  user_priors_in_model_sorted <- user_priors_in_model[, cols_to_compare]

  user_priors_in_model_sorted$class[4] <- "cor"
  user_priors_in_model_sorted$prior[4] <- "lkj(2)"
  user_priors_in_model_sorted <- user_priors_in_model_sorted[c(3,2,1,5,6,7,4),]
  rownames(user_priors_in_model_sorted) <- NULL


  elected_priors_sorted <- elected_priors[, cols_to_compare]

  rownames(elected_priors_sorted) <- NULL

  expect_equal(user_priors_in_model_sorted, elected_priors_sorted)

  #Check if MCMC parameters are right
  expect_equal(brm_model$stan_args$control$adapt_delta, 0.99)

  #Check number of rows in predicted_summary
  n_draws <- nrow(results$posterior_draws_summaries$predicted_summary) %>% as.numeric()
  expected_draws <- length(selected_market_factor_proxy_vector_upd_ref)*(ncol(selected_backtest_returns_corrected_positions_upd_ref) - 2)
  expect_true(n_draws == expected_draws)

  #Check number of rows in posterior_draws
  expect_equal((ncol(results$posterior_draws_summaries$intercept_summary) - 1)/3, 2) #Posterior theme and individual
  expect_equal((ncol(results$posterior_draws_summaries$slope_summary) - 1)/3, 2) #Posterior theme and individual
  expect_equal(ncol(results$posterior_draws_summaries$sd_summary)/3, 4) #Posterior theme and individual

  #Check tidydraws
  expected_results <- insight::get_parameters(brm_model, effects = "all")

  #Check theme alpha and beta
  #theme alpha
  expect_equal(c(median(expected_results$b_thememomentum), rep(median(expected_results$b_themevalue), 2)),
  results$posterior_draws_summaries$intercept_summary$posterior_theme_alpha)


  expect_equal(c(median(expected_results$b_thememomentum), rep(median(expected_results$b_themevalue), 2)),
               results$signal_universe_m_d_ref$posterior_theme_alpha[c(2,1,3)])

  #pd theme alpha
  expect_equal(c(mean(expected_results$b_thememomentum > 0), rep(mean(expected_results$b_themevalue > 0), 2)),
               results$signal_universe_m_d_ref$pd_theme_alpha[c(2,1,3)])

  #indi alpha
  expect_equal(c(median(expected_results$b_thememomentum + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`),
                 median(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Alpha,Intercept]`),
                 median(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)),
               results$posterior_draws_summaries$intercept_summary$posterior_individual_alpha)


  expect_equal(c(median(expected_results$b_thememomentum + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`),
                 median(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Alpha,Intercept]`),
                 median(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)),
               results$signal_universe_m_d_ref$posterior_individual_alpha[c(2,1,3)])

  #pd alpha
  expect_equal(c(mean(expected_results$b_thememomentum + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]` > 0),
                 mean(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Alpha,Intercept]` > 0),
                 mean(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Gamma,Intercept]` > 0)),
               results$signal_universe_m_d_ref$pd_alpha[c(2,1,3)])

  #alpha t stat
  expect_equal(c(mean(expected_results$b_thememomentum + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`)/
                   sd(expected_results$b_thememomentum + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`),

                 mean(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Alpha,Intercept]`)/
                   sd(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Alpha,Intercept]`),

                 mean(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)/
                   sd(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)
                 ),
               results$signal_universe_m_d_ref$posterior_alpha_t_stat[c(2,1,3)]
               )


  #Check theme beta
  expect_equal(rep(median(expected_results$b_market_factor_proxy), 3), results$posterior_draws_summaries$slope_summary$posterior_theme_beta)

  expect_equal(rep(median(expected_results$b_market_factor_proxy), 3),
               results$signal_universe_m_d_ref$posterior_theme_beta)

  #Check ind beta
  expect_equal(c(median(expected_results$b_market_factor_proxy + expected_results$`r_theme:tickers[momentum_low_Beta,market_factor_proxy]`),
                 median(expected_results$b_market_factor_proxy + expected_results$`r_theme:tickers[value_Alpha,market_factor_proxy]`),
                 median(expected_results$b_market_factor_proxy + expected_results$`r_theme:tickers[value_Gamma,market_factor_proxy]`)),
               results$signal_universe_m_d_ref$posterior_individual_beta[c(2,1,3)])


  #Check sd
  expect_equal(c(median(expected_results$`sd_theme:tickers__Intercept`), median(expected_results$`sd_theme:tickers__market_factor_proxy`),
                   median(expected_results$sigma), median(expected_results$`cor_theme:tickers__Intercept__market_factor_proxy`)),
              unname(unlist(results$posterior_draws_summaries$sd_summary[,c(1,4,7,10)]))
              )
  #Check posterior_predict
  expect_equal(brms::posterior_epred(brm_model) %>% apply(2, function(x) median(x)),
               results$posterior_draws_summaries$epred_summary$.epred[order(results$posterior_draws_summaries$epred_summary$.row)])



})

test_that("fit_bayesian_model adequately fits a bayesian hierarchical model for model spec 3", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #get selected info
  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    chosen_signals = signal_selection_policy$chosen_signals,
    signal_positions = signal_selection_policy$signal_positions,
    backtest_returns_df = backtest_returns_df
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_df <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_df
  selected_market_factor_proxy_df <- benchmark_returns_df[, c("dates", "IBOV")]

  #current info
  current_date <- "2001-07-15"
  selected_backtest_returns_corrected_positions_upd_ref <-
    selected_backtest_returns_corrected_positions_df[which(selected_backtest_returns_corrected_positions_df$dates <= current_date), ]

  selected_market_factor_proxy_vector_upd_ref <-
    selected_market_factor_proxy_df[which(selected_market_factor_proxy_df$dates <= current_date), "IBOV"]

  signal_themes_m_d_ref <- signal_themes_m_df[which(signal_themes_m_df$dates == current_date), ]

  priors_m_upd_ref <- priors_m_df[priors_m_df$dates <= current_date,]

  #expected results
  expected_result <- data.frame(id = paste0(colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1],"-",current_date),
                                tickers = colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1], dates = current_date)
  expected_result$dates <- as.Date(expected_result$dates, format = "%Y-%m-%d")
  expected_result$mean_active_return <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) mean(x))
  expected_result$tracking_error <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) sd(x))
  expected_result$IR <- expected_result$mean_active_return/expected_result$tracking_error

  lm_model_summary_list <- purrr::map(lapply(selected_backtest_returns_corrected_positions_upd_ref[,-1], as.vector),
                                      ~ summary(lm(.x ~ selected_market_factor_proxy_vector_upd_ref)))

  expected_result$alpha <- sapply(lm_model_summary_list, function(x) x$coefficients[1])
  expected_result$alpha_t_stat <- sapply(lm_model_summary_list, function(x) x$coefficients[5])
  expected_result$beta <- sapply(lm_model_summary_list, function(x) x$coefficients[2])
  expected_result$treynor <- expected_result$mean_active_return/expected_result$beta
  expected_result$p_value <- sapply(lm_model_summary_list, function(x) x$coefficients[7])

  #Inside Bayesian Adjustment

  #Create priors for model
  elected_priors <- c(
    # Prior for Value and Mom
    brms::set_prior("normal(0.0012, 0.0016)", class = "b", coef = "themevalue"),
    brms::set_prior("normal(0.0025, 0.0016)", class = "b", coef = "thememomentum"),
    brms::set_prior("normal(0.03, 0.002)", class = "b", coef = "themevalue:market_factor_proxy"),
    brms::set_prior("normal(0.0000, 0.004)", class = "b", coef = "thememomentum:market_factor_proxy"),


    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"),

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )

  future::plan("multisession")
  set.seed(123)
  results <- fit_bayesian_model(selected_backtest_returns_corrected_positions_upd_ref = selected_backtest_returns_corrected_positions_upd_ref,
                                selected_market_factor_proxy_vector_upd_ref = selected_market_factor_proxy_vector_upd_ref,
                                signal_universe_m_d_ref = expected_result,
                                signal_themes_m_d_ref = signal_themes_m_d_ref,
                                elected_priors = elected_priors,
                                model_spec_theme_level = "fixed_intercepts_and_slopes",
                                parallel = TRUE,
                                chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = NA, adapt_delta = 0.98, #MCMC parameters
                                verbose = TRUE
  )

  brm_model <- results$bayesian_model

  #check if brm model was fit correctly
  expect_equal(class(brm_model), "brmsfit")
  expect_equal(brm_model$family$family, "gaussian")

  expect_true(all(brm_model$basis$levels$`theme:tickers` %in% paste0(unique(signal_themes_m_df$theme), "_" ,unique(signal_themes_m_df$tickers))))

  expect_equal(as.character(brm_model$formula$formula)[c(2,1,3)], c("active_return", "~", "0 + theme + theme:market_factor_proxy + (1 + market_factor_proxy | theme:tickers)"))

  #Construct data
  selected_backtest_returns_corrected_positions_upd_ref$market_factor_proxy <- selected_market_factor_proxy_vector_upd_ref
  selected_backtest_returns_corrected_positions_upd_ref_long <- reshape2::melt(selected_backtest_returns_corrected_positions_upd_ref,
                                                                               id.vars = c("dates", "market_factor_proxy"),
                                                                               variable.name = "tickers", value.name = "active_return")

  selected_backtest_returns_corrected_positions_upd_ref_long <-
    selected_backtest_returns_corrected_positions_upd_ref_long[order(selected_backtest_returns_corrected_positions_upd_ref_long$dates),]
  selected_backtest_returns_corrected_positions_upd_ref_long <- dplyr::left_join(selected_backtest_returns_corrected_positions_upd_ref_long,
                                                                                 signal_themes_m_d_ref %>% dplyr::select(tickers, theme), by = "tickers")
  selected_backtest_returns_corrected_positions_upd_ref_long$`theme:tickers` <- paste0(selected_backtest_returns_corrected_positions_upd_ref_long$theme, "_", selected_backtest_returns_corrected_positions_upd_ref_long$tickers)

  selected_backtest_returns_corrected_positions_upd_ref_long <-
    selected_backtest_returns_corrected_positions_upd_ref_long[, c("active_return", "theme",  "market_factor_proxy", "tickers", "theme:tickers")]

  # Copy the data frame
  data_only <- brm_model$data
  # Remove unwanted attributes
  attr(data_only, "terms") <- NULL
  attr(data_only, "drop_unused_levels") <- NULL
  attr(data_only, "data_name") <- NULL
  expect_equal(data_only, selected_backtest_returns_corrected_positions_upd_ref_long)

  # Extract priors set by the user in brm_model
  user_priors_in_model <- subset(brm_model$prior, source == "user")

  # Select relevant columns for comparison
  cols_to_compare <- c("prior", "class", "coef", "group", "source")

  # Sort the data frames
  user_priors_in_model_sorted <- user_priors_in_model[, cols_to_compare]

  user_priors_in_model_sorted$class[5] <- "cor"
  user_priors_in_model_sorted$prior[5] <- "lkj(2)"
  user_priors_in_model_sorted <- user_priors_in_model_sorted[c(3,1,4,2,6,7,8,5),]
  rownames(user_priors_in_model_sorted) <- NULL


  elected_priors_sorted <- elected_priors[, cols_to_compare]

  rownames(elected_priors_sorted) <- NULL

  expect_equal(user_priors_in_model_sorted, elected_priors_sorted)

  #Check if MCMC parameters are right
  expect_equal(brm_model$stan_args$control$adapt_delta, 0.98)

  #Check number of rows in predicted_summary
  n_draws <- nrow(results$posterior_draws_summaries$predicted_summary) %>% as.numeric()
  expected_draws <- length(selected_market_factor_proxy_vector_upd_ref)*(ncol(selected_backtest_returns_corrected_positions_upd_ref) - 2)
  expect_true(n_draws == expected_draws)

  #Check number of rows in posterior_draws
  expect_equal((ncol(results$posterior_draws_summaries$intercept_summary) - 1)/3, 2) #Posterior theme and individual
  expect_equal((ncol(results$posterior_draws_summaries$slope_summary) - 1)/3, 2) #Posterior theme and individual
  expect_equal(ncol(results$posterior_draws_summaries$sd_summary)/3, 4) #Posterior theme and individual

  #Check tidydraws
  expected_results <- insight::get_parameters(brm_model, effects = "all")

  #Check theme alpha and beta
  #theme alpha
  expect_equal(c(median(expected_results$b_thememomentum), rep(median(expected_results$b_themevalue), 2)),
               results$posterior_draws_summaries$intercept_summary$posterior_theme_alpha)


  expect_equal(c(median(expected_results$b_thememomentum), rep(median(expected_results$b_themevalue), 2)),
               results$signal_universe_m_d_ref$posterior_theme_alpha[c(2,1,3)])

  #pd theme alpha
  expect_equal(c(mean(expected_results$b_thememomentum > 0), rep(mean(expected_results$b_themevalue > 0), 2)),
               results$signal_universe_m_d_ref$pd_theme_alpha[c(2,1,3)])

  #indi alpha
  expect_equal(c(median(expected_results$b_thememomentum + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`),
                 median(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Alpha,Intercept]`),
                 median(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)),
               results$posterior_draws_summaries$intercept_summary$posterior_individual_alpha)


  expect_equal(c(median(expected_results$b_thememomentum + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`),
                 median(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Alpha,Intercept]`),
                 median(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)),
               results$signal_universe_m_d_ref$posterior_individual_alpha[c(2,1,3)])

  #pd alpha
  expect_equal(c(mean(expected_results$b_thememomentum + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]` > 0),
                 mean(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Alpha,Intercept]` > 0),
                 mean(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Gamma,Intercept]` > 0)),
               results$signal_universe_m_d_ref$pd_alpha[c(2,1,3)])

  #alpha t stat
  expect_equal(c(mean(expected_results$b_thememomentum + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`)/
                   sd(expected_results$b_thememomentum + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`),

                 mean(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Alpha,Intercept]`)/
                   sd(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Alpha,Intercept]`),

                 mean(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)/
                   sd(expected_results$b_themevalue + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)
  ),
  results$signal_universe_m_d_ref$posterior_alpha_t_stat[c(2,1,3)]
  )


  #Check theme beta
  expect_equal(c(median(expected_results$`b_thememomentum:market_factor_proxy`), rep(median(expected_results$`b_themevalue:market_factor_proxy`),2)),
                  results$posterior_draws_summaries$slope_summary$posterior_theme_beta)

  expect_equal(c(median(expected_results$`b_thememomentum:market_factor_proxy`), rep(median(expected_results$`b_themevalue:market_factor_proxy`),2)),
               results$signal_universe_m_d_ref$posterior_theme_beta[c(2,1,3)])

  #Check ind beta
  expect_equal(c(median(expected_results$`b_thememomentum:market_factor_proxy` + expected_results$`r_theme:tickers[momentum_low_Beta,market_factor_proxy]`),
                 median(expected_results$`b_themevalue:market_factor_proxy` + expected_results$`r_theme:tickers[value_Alpha,market_factor_proxy]`),
                 median(expected_results$`b_themevalue:market_factor_proxy`+ expected_results$`r_theme:tickers[value_Gamma,market_factor_proxy]`)),
               results$signal_universe_m_d_ref$posterior_individual_beta[c(2,1,3)])


  #Check sd
  expect_equal(c(median(expected_results$`sd_theme:tickers__Intercept`), median(expected_results$`sd_theme:tickers__market_factor_proxy`),
                 median(expected_results$sigma), median(expected_results$`cor_theme:tickers__Intercept__market_factor_proxy`)),
               unname(unlist(results$posterior_draws_summaries$sd_summary[,c(1,4,7,10)]))
  )
  #Check posterior_predict
  expect_equal(brms::posterior_epred(brm_model) %>% apply(2, function(x) median(x)),
               results$posterior_draws_summaries$epred_summary$.epred[order(results$posterior_draws_summaries$epred_summary$.row)])



})

test_that("fit_bayesian_model adequately fits a bayesian hierarchical model for model spec 4", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  #get selected info
  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    chosen_signals = signal_selection_policy$chosen_signals,
    signal_positions = signal_selection_policy$signal_positions,
    backtest_returns_df = backtest_returns_df
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_df <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_df
  selected_market_factor_proxy_df <- benchmark_returns_df[, c("dates", "IBOV")]

  #current info
  current_date <- "2001-07-15"
  selected_backtest_returns_corrected_positions_upd_ref <-
    selected_backtest_returns_corrected_positions_df[which(selected_backtest_returns_corrected_positions_df$dates <= current_date), ]

  selected_market_factor_proxy_vector_upd_ref <-
    selected_market_factor_proxy_df[which(selected_market_factor_proxy_df$dates <= current_date), "IBOV"]

  signal_themes_m_d_ref <- signal_themes_m_df[which(signal_themes_m_df$dates == current_date), ]

  priors_m_upd_ref <- priors_m_df[priors_m_df$dates <= current_date,]

  #expected results
  expected_result <- data.frame(id = paste0(colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1],"-",current_date),
                                tickers = colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1], dates = current_date)
  expected_result$dates <- as.Date(expected_result$dates, format = "%Y-%m-%d")
  expected_result$mean_active_return <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) mean(x))
  expected_result$tracking_error <- selected_backtest_returns_corrected_positions_upd_ref[,-1] %>% apply(2, function(x) sd(x))
  expected_result$IR <- expected_result$mean_active_return/expected_result$tracking_error

  lm_model_summary_list <- purrr::map(lapply(selected_backtest_returns_corrected_positions_upd_ref[,-1], as.vector),
                                      ~ summary(lm(.x ~ selected_market_factor_proxy_vector_upd_ref)))

  expected_result$alpha <- sapply(lm_model_summary_list, function(x) x$coefficients[1])
  expected_result$alpha_t_stat <- sapply(lm_model_summary_list, function(x) x$coefficients[5])
  expected_result$beta <- sapply(lm_model_summary_list, function(x) x$coefficients[2])
  expected_result$treynor <- expected_result$mean_active_return/expected_result$beta
  expected_result$p_value <- sapply(lm_model_summary_list, function(x) x$coefficients[7])

  #Inside Bayesian Adjustment

  #Create priors for model
  elected_priors <- c(
    # Prior for Value and Mom
    brms::set_prior("normal(0.0012, 0.0016)", class = "Intercept"),
    brms::set_prior("normal(0.0025, 0.0016)", class = "b", coef = "market_factor_proxy"),

    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"),

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )

  future::plan("multisession")
  set.seed(123)
  results <- fit_bayesian_model(selected_backtest_returns_corrected_positions_upd_ref = selected_backtest_returns_corrected_positions_upd_ref,
                                selected_market_factor_proxy_vector_upd_ref = selected_market_factor_proxy_vector_upd_ref,
                                signal_universe_m_d_ref = expected_result,
                                signal_themes_m_d_ref = signal_themes_m_d_ref,
                                elected_priors = elected_priors,
                                model_spec_theme_level = "none",
                                parallel = TRUE,
                                chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = NA, adapt_delta = 0.98, #MCMC parameters
                                verbose = TRUE
  )

  brm_model <- results$bayesian_model

  #check if brm model was fit correctly
  expect_equal(class(brm_model), "brmsfit")
  expect_equal(brm_model$family$family, "gaussian")

  expect_true(all(brm_model$basis$levels$`theme:tickers` %in% paste0(unique(signal_themes_m_df$theme), "_" ,unique(signal_themes_m_df$tickers))))

  expect_equal(as.character(brm_model$formula$formula)[c(2,1,3)], c("active_return", "~", "market_factor_proxy + (1 + market_factor_proxy | theme:tickers)"))

  #Construct data
  selected_backtest_returns_corrected_positions_upd_ref$market_factor_proxy <- selected_market_factor_proxy_vector_upd_ref
  selected_backtest_returns_corrected_positions_upd_ref_long <- reshape2::melt(selected_backtest_returns_corrected_positions_upd_ref,
                                                                               id.vars = c("dates", "market_factor_proxy"),
                                                                               variable.name = "tickers", value.name = "active_return")

  selected_backtest_returns_corrected_positions_upd_ref_long <-
    selected_backtest_returns_corrected_positions_upd_ref_long[order(selected_backtest_returns_corrected_positions_upd_ref_long$dates),]
  selected_backtest_returns_corrected_positions_upd_ref_long <- dplyr::left_join(selected_backtest_returns_corrected_positions_upd_ref_long,
                                                                                 signal_themes_m_d_ref %>% dplyr::select(tickers, theme), by = "tickers")
  selected_backtest_returns_corrected_positions_upd_ref_long$`theme:tickers` <- paste0(selected_backtest_returns_corrected_positions_upd_ref_long$theme, "_", selected_backtest_returns_corrected_positions_upd_ref_long$tickers)

  selected_backtest_returns_corrected_positions_upd_ref_long <-
    selected_backtest_returns_corrected_positions_upd_ref_long[, c("active_return", "market_factor_proxy", "theme", "tickers", "theme:tickers")]

  # Copy the data frame
  data_only <- brm_model$data
  # Remove unwanted attributes
  attr(data_only, "terms") <- NULL
  attr(data_only, "drop_unused_levels") <- NULL
  attr(data_only, "data_name") <- NULL
  expect_equal(data_only, selected_backtest_returns_corrected_positions_upd_ref_long)

  # Extract priors set by the user in brm_model
  user_priors_in_model <- subset(brm_model$prior, source == "user")

  # Select relevant columns for comparison
  cols_to_compare <- c("prior", "class", "coef", "group", "source")

  # Sort the data frames
  user_priors_in_model_sorted <- user_priors_in_model[, cols_to_compare]

  user_priors_in_model_sorted$class[3] <- "cor"
  user_priors_in_model_sorted$prior[3] <- "lkj(2)"
  user_priors_in_model_sorted <- user_priors_in_model_sorted[c(2,1,4,5,6,3),]
  rownames(user_priors_in_model_sorted) <- NULL


  elected_priors_sorted <- elected_priors[, cols_to_compare]

  rownames(elected_priors_sorted) <- NULL

  expect_equal(user_priors_in_model_sorted, elected_priors_sorted)

  #Check if MCMC parameters are right
  expect_equal(brm_model$stan_args$control$adapt_delta, 0.98)

  #Check number of rows in predicted_summary
  n_draws <- nrow(results$posterior_draws_summaries$predicted_summary) %>% as.numeric()
  expected_draws <- length(selected_market_factor_proxy_vector_upd_ref)*(ncol(selected_backtest_returns_corrected_positions_upd_ref) - 2)
  expect_true(n_draws == expected_draws)

  #Check number of rows in posterior_draws
  expect_equal((ncol(results$posterior_draws_summaries$intercept_summary) - 1)/3, 2) #Posterior theme and individual
  expect_equal((ncol(results$posterior_draws_summaries$slope_summary) - 1)/3, 2) #Posterior theme and individual
  expect_equal(ncol(results$posterior_draws_summaries$sd_summary)/3, 4) #Posterior theme and individual

  #Check tidydraws
  expected_results <- insight::get_parameters(brm_model, effects = "all")

  #Check theme alpha and beta
  #theme alpha
  expect_equal(rep(median(expected_results$b_Intercept), 3),
               results$posterior_draws_summaries$intercept_summary$posterior_theme_alpha)


  expect_equal(rep(median(expected_results$b_Intercept), 3),
               results$signal_universe_m_d_ref$posterior_theme_alpha[c(2,1,3)])

  #pd theme alpha
  expect_equal(rep(mean(expected_results$b_Intercept > 0), 3),
               results$signal_universe_m_d_ref$pd_theme_alpha[c(2,1,3)])

  #indi alpha
  expect_equal(c(median(expected_results$b_Intercept + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`),
                 median(expected_results$b_Intercept + expected_results$`r_theme:tickers[value_Alpha,Intercept]`),
                 median(expected_results$b_Intercept + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)),
               results$posterior_draws_summaries$intercept_summary$posterior_individual_alpha)


  expect_equal(c(median(expected_results$b_Intercept + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`),
                 median(expected_results$b_Intercept + expected_results$`r_theme:tickers[value_Alpha,Intercept]`),
                 median(expected_results$b_Intercept + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)),
               results$signal_universe_m_d_ref$posterior_individual_alpha[c(2,1,3)])

  #pd alpha
  expect_equal(c(mean(expected_results$b_Intercept + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]` > 0),
                 mean(expected_results$b_Intercept + expected_results$`r_theme:tickers[value_Alpha,Intercept]` > 0),
                 mean(expected_results$b_Intercept + expected_results$`r_theme:tickers[value_Gamma,Intercept]` > 0)),
               results$signal_universe_m_d_ref$pd_alpha[c(2,1,3)])

  #alpha t stat
  expect_equal(c(mean(expected_results$b_Intercept + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`)/
                   sd(expected_results$b_Intercept + expected_results$`r_theme:tickers[momentum_low_Beta,Intercept]`),

                 mean(expected_results$b_Intercept + expected_results$`r_theme:tickers[value_Alpha,Intercept]`)/
                   sd(expected_results$b_Intercept + expected_results$`r_theme:tickers[value_Alpha,Intercept]`),

                 mean(expected_results$b_Intercept + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)/
                   sd(expected_results$b_Intercept + expected_results$`r_theme:tickers[value_Gamma,Intercept]`)
  ),
  results$signal_universe_m_d_ref$posterior_alpha_t_stat[c(2,1,3)]
  )


  #Check theme beta
  expect_equal(rep(median(expected_results$b_market_factor_proxy), 3),
               results$posterior_draws_summaries$slope_summary$posterior_theme_beta)

  expect_equal(rep(median(expected_results$b_market_factor_proxy), 3),
               results$signal_universe_m_d_ref$posterior_theme_beta[c(2,1,3)])

  #Check ind beta
  expect_equal(c(median(expected_results$b_market_factor_proxy + expected_results$`r_theme:tickers[momentum_low_Beta,market_factor_proxy]`),
                 median(expected_results$b_market_factor_proxy + expected_results$`r_theme:tickers[value_Alpha,market_factor_proxy]`),
                 median(expected_results$b_market_factor_proxy + expected_results$`r_theme:tickers[value_Gamma,market_factor_proxy]`)),
               results$signal_universe_m_d_ref$posterior_individual_beta[c(2,1,3)])


  #Check sd
  expect_equal(c(median(expected_results$`sd_theme:tickers__Intercept`), median(expected_results$`sd_theme:tickers__market_factor_proxy`),
                 median(expected_results$sigma), median(expected_results$`cor_theme:tickers__Intercept__market_factor_proxy`)),
               unname(unlist(results$posterior_draws_summaries$sd_summary[,c(1,4,7,10)]))
  )
  #Check posterior_predict
  expect_equal(brms::posterior_epred(brm_model) %>% apply(2, function(x) median(x)),
               results$posterior_draws_summaries$epred_summary$.epred[order(results$posterior_draws_summaries$epred_summary$.row)])



})
