test_that("fit_bayesian_hierarchical_model adequately fits a bayesian hierarchical model for model spec 1", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  #get selected info
  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    signal_themes_m_df = signal_themes_m_df,
    chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_m_xts = backtest_returns_m_xts
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_market_factor_proxy_m_xts <- benchmark_returns_m_xts[, c("IBOV")]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df


  #current info
  current_date <- "2001-07-15"
  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:5), ]

  selected_market_factor_proxy_m_xts_upd_ref <- selected_market_factor_proxy_m_xts[c(1:5), "IBOV"]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date), ]

  priors_m_upd_ref <- priors_m_df[priors_m_df$dates <= current_date,]

  #expected results
  expected_result <- summarize_performance(
    model_structure = "partial_pooled", model_spec_theme_level = "random_intercept_fixed_slope",
    lmer_control = list(lmer_optimizer = "nloptwrap", lmer_optimization_objective = "REML", hierarchical_p_value_method = "Satterthwaite"),
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
    selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref
  )


  #Inside Bayesian Adjustment

  #Create priors for model
  elected_priors <- c(
    # Prior for Intercept
    brms::set_prior("normal(0.0012, 0.0016)", class = "Intercept"), #ok

    # Prior for market_factor_proxy coefficient
    brms::set_prior("normal(0.0003, 0.0003)", class = "b", coef = "market_factor_proxy"), #ok

    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"), #ok

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"), #ok

    # Prior for sd of Intercept at theme level
    brms::set_prior("student_t(30, 0, 0.0011)", class = "sd", group = "theme", coef = "Intercept"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )

  future::plan("multisession")
  set.seed(123)
  suppressWarnings(
  results <- fit_bayesian_hierarchical_model(selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
                                signal_universe_m_d_ref = expected_result$signal_universe_m_d_ref,
                                selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
                                elected_priors = elected_priors,
                                model_spec_theme_level = "random_intercept_fixed_slope",
                                parallel = TRUE,
                                chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = NA, adapt_delta = 0.85, #MCMC parameters
                                verbose = TRUE
  )
  )

  brm_model <- results$brm_model

  #check if brm model was fit correctly
  expect_equal(class(brm_model), "brmsfit")
  expect_equal(brm_model$family$family, "gaussian")

  expect_true(all(brm_model$basis$levels$theme %in% unique(selected_signal_themes_m_d_ref$theme)))
  expect_true(all(brm_model$basis$levels$`theme:tickers` %in%
                    (selected_signal_themes_m_d_ref %>% dplyr::mutate(theme_tickers = paste0(theme, "_", tickers)) %>% dplyr::pull(theme_tickers))
                  ))


  expect_equal(as.character(brm_model$formula$formula)[c(2,1,3)], c("return", "~", "market_factor_proxy + (1 | theme) + (1 + market_factor_proxy | theme:tickers)"))

  #Construct data
  selected_backtest_returns_corrected_positions_m_xts_upd_ref$market_factor_proxy <- selected_market_factor_proxy_m_xts_upd_ref
  selected_backtest_returns_corrected_positions_m_upd_ref <- tibble::rownames_to_column(as.data.frame(selected_backtest_returns_corrected_positions_m_xts_upd_ref),
                                                                                        var = "dates")

  selected_backtest_returns_corrected_positions_m_upd_ref_long <- reshape2::melt(selected_backtest_returns_corrected_positions_m_upd_ref,
                                                                               id.vars = c("dates", "market_factor_proxy"),
                                                                               variable.name = "tickers", value.name = "return")

  selected_backtest_returns_corrected_positions_m_upd_ref_long <-
    selected_backtest_returns_corrected_positions_m_upd_ref_long[order(selected_backtest_returns_corrected_positions_m_upd_ref_long$dates),]

  selected_backtest_returns_corrected_positions_m_upd_ref_long <- dplyr::left_join(selected_backtest_returns_corrected_positions_m_upd_ref_long,
                                                                                 selected_signal_themes_m_d_ref %>% dplyr::select(tickers, theme), by = "tickers")
  selected_backtest_returns_corrected_positions_m_upd_ref_long$`theme:tickers` <- paste0(selected_backtest_returns_corrected_positions_m_upd_ref_long$theme, "_", selected_backtest_returns_corrected_positions_m_upd_ref_long$tickers)

  selected_backtest_returns_corrected_positions_m_upd_ref_long <-
    selected_backtest_returns_corrected_positions_m_upd_ref_long[, c("return", "market_factor_proxy", "theme", "tickers", "theme:tickers")]

  # Copy the data frame
  data_only <- brm_model$data
  # Remove unwanted attributes
  attr(data_only, "terms") <- NULL
  attr(data_only, "drop_unused_levels") <- NULL
  attr(data_only, "data_name") <- NULL
  expect_equal(data_only, selected_backtest_returns_corrected_positions_m_upd_ref_long)

  # Extract priors set by the user in brm_model
  user_priors_in_model <- subset(brm_model$prior, source == "user")

  # Select relevant columns for comparison
  cols_to_compare <- c("prior", "class", "coef", "group", "source")

  # Sort the data frames
  user_priors_in_model <- user_priors_in_model[, cols_to_compare]

  # Correct the class for the LKJ prior
  user_priors_in_model$class[user_priors_in_model$prior == "lkj_corr_cholesky(2)"] <- "cor"
  user_priors_in_model$prior[user_priors_in_model$prior == "lkj_corr_cholesky(2)"] <- "lkj(2)"

  rownames(user_priors_in_model) <- NULL
  elected_priors <- elected_priors[, cols_to_compare]
  rownames(elected_priors) <- NULL

  expect_equal(
    user_priors_in_model %>% dplyr::arrange(dplyr::across(dplyr::everything())),
    elected_priors %>% dplyr::arrange(dplyr::across(dplyr::everything()))
  )

  #Check if MCMC parameters are right
  expect_equal(brm_model$stan_args$control$adapt_delta, 0.85)

  #Check number of rows in predicted_summary
  n_draws <- nrow(results$posterior_draws_summaries$predicted_summary) %>% as.numeric()
  expected_draws <- length(selected_market_factor_proxy_m_xts_upd_ref)*(ncol(selected_backtest_returns_corrected_positions_m_xts_upd_ref) - 1)
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

  future::plan("sequential")
})

test_that("fit_bayesian_hierarchical_model adequately fits a bayesian hierarchical model for model spec 1 with plan(multisession)", {

  testthat::skip()
  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  #get selected info
  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    signal_themes_m_df = signal_themes_m_df,
    chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_m_xts = backtest_returns_m_xts
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_market_factor_proxy_m_xts <- benchmark_returns_m_xts[, c("IBOV")]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df


  #current info
  current_date <- "2001-07-15"
  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:5), ]

  selected_market_factor_proxy_m_xts_upd_ref <- selected_market_factor_proxy_m_xts[c(1:5), "IBOV"]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date), ]

  priors_m_upd_ref <- priors_m_df[priors_m_df$dates <= current_date,]

  #expected results
  expected_result <- summarize_performance(
    model_structure = "partial_pooled", model_spec_theme_level = "random_intercept_fixed_slope",
    lmer_control = list(lmer_optimizer = "nloptwrap", lmer_optimization_objective = "REML", hierarchical_p_value_method = "Satterthwaite"),
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
    selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref
  )


  #Inside Bayesian Adjustment

  #Create priors for model
  elected_priors <- c(
    # Prior for Intercept
    brms::set_prior("normal(0.0012, 0.0016)", class = "Intercept"), #ok

    # Prior for market_factor_proxy coefficient
    brms::set_prior("normal(0.0003, 0.0003)", class = "b", coef = "market_factor_proxy"), #ok

    # Prior for sd of Intercept at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"), #ok

    # Prior for sd of market_factor_proxy at theme:tickers level
    brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"), #ok

    # Prior for sd of Intercept at theme level
    brms::set_prior("student_t(30, 0, 0.0011)", class = "sd", group = "theme", coef = "Intercept"),

    # Prior for residual error (sigma)
    brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),

    # LKJ prior for correlations
    brms::set_prior("lkj(2)", class = "cor")
  )

  future::plan("multisession")
  set.seed(123)
  suppressWarnings(
    results <- fit_bayesian_hierarchical_model(selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                               selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
                                               signal_universe_m_d_ref = expected_result$signal_universe_m_d_ref,
                                               selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
                                               elected_priors = elected_priors,
                                               model_spec_theme_level = "random_intercept_fixed_slope",
                                               parallel = TRUE,
                                               chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = NA, adapt_delta = 0.85, #MCMC parameters
                                               verbose = TRUE
    )
  )

  brm_model <- results$brm_model

  #check if brm model was fit correctly
  expect_equal(class(brm_model), "brmsfit")
  expect_equal(brm_model$family$family, "gaussian")

  expect_true(all(brm_model$basis$levels$theme %in% unique(selected_signal_themes_m_d_ref$theme)))
  expect_true(all(brm_model$basis$levels$`theme:tickers` %in%
                    (selected_signal_themes_m_d_ref %>% dplyr::mutate(theme_tickers = paste0(theme, "_", tickers)) %>% dplyr::pull(theme_tickers))
  ))


  expect_equal(as.character(brm_model$formula$formula)[c(2,1,3)], c("return", "~", "market_factor_proxy + (1 | theme) + (1 + market_factor_proxy | theme:tickers)"))

  #Construct data
  selected_backtest_returns_corrected_positions_m_xts_upd_ref$market_factor_proxy <- selected_market_factor_proxy_m_xts_upd_ref
  selected_backtest_returns_corrected_positions_m_upd_ref <- tibble::rownames_to_column(as.data.frame(selected_backtest_returns_corrected_positions_m_xts_upd_ref),
                                                                                        var = "dates")

  selected_backtest_returns_corrected_positions_m_upd_ref_long <- reshape2::melt(selected_backtest_returns_corrected_positions_m_upd_ref,
                                                                                 id.vars = c("dates", "market_factor_proxy"),
                                                                                 variable.name = "tickers", value.name = "return")

  selected_backtest_returns_corrected_positions_m_upd_ref_long <-
    selected_backtest_returns_corrected_positions_m_upd_ref_long[order(selected_backtest_returns_corrected_positions_m_upd_ref_long$dates),]

  selected_backtest_returns_corrected_positions_m_upd_ref_long <- dplyr::left_join(selected_backtest_returns_corrected_positions_m_upd_ref_long,
                                                                                   selected_signal_themes_m_d_ref %>% dplyr::select(tickers, theme), by = "tickers")
  selected_backtest_returns_corrected_positions_m_upd_ref_long$`theme:tickers` <- paste0(selected_backtest_returns_corrected_positions_m_upd_ref_long$theme, "_", selected_backtest_returns_corrected_positions_m_upd_ref_long$tickers)

  selected_backtest_returns_corrected_positions_m_upd_ref_long <-
    selected_backtest_returns_corrected_positions_m_upd_ref_long[, c("return", "market_factor_proxy", "theme", "tickers", "theme:tickers")]

  # Copy the data frame
  data_only <- brm_model$data
  # Remove unwanted attributes
  attr(data_only, "terms") <- NULL
  attr(data_only, "drop_unused_levels") <- NULL
  attr(data_only, "data_name") <- NULL
  expect_equal(data_only, selected_backtest_returns_corrected_positions_m_upd_ref_long)

  # Extract priors set by the user in brm_model
  user_priors_in_model <- subset(brm_model$prior, source == "user")

  # Select relevant columns for comparison
  cols_to_compare <- c("prior", "class", "coef", "group", "source")

  # Sort the data frames
  user_priors_in_model <- user_priors_in_model[, cols_to_compare]

  # Correct the class for the LKJ prior
  user_priors_in_model$class[user_priors_in_model$prior == "lkj_corr_cholesky(2)"] <- "cor"
  user_priors_in_model$prior[user_priors_in_model$prior == "lkj_corr_cholesky(2)"] <- "lkj(2)"

  rownames(user_priors_in_model) <- NULL
  elected_priors <- elected_priors[, cols_to_compare]
  rownames(elected_priors) <- NULL

  expect_equal(
    user_priors_in_model %>% dplyr::arrange(dplyr::across(dplyr::everything())),
    elected_priors %>% dplyr::arrange(dplyr::across(dplyr::everything()))
  )

  #Check if MCMC parameters are right
  expect_equal(brm_model$stan_args$control$adapt_delta, 0.85)

  #Check number of rows in predicted_summary
  n_draws <- nrow(results$posterior_draws_summaries$predicted_summary) %>% as.numeric()
  expected_draws <- length(selected_market_factor_proxy_m_xts_upd_ref)*(ncol(selected_backtest_returns_corrected_positions_m_xts_upd_ref) - 1)
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


  future::plan("sequential")
})

test_that("fit_bayesian_hierarchical_model adequately fits a bayesian hierarchical model for model spec 2", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  #get selected info
  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    chosen_signals_and_positions = chosen_signals_and_positions,
    signal_themes_m_df = signal_themes_m_df,
    backtest_returns_m_xts = backtest_returns_m_xts
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_market_factor_proxy_m_xts <- benchmark_returns_m_xts[, c("IBOV")]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  #current info
  current_date <- "2001-07-15"
  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:5), ]

  selected_market_factor_proxy_m_xts_upd_ref <- selected_market_factor_proxy_m_xts[c(1:5), "IBOV"]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date), ]

  priors_m_upd_ref <- priors_m_df[priors_m_df$dates <= current_date,]

  #expected results
  expected_result <- summarize_performance(
    model_structure = "partial_pooled", model_spec_theme_level = "theme_specific_intercept_fixed_slope",
    lmer_control = list(lmer_optimizer = "nloptwrap", lmer_optimization_objective = "REML", hierarchical_p_value_method = "Satterthwaite"),
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
    selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref
  )

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
  results <- fit_bayesian_hierarchical_model(selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
                                signal_universe_m_d_ref = expected_result$signal_universe_m_d_ref,
                                selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
                                elected_priors = elected_priors,
                                model_spec_theme_level = "theme_specific_intercept_fixed_slope",
                                parallel = TRUE,
                                chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = NA, adapt_delta = 0.99, #MCMC parameters
                                verbose = TRUE
  )

  brm_model <- results$brm_model

  #check if brm model was fit correctly
  expect_equal(class(brm_model), "brmsfit")
  expect_equal(brm_model$family$family, "gaussian")

  expect_true(all(brm_model$basis$levels$`theme:tickers` %in%
                    (selected_signal_themes_m_d_ref %>% dplyr::mutate(theme_tickers = paste0(theme, "_", tickers)) %>% dplyr::pull(theme_tickers))
  ))

  expect_equal(as.character(brm_model$formula$formula)[c(2,1,3)], c("return", "~", "0 + theme + market_factor_proxy + (1 + market_factor_proxy | theme:tickers)"))

  #Construct data
  selected_backtest_returns_corrected_positions_m_xts_upd_ref$market_factor_proxy <- selected_market_factor_proxy_m_xts_upd_ref
  selected_backtest_returns_corrected_positions_m_upd_ref_long <- as.data.frame(selected_backtest_returns_corrected_positions_m_xts_upd_ref) %>%
    tibble::rownames_to_column(var = "dates")

  selected_backtest_returns_corrected_positions_m_upd_ref_long <- reshape2::melt(selected_backtest_returns_corrected_positions_m_upd_ref_long,
                                                                               id.vars = c("dates", "market_factor_proxy"),
                                                                               variable.name = "tickers", value.name = "return")

  selected_backtest_returns_corrected_positions_m_upd_ref_long <-
    selected_backtest_returns_corrected_positions_m_upd_ref_long[order(selected_backtest_returns_corrected_positions_m_upd_ref_long$dates),]
  selected_backtest_returns_corrected_positions_m_upd_ref_long <- dplyr::left_join(selected_backtest_returns_corrected_positions_m_upd_ref_long,
                                                                                   selected_signal_themes_m_d_ref %>% dplyr::select(tickers, theme), by = "tickers")
  selected_backtest_returns_corrected_positions_m_upd_ref_long$`theme:tickers` <- paste0(selected_backtest_returns_corrected_positions_m_upd_ref_long$theme, "_", selected_backtest_returns_corrected_positions_m_upd_ref_long$tickers)

  selected_backtest_returns_corrected_positions_m_upd_ref_long <-
    selected_backtest_returns_corrected_positions_m_upd_ref_long[, c("return", "theme",  "market_factor_proxy", "tickers", "theme:tickers")]

  # Copy the data frame
  data_only <- brm_model$data
  # Remove unwanted attributes
  attr(data_only, "terms") <- NULL
  attr(data_only, "drop_unused_levels") <- NULL
  attr(data_only, "data_name") <- NULL
  expect_equal(data_only, selected_backtest_returns_corrected_positions_m_upd_ref_long)

  # Extract priors set by the user in brm_model
  user_priors_in_model <- subset(brm_model$prior, source == "user")

  # Select relevant columns for comparison
  cols_to_compare <- c("prior", "class", "coef", "group", "source")

  # Sort the data frames
  user_priors_in_model <- user_priors_in_model[, cols_to_compare]

  user_priors_in_model$class[1] <- "cor"
  user_priors_in_model$prior[1] <- "lkj(2)"
  rownames(user_priors_in_model) <- NULL

  elected_priors <- elected_priors[, cols_to_compare]
  rownames(elected_priors) <- NULL

  testthat::expect_equal(
    user_priors_in_model %>% dplyr::arrange(dplyr::across(dplyr::everything())),
    elected_priors %>% dplyr::arrange(dplyr::across(dplyr::everything()))
  )

  #Check if MCMC parameters are right
  expect_equal(brm_model$stan_args$control$adapt_delta, 0.99)

  #Check number of rows in predicted_summary
  n_draws <- nrow(results$posterior_draws_summaries$predicted_summary) %>% as.numeric()
  expected_draws <- length(selected_market_factor_proxy_m_xts_upd_ref)*(ncol(selected_backtest_returns_corrected_positions_m_xts_upd_ref) - 1)
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

  future::plan("sequential")

})

test_that("fit_bayesian_hierarchical_model adequately fits a bayesian hierarchical model for model spec 3", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  #get selected info
  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    chosen_signals_and_positions = chosen_signals_and_positions,
    signal_themes_m_df = signal_themes_m_df,
    backtest_returns_m_xts = backtest_returns_m_xts
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_market_factor_proxy_m_xts <- benchmark_returns_m_xts[, c("IBOV")]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  #current info
  current_date <- "2001-07-15"
  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:5), ]

  selected_market_factor_proxy_m_xts_upd_ref <- selected_market_factor_proxy_m_xts[c(1:5), "IBOV"]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date), ]

  priors_m_upd_ref <- priors_m_df[priors_m_df$dates <= current_date,]

  #expected results
  expected_result <- summarize_performance(
    model_structure = "partial_pooled", model_spec_theme_level = "theme_specific_intercept_theme_specific_slope",
    lmer_control = list(lmer_optimizer = "nloptwrap", lmer_optimization_objective = "REML", hierarchical_p_value_method = "Satterthwaite"),
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
    selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref
  )

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
  results <- fit_bayesian_hierarchical_model(selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
                                signal_universe_m_d_ref = expected_result$signal_universe_m_d_ref,
                                selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
                                elected_priors = elected_priors,
                                model_spec_theme_level = "theme_specific_intercept_theme_specific_slope",
                                parallel = TRUE,
                                chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = NA, adapt_delta = 0.98, #MCMC parameters
                                verbose = TRUE
  )

  brm_model <- results$brm_model

  #check if brm model was fit correctly
  expect_equal(class(brm_model), "brmsfit")
  expect_equal(brm_model$family$family, "gaussian")

  expect_true(all(brm_model$basis$levels$`theme:tickers` %in%
                    (selected_signal_themes_m_d_ref %>% dplyr::mutate(theme_tickers = paste0(theme, "_", tickers)) %>% dplyr::pull(theme_tickers))
  ))

  expect_equal(as.character(brm_model$formula$formula)[c(2,1,3)], c("return", "~", "0 + theme + theme:market_factor_proxy + (1 + market_factor_proxy | theme:tickers)"))

  #Construct data
  selected_backtest_returns_corrected_positions_m_xts_upd_ref$market_factor_proxy <- selected_market_factor_proxy_m_xts_upd_ref
  selected_backtest_returns_corrected_positions_m_upd_ref <- as.data.frame(selected_backtest_returns_corrected_positions_m_xts_upd_ref) %>%
    tibble::rownames_to_column(var = "dates")
  selected_backtest_returns_corrected_positions_m_upd_ref <- reshape2::melt(selected_backtest_returns_corrected_positions_m_upd_ref,
                                                                               id.vars = c("dates", "market_factor_proxy"),
                                                                               variable.name = "tickers", value.name = "return")

  selected_backtest_returns_corrected_positions_m_upd_ref <-
    selected_backtest_returns_corrected_positions_m_upd_ref[order(selected_backtest_returns_corrected_positions_m_upd_ref$dates),]
  selected_backtest_returns_corrected_positions_m_upd_ref <- dplyr::left_join(selected_backtest_returns_corrected_positions_m_upd_ref,
                                                                              selected_signal_themes_m_d_ref %>% dplyr::select(tickers, theme), by = "tickers")
  selected_backtest_returns_corrected_positions_m_upd_ref$`theme:tickers` <- paste0(selected_backtest_returns_corrected_positions_m_upd_ref$theme, "_", selected_backtest_returns_corrected_positions_m_upd_ref$tickers)

  selected_backtest_returns_corrected_positions_m_upd_ref <-
    selected_backtest_returns_corrected_positions_m_upd_ref[, c("return", "theme",  "market_factor_proxy", "tickers", "theme:tickers")]

  # Copy the data frame
  data_only <- brm_model$data
  # Remove unwanted attributes
  attr(data_only, "terms") <- NULL
  attr(data_only, "drop_unused_levels") <- NULL
  attr(data_only, "data_name") <- NULL
  expect_equal(data_only, selected_backtest_returns_corrected_positions_m_upd_ref)

  # Extract priors set by the user in brm_model
  user_priors_in_model <- subset(brm_model$prior, source == "user")

  # Select relevant columns for comparison
  cols_to_compare <- c("prior", "class", "coef", "group", "source")

  # Sort the data frames
  user_priors_in_model <- user_priors_in_model[, cols_to_compare]

  user_priors_in_model$class[1] <- "cor"
  user_priors_in_model$prior[1] <- "lkj(2)"
  rownames(user_priors_in_model) <- NULL


  elected_priors <- elected_priors[, cols_to_compare]
  rownames(elected_priors) <- NULL

  expect_equal(
    user_priors_in_model %>% dplyr::arrange(dplyr::across(dplyr::everything())),
    elected_priors %>% dplyr::arrange(dplyr::across(dplyr::everything()))
  )

  #Check if MCMC parameters are right
  expect_equal(brm_model$stan_args$control$adapt_delta, 0.98)

  #Check number of rows in predicted_summary
  n_draws <- nrow(results$posterior_draws_summaries$predicted_summary) %>% as.numeric()
  expected_draws <- length(selected_market_factor_proxy_m_xts_upd_ref)*(ncol(selected_backtest_returns_corrected_positions_m_xts_upd_ref) - 1)
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

  future::plan("sequential")


})

test_that("fit_bayesian_hierarchical_model adequately ignores extra-prior when fitting bayesian hierarchical model for model spec 3", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  #get selected info
  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    chosen_signals_and_positions = chosen_signals_and_positions,
    signal_themes_m_df = signal_themes_m_df,
    backtest_returns_m_xts = backtest_returns_m_xts
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_market_factor_proxy_m_xts <- benchmark_returns_m_xts[, c("IBOV")]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  #current info
  current_date <- "2001-07-15"
  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:5), ]

  selected_market_factor_proxy_m_xts_upd_ref <- selected_market_factor_proxy_m_xts[c(1:5), "IBOV"]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date), ]

  priors_m_upd_ref <- priors_m_df[priors_m_df$dates <= current_date,]

  #expected results
  expected_result <- summarize_performance(
    model_structure = "partial_pooled", model_spec_theme_level = "theme_specific_intercept_theme_specific_slope",
    lmer_control = list(lmer_optimizer = "nloptwrap", lmer_optimization_objective = "REML", hierarchical_p_value_method = "Satterthwaite"),
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
    selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref
  )

  #Inside Bayesian Adjustment

  #Create priors for model
  elected_priors <- c(
    # Prior for Value and Mom
    brms::set_prior("normal(0.0012, 0.0016)", class = "b", coef = "themevalue"),
    brms::set_prior("normal(0.0012, 0.0016)", class = "b", coef = "themedefensive"),
    brms::set_prior("normal(0.0025, 0.0016)", class = "b", coef = "thememomentum"),
    brms::set_prior("normal(0.03, 0.002)", class = "b", coef = "themevalue:market_factor_proxy"),
    brms::set_prior("normal(0.0000, 0.004)", class = "b", coef = "thememomentum:market_factor_proxy"),
    brms::set_prior("normal(0.0000, 0.004)", class = "b", coef = "themegrowth:market_factor_proxy"),

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
  suppressWarnings(
  results <- fit_bayesian_hierarchical_model(selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                             selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
                                             signal_universe_m_d_ref = expected_result$signal_universe_m_d_ref,
                                             selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
                                             elected_priors = elected_priors,
                                             model_spec_theme_level = "theme_specific_intercept_theme_specific_slope",
                                             parallel = TRUE,
                                             chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = NA, adapt_delta = 0.98, #MCMC parameters
                                             verbose = TRUE
  )
  )

  brm_model <- results$brm_model

  #check if brm model was fit correctly
  expect_equal(class(brm_model), "brmsfit")
  expect_equal(brm_model$family$family, "gaussian")

  expect_true(all(brm_model$basis$levels$`theme:tickers` %in%
                    (selected_signal_themes_m_d_ref %>% dplyr::mutate(theme_tickers = paste0(theme, "_", tickers)) %>% dplyr::pull(theme_tickers))
  ))

  expect_equal(as.character(brm_model$formula$formula)[c(2,1,3)], c("return", "~", "0 + theme + theme:market_factor_proxy + (1 + market_factor_proxy | theme:tickers)"))

  #Construct data
  selected_backtest_returns_corrected_positions_m_xts_upd_ref$market_factor_proxy <- selected_market_factor_proxy_m_xts_upd_ref
  selected_backtest_returns_corrected_positions_m_upd_ref <- as.data.frame(selected_backtest_returns_corrected_positions_m_xts_upd_ref) %>%
    tibble::rownames_to_column(var = "dates")
  selected_backtest_returns_corrected_positions_m_upd_ref <- reshape2::melt(selected_backtest_returns_corrected_positions_m_upd_ref,
                                                                            id.vars = c("dates", "market_factor_proxy"),
                                                                            variable.name = "tickers", value.name = "return")

  selected_backtest_returns_corrected_positions_m_upd_ref <-
    selected_backtest_returns_corrected_positions_m_upd_ref[order(selected_backtest_returns_corrected_positions_m_upd_ref$dates),]
  selected_backtest_returns_corrected_positions_m_upd_ref <- dplyr::left_join(selected_backtest_returns_corrected_positions_m_upd_ref,
                                                                              selected_signal_themes_m_d_ref %>% dplyr::select(tickers, theme), by = "tickers")
  selected_backtest_returns_corrected_positions_m_upd_ref$`theme:tickers` <- paste0(selected_backtest_returns_corrected_positions_m_upd_ref$theme, "_", selected_backtest_returns_corrected_positions_m_upd_ref$tickers)

  selected_backtest_returns_corrected_positions_m_upd_ref <-
    selected_backtest_returns_corrected_positions_m_upd_ref[, c("return", "theme",  "market_factor_proxy", "tickers", "theme:tickers")]

  # Copy the data frame
  data_only <- brm_model$data
  # Remove unwanted attributes
  attr(data_only, "terms") <- NULL
  attr(data_only, "drop_unused_levels") <- NULL
  attr(data_only, "data_name") <- NULL
  expect_equal(data_only, selected_backtest_returns_corrected_positions_m_upd_ref)

  # Extract priors set by the user in brm_model
  user_priors_in_model <- subset(brm_model$prior, source == "user")

  # Select relevant columns for comparison
  cols_to_compare <- c("prior", "class", "coef", "group", "source")

  # Sort the data frames
  user_priors_in_model <- user_priors_in_model[, cols_to_compare]
  rownames(user_priors_in_model) <- NULL

  #Create priors for model
  corrected_priors <- c(
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

  elected_priors <- corrected_priors[, cols_to_compare]
  rownames(elected_priors) <- NULL


  user_priors_in_model$class[which(user_priors_in_model$class == "L")] <- "cor"
  user_priors_in_model$prior[which(user_priors_in_model$prior == "lkj_corr_cholesky(2)")] <- "lkj(2)"

  # Check if they have the same rows
  expect_equal(
    user_priors_in_model %>% dplyr::arrange(dplyr::across(dplyr::everything())),
    elected_priors %>% dplyr::arrange(dplyr::across(dplyr::everything()))
  )


  #combined <- dplyr::full_join(
  #  elected_priors_sorted %>% dplyr::mutate(source_df = "elected"),
  #  user_priors_in_model_sorted %>% dplyr::mutate(source_df = "user"),
  #  by = c("prior", "class", "coef", "group", "source")
  #)

  # Find rows only in one of the data frames
  #differences <- combined %>% dplyr::filter(is.na(source_df.x) | is.na(source_df.y))

  # Define the expected discrepancies
  #expected_discrepancies <- data.frame(
  #  prior = c("lkj(2)", "lkj_corr_cholesky(2)"),
  #  class = c("cor", "L"),
  #  coef = c("", ""),
  #  group = c("", ""),
  #  source = c("user", "user"),
  #  source_df.x = c("elected", NA_character_),
  #  source_df.y = c(NA_character_, "user"),
  #  stringsAsFactors = FALSE
  #)

  # Check if discrepancies match expected
  #is_okay <- setequal(differences, expected_discrepancies)

  #expect_true(is_okay)

  #Check if MCMC parameters are right
  expect_equal(brm_model$stan_args$control$adapt_delta, 0.98)

  #Check number of rows in predicted_summary
  n_draws <- nrow(results$posterior_draws_summaries$predicted_summary) %>% as.numeric()
  expected_draws <- length(selected_market_factor_proxy_m_xts_upd_ref)*(ncol(selected_backtest_returns_corrected_positions_m_xts_upd_ref) - 1)
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

  future::plan("sequential")

})

test_that("fit_bayesian_hierarchical_model adequately fits a bayesian hierarchical model for model spec 4", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_signal_selection_obj.RData", sep =""))

  chosen_signals_and_positions <- c(Alpha = "long", Beta = "short", Gamma = "long")

  #get selected info
  selected_signals_and_backtest_list <- select_and_correct_signals(
    signals_m_df = signals_m_df,
    signal_themes_m_df = signal_themes_m_df,
    chosen_signals_and_positions = chosen_signals_and_positions,
    backtest_returns_m_xts = backtest_returns_m_xts
  )

  selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
  selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
  selected_market_factor_proxy_m_xts <- benchmark_returns_m_xts[, c("IBOV")]
  selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df

  #current info
  current_date <- "2001-07-15"
  selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[c(1:5), ]

  selected_market_factor_proxy_m_xts_upd_ref <- selected_market_factor_proxy_m_xts[c(1:5), "IBOV"]

  selected_signal_themes_m_d_ref <- selected_signal_themes_m_df[which(selected_signal_themes_m_df$dates == current_date), ]

  priors_m_upd_ref <- priors_m_df[priors_m_df$dates <= current_date,]

  #expected results
  expected_result <- summarize_performance(
    model_structure = "partial_pooled", model_spec_theme_level = "fixed_intercept_fixed_slope",
    lmer_control = list(lmer_optimizer = "nloptwrap", lmer_optimization_objective = "REML", hierarchical_p_value_method = "Satterthwaite"),
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
    selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref
  )

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
  results <- fit_bayesian_hierarchical_model(selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
                                signal_universe_m_d_ref =  expected_result$signal_universe_m_d_ref,
                                selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
                                elected_priors = elected_priors,
                                model_spec_theme_level = "fixed_intercept_fixed_slope",
                                parallel = TRUE,
                                chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = NA, adapt_delta = 0.98, #MCMC parameters
                                verbose = TRUE
  )

  brm_model <- results$brm_model

  #check if brm model was fit correctly
  expect_equal(class(brm_model), "brmsfit")
  expect_equal(brm_model$family$family, "gaussian")

  expect_true(all(brm_model$basis$levels$`theme:tickers` %in%
                    (selected_signal_themes_m_d_ref %>% dplyr::mutate(theme_tickers = paste0(theme, "_", tickers)) %>% dplyr::pull(theme_tickers))
  ))
  expect_equal(as.character(brm_model$formula$formula)[c(2,1,3)], c("return", "~", "market_factor_proxy + (1 + market_factor_proxy | theme:tickers)"))

  #Construct data
  selected_backtest_returns_corrected_positions_m_xts_upd_ref$market_factor_proxy <- selected_market_factor_proxy_m_xts_upd_ref
  selected_backtest_returns_corrected_positions_m_upd_ref <- as.data.frame(selected_backtest_returns_corrected_positions_m_xts_upd_ref) %>%
    tibble::rownames_to_column(var = "dates")

  selected_backtest_returns_corrected_positions_m_upd_ref <- reshape2::melt(selected_backtest_returns_corrected_positions_m_upd_ref,
                                                                            id.vars = c("dates", "market_factor_proxy"),
                                                                            variable.name = "tickers", value.name = "return")

  selected_backtest_returns_corrected_positions_m_upd_ref <-
    selected_backtest_returns_corrected_positions_m_upd_ref[order(selected_backtest_returns_corrected_positions_m_upd_ref$dates),]

  selected_backtest_returns_corrected_positions_m_upd_ref <- dplyr::left_join(selected_backtest_returns_corrected_positions_m_upd_ref,
                                                                              selected_signal_themes_m_d_ref %>% dplyr::select(tickers, theme), by = "tickers")
  selected_backtest_returns_corrected_positions_m_upd_ref$`theme:tickers` <- paste0(selected_backtest_returns_corrected_positions_m_upd_ref$theme, "_", selected_backtest_returns_corrected_positions_m_upd_ref$tickers)

  selected_backtest_returns_corrected_positions_m_upd_ref <- selected_backtest_returns_corrected_positions_m_upd_ref[, c("return", "market_factor_proxy", "theme", "tickers", "theme:tickers")]

  # Copy the data frame
  data_only <- brm_model$data
  # Remove unwanted attributes
  attr(data_only, "terms") <- NULL
  attr(data_only, "drop_unused_levels") <- NULL
  attr(data_only, "data_name") <- NULL
  expect_equal(data_only, selected_backtest_returns_corrected_positions_m_upd_ref)

  # Extract priors set by the user in brm_model
  user_priors_in_model <- subset(brm_model$prior, source == "user")

  # Select relevant columns for comparison
  cols_to_compare <- c("prior", "class", "coef", "group", "source")

  # Sort the data frames
  user_priors_in_model <- user_priors_in_model[, cols_to_compare]

  user_priors_in_model$class[2] <- "cor"
  user_priors_in_model$prior[2] <- "lkj(2)"
  rownames(user_priors_in_model) <- NULL

  elected_priors <- elected_priors[, cols_to_compare]
  rownames(elected_priors) <- NULL

  expect_equal(
    user_priors_in_model %>% dplyr::arrange(dplyr::across(dplyr::everything())),
    elected_priors %>% dplyr::arrange(dplyr::across(dplyr::everything()))
  )

  #Check if MCMC parameters are right
  expect_equal(brm_model$stan_args$control$adapt_delta, 0.98)

  #Check number of rows in predicted_summary
  n_draws <- nrow(results$posterior_draws_summaries$predicted_summary) %>% as.numeric()
  expected_draws <- length(selected_market_factor_proxy_m_xts_upd_ref)*(ncol(selected_backtest_returns_corrected_positions_m_xts_upd_ref) - 1)
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

  future::plan("sequential")

})
