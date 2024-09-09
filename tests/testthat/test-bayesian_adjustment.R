test_that("bayesian model correctly shrinks alpha based on conservative priors",{

  set.seed(123)
  #Generate sample data
  #Parameters
  overall_alpha <- rnorm(n = 100, mean = 10, sd = 5) #Big SD to make prior stronger
  overall_beta <- rnorm(n = 100, mean = 0, sd = 1)
  tau_u1 <- extraDistr::rhnorm(n = 100, sigma = 1)
  tau_u2 <- extraDistr::rhnorm(n = 100, sigma = 1)
  sigma <- extraDistr::rhnorm(n = 100, sigma = 2)
  bench_return <- rnorm(n = 100, mean = 0, sd = 1)

  #Signal 1 (earnings yield)
  alpha <- overall_alpha + unlist(purrr::map(tau_u1, ~ rnorm(n=1, mean = 0, sd = .x))) #u1 ~ N(0, tau_u1)
  beta <- overall_beta + unlist(purrr::map(tau_u2, ~ rnorm(n=1, mean = 0, sd = .x))) #u2 ~ N(0, tau_u2)

  parameters <- list(mean = alpha + bench_return*(beta), sigma = sigma)

  subj1 <- purrr::map2_dbl(parameters$mean, parameters$sigma, ~ rnorm(n = 1, mean = .x, sd = .y))

  subj1_data = data.frame(characteristic = rep("earnings_yield", length(subj1)),
                          dates = seq.Date(as.Date("2001-04-15", format = "%Y-%m-%d"), by = "month", length.out = length(subj1)),
                          active_return = subj1,
                          month_year = paste0(lubridate::month(seq.Date(as.Date("2001-04-15", format = "%Y-%m-%d"), by = "month", length.out = length(subj1))),"-",
                                              lubridate::year(seq.Date(as.Date("2001-04-15", format = "%Y-%m-%d"), by = "month", length.out = length(subj1)))),
                          theme = "value"
  )

  #Signal 2 (book_yield)
  alpha <- overall_alpha + unlist(purrr::map(tau_u1, ~ rnorm(n=1, mean = 0, sd = .x))) #u1 ~ N(0, tau_u1)
  beta <- overall_beta + unlist(purrr::map(tau_u2, ~ rnorm(n=1, mean = 0, sd = .x))) #u2 ~ N(0, tau_u2)

  parameters <- list(mean = alpha + bench_return*(beta), sigma = sigma)

  subj2 <- purrr::map2_dbl(parameters$mean, parameters$sigma, ~ rnorm(n = 1, mean = .x, sd = .y))

  subj2_data = data.frame(characteristic = rep("book_yield", length(subj1)),
                          dates = seq.Date(as.Date("2001-04-15", format = "%Y-%m-%d"), by = "month", length.out = length(subj1)),
                          active_return = subj2,
                          month_year = paste0(lubridate::month(seq.Date(as.Date("2001-04-15", format = "%Y-%m-%d"), by = "month", length.out = length(subj2))),"-",
                                              lubridate::year(seq.Date(as.Date("2001-04-15", format = "%Y-%m-%d"), by = "month", length.out = length(subj2)))),
                          theme = "value")

  signals_groups_m_d_ref <- data.frame(id = paste0(c("earnings_yield", "book_yield"), "-", "2009-07-15") , tickers = c("earnings_yield", "book_yield"), dates = "2009-07-15", theme = c("value", "value"))

  #bench return
  bench_return_df <- data.frame(dates = subj1_data$dates, bench_return = bench_return)

  #bind data to backtest
  backtest_data_df <- data.frame(dates = subj1_data$dates, earnings_yield = subj1_data$active_return, book_yield = subj2_data$active_return)

  #get signal_universe_m_d_ref
  signal_universe_m_d_ref <- data.frame(id = paste0(c("earnings_yield", "book_yield"),"-", "2009-07-15"), tickers = c("earnings_yield", "book_yield"), dates = "2009-07-15",
                                        mean_active_return = backtest_data_df[,-1] %>% apply(2, function(x) mean(x)),
                                        tracking_error = backtest_data_df[,-1] %>% apply(2, function(x) sd(x)),
                                        IR = backtest_data_df[,-1] %>% apply(2, function(x) mean(x)/sd(x)),
                                        alpha = backtest_data_df[,-1] %>% apply(2, function(x){
                                          summary(lm(x ~ bench_return_df[,2]))$coefficients[1]
                                        }),
                                        AP = backtest_data_df[,-1] %>% apply(2, function(x){
                                          summary(lm(x ~ bench_return_df[,2]))$coefficients[5]
                                        }),
                                        beta = backtest_data_df[,-1] %>% apply(2, function(x){
                                          summary(lm(x ~ bench_return_df[,2]))$coefficients[2]
                                        }),
                                        treynor = backtest_data_df[,-1] %>% apply(2, function(x){
                                          mean(x)/summary(lm(x ~ bench_return_df[,2]))$coefficients[2]
                                        }),
                                        p_value = backtest_data_df[,-1] %>% apply(2, function(x){
                                          summary(lm(x ~ bench_return_df[,2]))$coefficients[7]
                                        }))



  #get priors
  #priors <- set_priors(priors_data = value_data, set_priors_on = "all") #based on data
  value_prior <- c(brms::set_prior("normal(0,1)", class = "Intercept"), brms::set_prior("normal(0,1)", class = "b", coef = "bench_return"))


  #Fit bayesian model
  results <- bayesian_adjustment(selected_signals_backtest_returns_upd_ref = backtest_data_df,
                                 selected_benchmark_returns_upd_ref_vector = bench_return_df$bench_return,
                                 selected_priors_informative_data_m_upd_ref = NULL,
                                 priors_type = "user",
                                 user_priors_list = value_prior,
                                 signals_groups_m_d_ref = signals_groups_m_d_ref)



  #Check expectations
  ##Check that priors is correctly set
  expect_equal(value_prior, results$elected_priors_list)


  bayesian_fit <- results$bayesian_fit_list[[1]]

  ##Check that data is correctly set for model to consume
  expected_data <- rbind(
    data.frame(active_return = subj1_data$active_return, bench_return = bench_return_df$bench_return, signal = "earnings_yield"),
    data.frame(active_return = subj2_data$active_return, bench_return = bench_return_df$bench_return, signal = "book_yield")
  )
  expect_equal(bayesian_fit$data$active_return, expected_data$active_return)
  expect_equal(bayesian_fit$data$bench_return, expected_data$bench_return)
  expect_equal(bayesian_fit$data$signal, expected_data$signal)

  ##Check posteriors
  post_samples <- insight::get_parameters(bayesian_fit, effects = "all")

  post_samples$b_Intercept %>% hist()

  # Plot posterior distributions
  library(bayesplot)
  mcmc_dens(post_samples, pars = c("b_Intercept", "b_bench_return"))

})



