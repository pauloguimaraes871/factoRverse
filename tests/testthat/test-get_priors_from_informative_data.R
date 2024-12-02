test_that("get_priors_from_informative_data works for mean", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-07-15"

  priors_m_upd_ref <- priors_m_df[priors_m_df$dates <= current_date,]

  set.seed(123)
  #This is alpha for the value theme across time
  value_theme_priors <- priors_m_upd_ref[priors_m_upd_ref$theme == "value",]
  overall_alpha_value_ts <- value_theme_priors %>% dplyr::group_by(dates) %>%
    dplyr::summarise(overall_alpha = mean(alpha)) #Calculate mean of alphas by time
  #This is beta for the value theme across time
    overall_beta_value_ts <- value_theme_priors %>% dplyr::group_by(dates) %>%
    dplyr::summarise(overall_beta = mean(beta)) #Calculate mean of alphas by time
  #This is sigma for the value theme across time
  overall_sigma_value_ts <- value_theme_priors %>% dplyr::group_by(dates) %>%
    dplyr::summarise(overall_sigma = mean(sigma, na.rm = TRUE)) #Calculate mean of alphas by time


  #Pick priors
  overall_alpha_priors <- choose_prior(overall_alpha_value_ts$overall_alpha, parameter_class = "location")
  #Try a manual version
  overall_alpha_priors_manual_bic <- list(
    fitdistrplus::gofstat(fitdistrplus::fitdist(
      overall_alpha_value_ts$overall_alpha, "norm")),
    fitdistrplus::gofstat(fitdistrplus::fitdist(
      overall_alpha_value_ts$overall_alpha, "t", start = list(df = 5)))
  )
  #Priors for overall alpha
  best_alpha_distribution <- c("norm", "t")[which.min(sapply(overall_alpha_priors_manual_bic, function(x) x$bic))]
  best_alpha_pars <- fitdistrplus::fitdist(overall_alpha_value_ts$overall_alpha, best_alpha_distribution)

  #Priors for beta
  overall_beta_priors <- choose_prior(overall_beta_value_ts$overall_beta[-1], parameter_class = "location")
  #Priors for sigma
  overall_sigma_priors <- choose_prior(log(overall_sigma_value_ts$overall_sigma[-c(1,2)]), parameter_class = "scale")

  #Get differences from u1 to overall alpha
  u1 <- dplyr::left_join(value_theme_priors, overall_alpha_value_ts, by = "dates") %>%
    dplyr::mutate(diff = alpha - overall_alpha) %>%
    dplyr::select(id, dates, diff)

  tau_u1 <- u1 %>% dplyr::group_by(dates) %>%
    dplyr::summarise(tau = sd(diff))


  tau_u1_priors <- choose_prior(tau_u1$tau[c(6:9)],
                                parameter_class = "scale")

  tau_u1_priors_manual_bic <- list(
      fitdistrplus::gofstat(fitdistrplus::fitdist(
        tau_u1$tau[c(6:9)], "norm")),
      fitdistrplus::gofstat(fitdistrplus::fitdist(
        tau_u1$tau[c(6:9)], "t", start = list(df=5))),
      fitdistrplus::gofstat(fitdistrplus::fitdist(
        tau_u1$tau[c(6:9)], "cauchy")),
      fitdistrplus::gofstat(fitdistrplus::fitdist(
        tau_u1$tau[c(6:9)], "lnorm"))
  )

  try(tau_u1_priors_manual_bic[[5]] <-
  fitdistrplus::gofstat(fitdistrplus::fitdist(
    tau_u1$tau[c(6:9)], "invgamma", start = list(alpha = 2, beta = 2)))
  , silent = TRUE)

  #Now for beta
  u2 <- dplyr::left_join(value_theme_priors, overall_beta_value_ts, by = "dates") %>%
    dplyr::mutate(diff = beta - overall_beta) %>%
    dplyr::select(id, dates, diff)

  tau_u2 <- u2 %>% dplyr::group_by(dates) %>%
    dplyr::summarise(tau = sd(diff))

  tau_u2_priors <- choose_prior(tau_u2$tau[c(6:9)],
                                parameter_class = "scale")

  #Now for sigma
  u_sigma <- dplyr::left_join(value_theme_priors, overall_sigma_value_ts, by = "dates") %>%
    dplyr::mutate(diff = sigma - overall_sigma) %>%
    dplyr::select(id, dates, diff)

  tau_u_sigma <- u_sigma %>% dplyr::group_by(dates) %>%
    dplyr::summarise(tau = sd(diff))

  tau_u2_sigma <- choose_prior(tau_u_sigma$tau[c(7:9)],
                                parameter_class = "scale")


  #Actual result only for mean
  result <- get_priors_from_informative_data(priors_m_upd_ref, priors_type = "mean")

  #Expected result for value
  value_priors <- c(brms::set_prior("normal(-0.00588,0.11452)", class = "Intercept"),
                    brms::set_prior("normal(0.05463,0.07403)", class = "b", coef = "bench_return"))

  #Expected result for value
  expect_equal(result$value, value_priors)

  #Expected result for chosen priors
  expect_equal(as.numeric(unlist(best_alpha_pars)[1]),
               as.numeric(unlist(regmatches(result$value$prior,
               gregexpr("-?\\d+\\.\\d+", result$value$prior))))[1],
               tolerance = 1e-3)

  expect_equal(as.numeric(unlist(best_alpha_pars)[2]),
               as.numeric(unlist(regmatches(result$value$prior,
                                            gregexpr("-?\\d+\\.\\d+", result$value$prior))))[2],
               tolerance = 1e-3)

})


test_that("get_priors_from_informative_data works for all", {

  #Create signals_m_d_ref_test
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-07-15"

  priors_m_upd_ref <- priors_m_df[priors_m_df$dates <= current_date,]

  set.seed(123)
  #This is alpha for the momentum theme across time
  momentum_theme_priors <- priors_m_upd_ref[priors_m_upd_ref$theme == "momentum",]
  overall_alpha_momentum_ts <- momentum_theme_priors %>% dplyr::group_by(dates) %>%
    dplyr::summarise(overall_alpha = mean(alpha)) #Calculate mean of alphas by time
  #This is beta for the momentum theme across time
  overall_beta_momentum_ts <- momentum_theme_priors %>% dplyr::group_by(dates) %>%
    dplyr::summarise(overall_beta = mean(beta)) #Calculate mean of alphas by time
  #This is sigma for the momentum theme across time
  overall_sigma_momentum_ts <- momentum_theme_priors %>% dplyr::group_by(dates) %>%
    dplyr::summarise(overall_sigma = mean(sigma, na.rm = TRUE)) #Calculate mean of alphas by time


  #Pick priors
  overall_alpha_priors <- choose_prior(overall_alpha_momentum_ts$overall_alpha, parameter_class = "location")
  #Try a manual version
  overall_alpha_priors_manual_bic <- list(
    fitdistrplus::gofstat(fitdistrplus::fitdist(
      overall_alpha_momentum_ts$overall_alpha, "norm")),
    fitdistrplus::gofstat(fitdistrplus::fitdist(
      overall_alpha_momentum_ts$overall_alpha, "t", start = list(df = 5)))
  )
  #Priors for overall alpha
  best_alpha_distribution <- c("norm", "t")[which.min(sapply(overall_alpha_priors_manual_bic, function(x) x$bic))]
  best_alpha_pars <- fitdistrplus::fitdist(overall_alpha_momentum_ts$overall_alpha, best_alpha_distribution)

  #Priors for beta
  overall_beta_priors <- choose_prior(overall_beta_momentum_ts$overall_beta[-1], parameter_class = "location")
  #Priors for sigma
  overall_sigma_priors <- choose_prior(log(overall_sigma_momentum_ts$overall_sigma[-c(1,2)]), parameter_class = "location")

  #Get differences from u1 to overall alpha
  u1 <- dplyr::left_join(momentum_theme_priors, overall_alpha_momentum_ts, by = "dates") %>%
    dplyr::mutate(diff = alpha - overall_alpha) %>%
    dplyr::select(id, dates, diff)

  tau_u1 <- u1 %>% dplyr::group_by(dates) %>%
    dplyr::summarise(tau = sd(diff))


  tau_u1_priors <- choose_prior(tau_u1$tau[c(6:9)],
                                parameter_class = "scale")

  tau_u1_priors_manual_bic <- list(
    fitdistrplus::gofstat(fitdistrplus::fitdist(
      tau_u1$tau[c(6:9)], "norm")),
    fitdistrplus::gofstat(fitdistrplus::fitdist(
      tau_u1$tau[c(6:9)], "t", start = list(df=5))),
    fitdistrplus::gofstat(fitdistrplus::fitdist(
      tau_u1$tau[c(6:9)], "cauchy")),
    fitdistrplus::gofstat(fitdistrplus::fitdist(
      tau_u1$tau[c(6:9)], "lnorm"))
  )

  try(tau_u1_priors_manual_bic[[5]] <-
        fitdistrplus::gofstat(fitdistrplus::fitdist(
          tau_u1$tau[c(6:9)], "invgamma", start = list(alpha = 2, beta = 2)))
      , silent = TRUE)

  #Priors for tau_u1
  best_tau_u1_distribution <- c("norm", "t", "cauchy", "lnorm", "invgamma")[which.min(sapply(tau_u1_priors_manual_bic, function(x) x$bic))]
  best_tau_u1_pars <- fitdistrplus::fitdist(tau_u1$tau[c(6:9)], best_tau_u1_distribution)

  #Now for beta
  u2 <- dplyr::left_join(momentum_theme_priors, overall_beta_momentum_ts, by = "dates") %>%
    dplyr::mutate(diff = beta - overall_beta) %>%
    dplyr::select(id, dates, diff)

  tau_u2 <- u2 %>% dplyr::group_by(dates) %>%
    dplyr::summarise(tau = sd(diff))

  tau_u2_priors <- choose_prior(tau_u2$tau[c(6:9)],
                                parameter_class = "scale")

  #Now for sigma
  u_sigma <- dplyr::left_join(momentum_theme_priors, overall_sigma_momentum_ts, by = "dates") %>%
    dplyr::mutate(diff = sigma - overall_sigma) %>%
    dplyr::select(id, dates, diff)

  tau_u_sigma <- u_sigma %>% dplyr::group_by(dates) %>%
    dplyr::summarise(tau = sd(diff))

  tau_u_sigma <- choose_prior(tau_u_sigma$tau[c(7:9)],
                               parameter_class = "scale")


  #Actual result only for mean
  result <- get_priors_from_informative_data(priors_m_upd_ref, priors_type = "all")

  #Expected result for momentum
  momentum_priors <- c(
    brms::set_prior("normal(-0.00588,0.11452)", class = "Intercept"),
    brms::set_prior("normal(0.05463,0.07403)", class = "b", coef = "bench_return"),

    brms::set_prior("lognormal(-3.53436,1.62204)", class = "sd", coef = "Intercept", group = "signal"),
    brms::set_prior("lognormal(-2.89018,0.65209)", class = "sd", coef = "bench_return", group = "signal"),

    brms::set_prior("normal(-0.0217,0.43376)", class = "Intercept", dpar = "sigma"),
    brms::set_prior("lognormal(-1.57163,1.35422)", class = "sd", dpar = "sigma", group = "signal"),

    brms::set_prior("lkj(2)", class = "cor", group = "signal")
  )

  #Expected result for momentum
  expect_equal(result$momentum, momentum_priors)

  #Expected result for chosen priors
  expect_equal(as.numeric(unlist(best_tau_u1_pars)[1]),
               as.numeric(unlist(regmatches(result$momentum$prior,
                                            gregexpr("-?\\d+\\.\\d+", result$momentum$prior))))[5],
               tolerance = 1e-3)

  expect_equal(as.numeric(unlist(best_tau_u1_pars)[2]),
               as.numeric(unlist(regmatches(result$momentum$prior,
                                            gregexpr("-?\\d+\\.\\d+", result$momentum$prior))))[6],
               tolerance = 1e-3)

})
