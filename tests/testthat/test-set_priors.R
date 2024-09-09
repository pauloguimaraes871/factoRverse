test_that("set_priors works for location", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-04-15"



  #Subject 2
  #Priors
  overall_alpha <- rnorm(n = 10000, mean = 0, sd = 10)
  overall_beta <- rnorm(n = 10000, mean = 0, sd = 10)
  tau_u1 <- rlnorm(n = 10000, meanlog = 2, sdlog = 2)
  tau_u2 <- rlnorm(n = 10000, meanlog = 2, sdlog = 2)
  sigma <- rlnorm(n = 10000, meanlog = 2, sdlog = 2)

  alpha <- overall_alpha + unlist(purrr::map(tau_u1, ~ rnorm(n=1, mean = 0, sd = .x)))
  beta <- overall_beta + unlist(purrr::map(tau_u2, ~ rnorm(n=1, mean = 0, sd = .x)))

  priors <- list(mean = alpha + 1.0*(beta), sigma = sigma)

  subj2 <- purrr::map2_dbl(priors$mean, priors$sigma, ~ rnorm(n = 1, mean = .x, sd = .y))


  priors_data_2 = data.frame(characteristic = rep("subj2", length(subj2)),
                             dates = seq.Date(as.Date("2001-04-15", format = "%Y-%m-%d"), by = "month", length.out = length(subj2)),
                             active_return = subj2,
                             month_year = paste0(lubridate::month(seq.Date(as.Date("2001-04-15", format = "%Y-%m-%d"), by = "month", length.out = length(subj2))),"-",
                                                 lubridate::year(seq.Date(as.Date("2001-04-15", format = "%Y-%m-%d"), by = "month", length.out = length(subj2)))),
                             theme = "value",
                             alpha = alpha,
                             alpha_se = tau_u1,
                             beta = beta,
                             beta_se = tau_u2,
                             sigma = sigma
  )

  priors_data <- rbind(priors_data_1, priors_data_2)


  set_priors(priors_data = priors_data, set_priors_on = "mean")

})




test_that("set_priors works for location", {

  #Load
  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  current_date <- "2001-04-15"
  priors_m_upd_ref_list <- list(jkp_emerging = priors_m_df_list$jkp_emerging[which(priors_m_df_list$jkp_emerging$dates <= current_date), ])



  set_priors(priors_data = priors_data, set_priors_on = "mean")

})

