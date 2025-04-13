#' Get Priors for Signals Based on Informative Data
#'
#' This function constructs prior distributions for model parameters based on historical data. It is tailored for use in hierarchical Bayesian models where signals (predictors) are grouped thematically. The priors for the intercept, beta coefficient, variance, and intra-group variability are selected using a BIC-based procedure via \code{choose_prior()}.
#'
#' @param priors_m_upd_ref A meta-dataframe containing the latest data to inform the priors. It must include the following columns:
#' \describe{
#'   \item{\code{id}}{Signal or characteristic identifier.}
#'   \item{\code{characteristic}}{Name of the characteristic or signal.}
#'   \item{\code{dates}}{A \code{Date} vector corresponding to observations.}
#'   \item{\code{theme}}{Theme or group identifier used for hierarchical modeling.}
#'   \item{\code{alpha}, \code{beta}}{Point estimates of intercept and slope coefficients.}
#'   \item{\code{sigma}}{Estimate of the residual standard deviation.}
#' }
#' The data should include only values for the most recent date.
#'
#' @param priors_type A string specifying the type of priors to generate. Options include:
#' \describe{
#'   \item{\code{"all"}}{Estimate priors for intercept, beta, group-level variability (standard deviations), and residual sigma.}
#'   \item{\code{"mean"}}{Estimate only priors for intercept and beta.}
#'   \item{\code{"uninformative"}}{Use default non-informative priors (not implemented here, reserved for future use).}
#'   \item{\code{"user"}}{Use user-defined priors (not implemented here, reserved for future use).}
#' }
#'
#' @return A named list of `brms::set_prior` objects, one per theme, each specifying priors for intercept, beta coefficients, and optionally variance and correlation parameters depending on \code{priors_type}.
#'
#' @details
#' The function performs the following:
#' \itemize{
#'   \item Extracts the current date and filters the data accordingly.
#'   \item Groups signals by \code{theme}, treating each theme as a hierarchical group.
#'   \item Computes summary statistics for \code{alpha}, \code{beta}, and \code{sigma}.
#'   \item Calculates signal-specific deviations from theme-level means to model intra-group variability.
#'   \item Fits appropriate prior distributions using the \code{choose_prior()} function.
#'   \item Constructs \code{brms::set_prior} calls, including correlation priors when \code{priors_type = "all"}.
#' }
#'
#' @seealso \code{\link[brms]{set_prior}}, \code{\link{choose_prior}}
get_priors_from_informative_data <- function(priors_m_upd_ref, priors_type){

  #Get dates
  dates <- unique(as.Date(priors_m_upd_ref$dates, format = "%Y-%m-%d"))
  current_date <- dates[which.max(dates)]

  #Get themes
  themes <- unique(priors_m_upd_ref$theme)

  #Get Current Themes Data
  current_themes_data <- priors_m_upd_ref %>% dplyr::filter(dates == current_date) %>%
    dplyr::group_by(theme) %>% #Get information by theme
    #Summarize information by cluster
    dplyr::summarise(grand_mean_alpha = mean(alpha), #alpha ~ N(grand_mean_alpha, grand_sd_alpha)
                     grand_sd_alpha = sd(alpha),
                     grand_mean_beta = mean(beta), #beta ~ N(grand_mean_beta, grand_sd_beta)
                     grande_sd_beta = sd(beta))

  #Set priors for each theme (Signals are Groups)
  ##########################
  themes_priors <- list() #init object
  ##Loop through theme
  for(i in 1:length(themes)){
    current_theme <- themes[i]

    ###Prepare objects
    ##################
    ####Get theme specific data
    specific_theme_data <- priors_m_upd_ref %>% dplyr::filter(theme == current_theme)

    ####Get overall info by dates
    specific_theme_overall_data_by_dates <- specific_theme_data %>%
      dplyr::group_by(dates) %>%
      dplyr::summarise(
        ###Alpha
        overall_alpha_mean_by_dates = mean(alpha, na.rm = TRUE),
        overall_sd_alpha_by_dates = sd(alpha, na.rm = TRUE),
        ###Beta
        overall_beta_mean_by_dates = mean(beta, na.rm = TRUE),
        overall_sd_beta_by_dates = sd(beta, na.rm = TRUE),
        ###Sigma
        overall_sigma_mean_by_dates = mean(sigma, na.rm = TRUE),
        overall_sd_sigma_by_dates = sd(sigma, na.rm = TRUE),
        .groups = "drop"
      )

    ####Get u (each signal time-dependent difference from overall mean)
    specific_theme_data <-
      #Join specific theme data with overall data
      dplyr::left_join(specific_theme_data, specific_theme_overall_data_by_dates, by = "dates") %>%
      #Create u's data (difference from overall_mean)
      dplyr::mutate(alpha_difference_from_overall_mean = alpha - overall_alpha_mean_by_dates) %>%
      dplyr::mutate(beta_difference_from_overall_mean = beta - overall_beta_mean_by_dates) %>%
      dplyr::mutate(sigma_difference_from_overall_mean = sigma - overall_sigma_mean_by_dates)

    ##################

    ###Choose Priors
    #####################
    #Priors for Overall Alpha
    overall_alpha <- unique(specific_theme_data$overall_alpha_mean_by_dates) #Consider one value by date
    overall_alpha_prior <- choose_prior(vector = overall_alpha, parameter_class = "location")

    #Priors for u1
    if(priors_type == "all"){
      ##Create u1 (difference from signal's alpha and mean alpha of the cluster)
      u1 <- specific_theme_data %>% dplyr::group_by(dates) %>% #group by dates
        dplyr::summarise(mean_alpha_difference_from_overall_mean_by_dates = mean(alpha_difference_from_overall_mean), #take mean alpha difference by dates
                         sd_alpha_difference_from_overall_mean_by_dates = sd(alpha_difference_from_overall_mean))  #take dispersion of alpha differences by dates
      ###mu_u1 (expected difference from signal's alpha and mean alpha of the cluster (expect to be zero))
      mu_u1 <- mean(u1$mean_alpha_difference_from_overall_mean_by_dates)
      ###tau_u1 (dispersion in differences from signal's alpha and mean alpha of the cluster (intra-cluster variability))
      tau_u1 <- u1$sd_alpha_difference_from_overall_mean_by_dates
      tau_u1 <- tau_u1[!is.na(tau_u1)] #Remove possible NAs (those might arrive because of existing only one signal in a given dates)
      tau_u1_prior <- choose_prior(vector = tau_u1, parameter_class = "scale")
    }

    #Priors for Overall Beta
    overall_beta <- unique(specific_theme_data$overall_beta_mean_by_dates) #Consider one value by date
    overall_beta <- overall_beta[!is.na(overall_beta)] #NAs might arise in first dates because of division by a NA var
    overall_beta_prior <- choose_prior(vector = overall_beta, parameter_class = "location")

    #Priors for u2
    if(priors_type == "all"){
      ##Create u2 (difference from signal's beta and mean beta of the cluster)
      u2 <- specific_theme_data %>% dplyr::group_by(dates) %>% #group by dates
        dplyr::summarise(mean_beta_difference_from_overall_mean_by_dates = mean(beta_difference_from_overall_mean), #take mean beta difference by dates
                         sd_beta_difference_from_overall_mean_by_dates = sd(beta_difference_from_overall_mean))  #take dispersion of beta differences by dates
      ###mu_u2 (expected difference from signal's beta and mean beta of the cluster (expect to be zero))
      mu_u2 <- mean(u2$mean_beta_difference_from_overall_mean_by_dates, na.rm = TRUE)
      ###tau_u2 (dispersion in differences from signal's beta and mean beta of the cluster (intra-cluster variability))
      tau_u2 <- u2$sd_beta_difference_from_overall_mean_by_dates
      tau_u2 <- tau_u2[!is.na(tau_u2)] #Remove possible NAs (those might arrive because of existing only one signal in a given dates)
      tau_u2_prior <- choose_prior(vector = tau_u2, parameter_class = "scale")
    }

    #Priors for Overall Sigma
    if(priors_type == "all"){
      overall_sigma <- unique(specific_theme_data$overall_sigma_mean_by_dates) #Consider one value by date
      overall_sigma <- overall_sigma[!is.na(overall_sigma)]
      overall_sigma_prior <- choose_prior(vector = log(overall_sigma), parameter_class = "location") #Log because sigma_alpha will enter exponentially

      #Priors for u_sigma
      ##Create u_sigma (difference from signal's sigma and mean sigma)
      u_sigma <- specific_theme_data %>% dplyr::group_by(dates) %>% #group by dates
        dplyr::summarise(mean_sigma_difference_from_overall_mean_by_dates = mean(sigma_difference_from_overall_mean),
                         sd_sigma_difference_from_grand_mean_by_dates = sd(sigma_difference_from_overall_mean))
      ###mu_u_sigma
      mu_u_sigma <- mean(u_sigma$mean_sigma_difference_from_overall_mean_by_dates, na.rm = TRUE)
      ###tau_u_sigma
      tau_u_sigma <- u_sigma$sd_sigma_difference_from_grand_mean_by_dates
      tau_u_sigma <- tau_u_sigma[!is.na(tau_u_sigma)] #Remove possible NAs (those might arrive because of existing only one signal in a given dates)
      tau_u_sigma_prior <- choose_prior(vector = tau_u_sigma, parameter_class = "scale")
    }
    #####################

    ###Set Priors
    themes_priors[[i]] <- c(
      #Intercept Prior
      brms::set_prior(
        switch(overall_alpha_prior$distribution,
               norm = paste0("normal(",overall_alpha_prior$estimated_parameters[1],",", overall_alpha_prior$estimated_parameters[2], ")"),
               t = paste0("student_t(", overall_alpha_prior$estimated_parameters[1], ")")
        ), class = "Intercept"),
      #Beta Prior
      brms::set_prior(
        switch(overall_beta_prior$distribution,
               norm = paste0("normal(",overall_beta_prior$estimated_parameters[1],",", overall_beta_prior$estimated_parameters[2], ")"),
               t = paste0("student_t(", overall_beta_prior$estimated_parameters[1], ")")
        ), class = "b", coef = "bench_return")
    )

    #If priors_type == "all", add tau and sigma priors
    if(priors_type == "all"){
      themes_priors[[i]] <- c(themes_priors[[i]],
                              #Tau U1 Prior
                              brms::set_prior(
                                switch(tau_u1_prior$distribution,
                                       norm = paste0("normal(",tau_u1_prior$estimated_parameters[1],",", tau_u1_prior$estimated_parameters[2], ")"),
                                       t = paste0("student_t(", tau_u1_prior$estimated_parameters[1], ")"),
                                       cauchy = paste0("cauchy(", tau_u1_prior$estimated_parameters[1], ",",tau_u1_prior$estimated_parameters[2], ")"),
                                       invgamma = paste0("inv_gamma(", tau_u1_prior$estimated_parameters[1], ",",tau_u1_prior$estimated_parameters[2], ")"),
                                       lnorm = paste0("lognormal(", tau_u1_prior$estimated_parameters[1], ",",tau_u1_prior$estimated_parameters[2], ")")
                                ), class = "sd", coef = "Intercept", group = "signal"),
                              #Tau U2 Prior
                              brms::set_prior(
                                switch(tau_u2_prior$distribution,
                                       norm = paste0("normal(",tau_u2_prior$estimated_parameters[1],",", tau_u2_prior$estimated_parameters[2], ")"),
                                       t = paste0("student_t(", tau_u2_prior$estimated_parameters[1], ")"),
                                       cauchy = paste0("cauchy(", tau_u2_prior$estimated_parameters[1], ",",tau_u2_prior$estimated_parameters[2], ")"),
                                       invgamma = paste0("inv_gamma(", tau_u2_prior$estimated_parameters[1], ",",tau_u2_prior$estimated_parameters[2], ")"),
                                       lnorm = paste0("lognormal(", tau_u2_prior$estimated_parameters[1], ",",tau_u2_prior$estimated_parameters[2], ")")
                                ), class = "sd", coef = "bench_return", group = "signal"),
                              #Sigma
                              ##Sigma Alpha
                              brms::set_prior(
                                switch(overall_sigma_prior$distribution,
                                       norm = paste0("normal(",overall_sigma_prior$estimated_parameters[1],",", overall_sigma_prior$estimated_parameters[2], ")"),
                                       t = paste0("student_t(", overall_sigma_prior$estimated_parameters[1], ")"),
                                       cauchy = paste0("cauchy(", overall_sigma_prior$estimated_parameters[1], ",",overall_sigma_prior$estimated_parameters[2], ")"),
                                       invgamma = paste0("inv_gamma(", overall_sigma_prior$estimated_parameters[1], ",",overall_sigma_prior$estimated_parameters[2], ")"),
                                       lnorm = paste0("lognormal(", overall_sigma_prior$estimated_parameters[1], ",",overall_sigma_prior$estimated_parameters[2], ")")
                                ), class = "Intercept", dpar = "sigma"),
                              ##Sigma Tau
                              brms::set_prior(
                                switch(tau_u_sigma_prior$distribution,
                                       norm = paste0("normal(",tau_u_sigma_prior$estimated_parameters[1],",", tau_u_sigma_prior$estimated_parameters[2], ")"),
                                       t = paste0("student_t(", tau_u_sigma_prior$estimated_parameters[1], ")"),
                                       cauchy = paste0("cauchy(", tau_u_sigma_prior$estimated_parameters[1], ",",tau_u_sigma_prior$estimated_parameters[2], ")"),
                                       invgamma = paste0("inv_gamma(", tau_u_sigma_prior$estimated_parameters[1], ",",tau_u_sigma_prior$estimated_parameters[2], ")"),
                                       lnorm = paste0("lognormal(", tau_u_sigma_prior$estimated_parameters[1], ",",tau_u_sigma_prior$estimated_parameters[2], ")")
                                ), class = "sd", group = "signal", dpar = "sigma"),
                              #Correlation
                              brms::set_prior("lkj(2)", class = "cor", group = "signal")
      )
    }
  }

  #rename
  names(themes_priors) <- themes
  return(themes_priors)
}
