#' Choose a prior based on the sample fit to a range of distributions
#'
#' This function estimates parameters for several distributions for a given vector and selects the one that best represents the data based on the Bayesian Information Criterion (BIC).
#' It provides the estimated parameters of the selected distribution and also a comparative plot.
#' The sample used should represent data from a pilot study based on which an informative prior will be constructed in the context of define_signal_eligibility
#'
#' @param vector A numeric vector of data for which the best distribution will be selected.
#' @param parameter_class A character string indicating the type of parameterization. Options are "scale" and "location". This determines which distributions will be considered.
#'
#' @return A list containing:
#' \item{bic}{A matrix of BIC values for each distribution considered, where each row corresponds to a different distribution.}
#' \item{distribution}{A character string indicating the distribution with the minimum BIC.}
#' \item{estimated_parameters}{A numeric vector of estimated parameters for the chosen distribution.}
#' \item{plot}{A ggplot object showing the comparative plot of the data and the chosen distribution.}
#'
#' @importFrom fitdistrplus fitdist gofstat plotdist
#' @importFrom extraDistr dinvgamma pinvgamma
#' @export
choose_prior <- function(vector, parameter_class){

  # Define custom functions for the inverse gamma distribution
  dinvgamma_fit <- function(x, shape, rate) {
    extraDistr::dinvgamma(x, alpha = shape, beta = rate)
  }

  pinvgamma_fit <- function(q, shape, rate) {
    extraDistr::pinvgamma(q, alpha = shape, beta = rate)
  }

  # Get the distribution that better represents the vector based on BIC
  ############
  # Calculate BIC for normal and t distributions
  normal_bic <- try(
    fitdistrplus::gofstat(
      fitdistrplus::fitdist(vector, "norm")
    )$bic
  )  # Normal

  t_bic <- try(
    fitdistrplus::gofstat(
      fitdistrplus::fitdist(vector, "t", start = list(df = 5))
    )$bic
  )  # Student T

  cauchy_bic <- if (parameter_class == "scale") {
    try(
      fitdistrplus::gofstat(
        fitdistrplus::fitdist(vector, "cauchy")
      )$bic,
      silent = TRUE
    )
  } else NA  # Cauchy

  invgamma_bic <- if (parameter_class == "scale") {
    try(
      fitdistrplus::gofstat(
        fitdistrplus::fitdist(
          vector, "invgamma",
          start = list(shape = 2, rate = 2),
          densfun = dinvgamma_fit,
          distr = pinvgamma_fit
        )
      )$bic,
      silent = TRUE
    )
  } else NA  # Inverse Gamma

  lnorm_bic <- if (parameter_class == "scale") {
    try(
      fitdistrplus::gofstat(
        fitdistrplus::fitdist(vector, "lnorm")
      )$bic,
      silent = TRUE
    )
  } else NA  # Lognormal

  # Bind BIC values
  bic <- cbind(
    norm = ifelse(is.numeric(normal_bic), normal_bic, NA),
    t = ifelse(is.numeric(t_bic), t_bic, NA),
    cauchy = ifelse(is.numeric(cauchy_bic), cauchy_bic, NA),
    invgamma = ifelse(is.numeric(invgamma_bic), invgamma_bic, NA),
    lnorm = ifelse(is.numeric(lnorm_bic), lnorm_bic, NA)
  )

  # Chosen Distribution
  vector_distribution <- colnames(bic)[which.min(bic)]
  ############

  # Estimate parameters
  ####################
  # For all possible distributions
  fit_normal <- try(
    fitdistrplus::fitdist(vector, "norm")$estimate
  )  # Normal

  fit_t <- try(
    fitdistrplus::fitdist(vector, "t", start = list(df = 5))$estimate
  )  # Student T

  fit_cauchy <- if (parameter_class == "scale") {
    try(
      fitdistrplus::fitdist(vector, "cauchy")$estimate,
      silent = TRUE
    )
  } else NA  # Cauchy

  fit_invgamma <- if (parameter_class == "scale") {
    try(
      fitdistrplus::fitdist(
        vector, "invgamma",
        start = list(shape = 2, rate = 2),
        densfun = dinvgamma_fit,
        distr = pinvgamma_fit
      )$estimate,
      silent = TRUE
    )
  } else NA  # Inverse Gamma

  fit_lognormal <- if (parameter_class == "scale") {
    try(
      fitdistrplus::fitdist(vector, "lnorm")$estimate,
      silent = TRUE
    )
  } else NA  # Lognormal

  # Create the list of parameters
  estimated_params_list <- list(
    norm = if (is.numeric(fit_normal)) fit_normal else NA,
    t = if (is.numeric(fit_t)) fit_t else NA,
    cauchy = if (is.numeric(fit_cauchy)) fit_cauchy else NA,
    invgamma = if (is.numeric(fit_invgamma)) fit_invgamma else NA,
    lnorm = if (is.numeric(fit_lognormal)) fit_lognormal else NA
  )

  # Get the parameters that minimize BIC
  vector_estimated_parameters <- estimated_params_list[[vector_distribution]] %>% round(5)

  ####################

  # Plot comparative with chosen distribution
  if (vector_distribution == "invgamma") {
    # Use custom density and distribution functions for plotting
    comparative_plot <- fitdistrplus::plotdist(
      data = vector,
      distr = dinvgamma_fit,
      para = as.list(vector_estimated_parameters)
    )
  } else {
    comparative_plot <- fitdistrplus::plotdist(
      data = vector,
      distr = vector_distribution,
      para = as.list(vector_estimated_parameters)
    )
  }

  # Return data
  priors_choice <- list(
    bic = bic,
    distribution = vector_distribution,
    estimated_parameters = vector_estimated_parameters,
    plot = comparative_plot
  )

  return(priors_choice)
}
