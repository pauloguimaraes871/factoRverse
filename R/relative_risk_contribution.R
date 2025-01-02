#' Calculate Relative Risk Contribution
#'
#' Computes the relative risk contribution of each asset in a portfolio, given a vector of
#' weights and a covariance matrix
#'
#' @param weights A numeric vector of weights for each asset in the portfolio
#' @param covariance_matrix A numeric matrix representing the covariance matrix of the assets
#'
#' @return A numeric vector of the same length as `weights` with the relative risk contribution of each asset
#' @examples
#' # example code
#' weights <- c(0.2, 0.3, 0.5)
#' covariance_matrix <- matrix(c(0.04, 0.02, 0.01, 0.02, 0.05, 0.03, 0.01, 0.03, 0.06), nrow = 3)
#' relative_risk_contribution(weights, covariance_matrix)
#'
#' @export
relative_risk_contribution <- function(weights, covariance_matrix){

  #Calculate portfolio variance
  portfolio_variance <- as.numeric(t(weights) %*% covariance_matrix %*% weights)

  #Calculate marginal risk contribution
  marg_risk_contribution <- weights * (covariance_matrix %*% weights)

  #Calculate relative risk contribution
  relative_risk_contribution <- marg_risk_contribution / portfolio_variance

  #Adjust format
  relative_risk_contribution <- data.frame(tickers = colnames(covariance_matrix), rel_risk_contr = relative_risk_contribution)
  rownames(relative_risk_contribution) <- NULL

  return(relative_risk_contribution)
}
