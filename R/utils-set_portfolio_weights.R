#' Set Portfolio Weights
#'
#' This function assigns weights to a portfolio based on the specified portfolio construction method. It supports various methods including equal weighting, signal weighting, capitalization weighting, capitalization scaling, risk parity, and mean-tracking error optimization. The function also accommodates additional constraints and policies related to liquidity, turnover, and benchmark weights.
#'
#' @param universe_m_d_ref A data frame containing the current universe of signals, including their associated metrics. The structure of this data frame depends on the portfolio construction method used.
#' @param portfolio_construction_method A character string indicating the method to use for portfolio construction. Supported methods are:
#' \describe{
#'   \item{\code{EW}}{Equal-Weighted Portfolio}
#'   \item{\code{SW}}{Signal-Weighted Portfolio}
#'   \item{\code{CW}}{Cap-Weighted Portfolio}
#'   \item{\code{CS}}{Cap-Scaled Portfolio}
#'   \item{\code{RP}}{Risk-Parity Portfolio}
#'   \item{\code{MTO}}{Mean-Tracking Error Optimization}
#' }
#' @param liquidity_m_d_ref An optional data frame or matrix containing liquidity metrics for the signals, used for capitalization weighting and scaling. Defaults to \code{NULL}.
#' @param cap_weighting_metric An optional character string specifying the metric to use for capitalization weighting or scaling. Defaults to \code{NULL}.
#' @param groups_m_d_ref An optional data frame used for group constraints and covariance matrix estimation. Should include group information if used. Defaults to \code{NULL}.
#' @param covariance_estimation_method An optional character string specifying the method for estimating the covariance matrix. Defaults to \code{NULL}.
#' @param liquidity_constraint_policy An optional list specifying the policy for liquidity constraints. Defaults to \code{NULL}.
#' @param turnover_constraint_policy An optional list specifying the policy for turnover constraints. Defaults to \code{NULL}.
#' @param benchmark_weights_m_d_ref An optional data frame or matrix specifying the benchmark weights for setting sector constraints. Defaults to \code{NULL}.
#' @param n_random_portfolios An optional numeric value indicating the number of random portfolios to generate for optimization methods. Defaults to \code{NULL}.
#' @param rp_method An optional character string specifying the method for risk parity optimization. Defaults to \code{NULL}.
#' @param mto_port_objective Objective of mean-tracking error optimization. Defaults to \code{NULL}.
#' @param lower_quantile_winsorization An optional numeric value for lower quantile winsorization when handling cap weighting and scaling. Defaults to \code{NULL}.
#' @param upper_quantile_winsorization An optional numeric value for upper quantile winsorization when handling cap weighting and scaling. Defaults to \code{NULL}.
#'
#' @return A data frame or object (depending on the portfolio construction method) with the updated portfolio weights assigned based on the specified method and constraints.
#'
#' @importFrom dplyr %>%
#' @export
set_portfolio_weights <- function(universe_m_d_ref, portfolio_construction_method,
                                  liquidity_m_d_ref = NULL, cap_weighting_metric = NULL, #Cap-Weight and Cap-Scaled
                                  groups_m_d_ref = NULL, #Used for filling returns for covariance matrix estimation and/or setting group constraints
                                  returns_upd_ref = NULL, covariance_matrix_sample_size = NULL, #Returns to estimate cov matrix
                                  covariance_estimation_method = NULL, #How to estimate covariance matrix?
                                  liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL, #Policies
                                  n_random_portfolios = 2000, rp_method = "sample", mto_port_objective = "IR",
                                  lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975
){

  #Attach weights to universe_m_d_ref object
  universe_m_d_ref <- switch( #Chose port construction method
    portfolio_construction_method,

    #Equal-Weighted Portfolio
    EW = create_equal_weighted_portfolio(
      universe_m_d_ref = universe_m_d_ref #Signal Universe
    ),

    #Signal-Weighted Portfolio
    SW = create_signal_weighted_portfolio(
      universe_m_d_ref = universe_m_d_ref #Signal Universe
    ),

    #Cap-Weighted Portfolio
    CW = create_cap_weighted_portfolio(
      universe_m_d_ref = universe_m_d_ref,
      liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = cap_weighting_metric, #How to set cap scores
      lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization #Winsorize cap scores
    ),

    #Cap-Scaled Portfolio
    CS = create_cap_scaled_portfolio(
      universe_m_d_ref = universe_m_d_ref, #Signal Universe
      liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = cap_weighting_metric, #How to set cap scores
      lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization #Winsorize cap scores
    ),

    #Risk-Parity Portfolio
    RP = create_risk_parity_portfolio(
      universe_m_d_ref = universe_m_d_ref, #Signal Universe
      returns_upd_ref = returns_upd_ref, groups_m_d_ref = groups_m_d_ref, #Daily Returns
      covariance_matrix_sample_size = covariance_matrix_sample_size, covariance_estimation_method = covariance_estimation_method #How to estimate cov?
    ),

    #Mean-Tracking Error Optimization
    MTO = create_mto_portfolio(
      universe_m_d_ref = universe_m_d_ref, #Signal Universe
      returns_upd_ref = returns_upd_ref,
      covariance_matrix_sample_size = covariance_matrix_sample_size,
      covariance_estimation_method = covariance_estimation_method,
      liquidity_constraint_policy = liquidity_constraint_policy, #Liquidity constraints
      turnover_constraint_policy = turnover_constraint_policy, #Turnover constraints
      concentration_constraint_policy = concentration_constraint_policy, #Concentration constraints
      groups_m_d_ref = groups_m_d_ref, #Sectors for generate_sector_constraints
      n_random_portfolios = n_random_portfolios,  rp_method = rp_method,  mto_port_objective = mto_port_objective #MTO methods
    )

  )

  return(universe_m_d_ref)

}
