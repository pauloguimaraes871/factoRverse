#' Set Portfolio Weights
#'
#' This function assigns weights to a portfolio based on the specified portfolio construction method. It supports various methods including equal weighting, signal weighting, capitalization weighting, capitalization scaling, risk parity, and mean-tracking error optimization. The function also accommodates additional constraints and policies related to liquidity, turnover, and benchmark weights.
#'
#' @param universe_m_d_ref A data frame containing the current universe of signals, including their associated metrics. The structure of this data frame depends on the portfolio construction method used.
#' @param port_construction_method A character string indicating the method to use for portfolio construction. Supported methods are:
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
#' @param cov_estimation_method An optional character string specifying the method for estimating the covariance matrix. Defaults to \code{NULL}.
#' @param liquidity_constraint_policy An optional list specifying the policy for liquidity constraints. Defaults to \code{NULL}.
#' @param turnover_constraint_policy An optional list specifying the policy for turnover constraints. Defaults to \code{NULL}.
#' @param benchmark_weights_m_d_ref An optional data frame or matrix specifying the benchmark weights for setting sector constraints. Defaults to \code{NULL}.
#' @param n_random_ports An optional numeric value indicating the number of random portfolios to generate for optimization methods. Defaults to \code{NULL}.
#' @param random_ports_method An optional character string specifying the method for risk parity optimization. Defaults to \code{NULL}.
#' @param opt_objective Objective of mean-tracking error optimization. Defaults to \code{NULL}.
#' @param lower_quantile_winsorization An optional numeric value for lower quantile winsorization when handling cap weighting and scaling. Defaults to \code{NULL}.
#' @param upper_quantile_winsorization An optional numeric value for upper quantile winsorization when handling cap weighting and scaling. Defaults to \code{NULL}.
#'
#' @return A data frame or object (depending on the portfolio construction method) with the updated portfolio weights assigned based on the specified method and constraints.
#'
#' @importFrom dplyr %>%
#' @export
set_portfolio_weights <- function(universe_m_d_ref, port_construction_method,
                                  liquidity_m_d_ref = NULL, cap_weighting_metric = NULL, #Cap-Weight and Cap-Scaled
                                  groups_m_d_ref = NULL, #Used for filling returns for covariance matrix estimation and/or setting group constraints
                                  returns_xts_upd_ref = NULL, selected_benchmark_xts_upd_ref = NULL, active_returns = if(is.null(selected_benchmark_xts_upd_ref)) FALSE else TRUE,  #Returns to estimate cov matrix
                                  cov_estimation_method = "sample", cov_matrix_sample_size = if(is.null(returns_xts_upd_ref)) NULL else nrow(returns_xts_upd_ref), #How to estimate covariance matrix?
                                  liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL, #Policies
                                  n_random_ports = 2000, random_ports_method = "sample", opt_objective = "sharpe", opt_method = "random", rp_method = "cyclical-spinu",
                                  custom_weights_m_d_ref = NULL, #Custom Weights
                                  lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975
){

  ##Get eligible tickers
  eligible_tickers <-  universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  #Calculate covariance matrix
  ###################
  ##Check if there is returns data
  if(!is.null(returns_xts_upd_ref)){

    ##Run estimation function
    covariance_matrix <- estimate_covariance_matrix(
      tickers = eligible_tickers, #Eligible universe
      returns_xts_upd_ref = returns_xts_upd_ref, #Return sample
      cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, #Cov estimation
      selected_benchmark_xts_upd_ref = selected_benchmark_xts_upd_ref, #Benchmark for calculating active returns
      groups_m_d_ref = groups_m_d_ref #Groups for correcting NAs
    )
  } else {
    covariance_matrix <- NULL
    if(port_construction_method %in% c("rp", "mvo")){
      stop("Covariance matrix estimation requires returns data.")
    }
  }
  ###################

  #Attach weights to universe_m_d_ref object
  ###################
  port_results_list <- switch( #Chose port construction method
    port_construction_method,

    #Custom-Weighted Portfolio
    custom_weights = create_custom_weighted_portfolio(
      universe_m_d_ref = universe_m_d_ref, #Signal Universe
      custom_weights_m_d_ref = custom_weights_m_d_ref #Custom weights
    ),

    #Equal-Weighted Portfolio
    ew = create_equal_weighted_portfolio(
      universe_m_d_ref = universe_m_d_ref #Signal Universe
    ),

    #Signal-Weighted Portfolio
    sw = create_signal_weighted_portfolio(
      universe_m_d_ref = universe_m_d_ref #Signal Universe
    ),

    #Cap-Weighted Portfolio
    cw = create_cap_weighted_portfolio(
      universe_m_d_ref = universe_m_d_ref,
      liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = cap_weighting_metric, #How to set cap scores
      lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization #Winsorize cap scores
    ),

    #Cap-Scaled Portfolio
    cs = create_cap_scaled_portfolio(
      universe_m_d_ref = universe_m_d_ref, #Signal Universe
      liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = cap_weighting_metric, #How to set cap scores
      lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization #Winsorize cap scores
    ),

    #Risk-Parity Portfolio
    rp = create_risk_parity_portfolio(
      universe_m_d_ref = universe_m_d_ref, #Signal Universe
      covariance_matrix = covariance_matrix,
      rp_method = rp_method, #Risk Parity method
    ),

    #Mean-Variance Optimization
    mvo = create_mvo_portfolio(
      universe_m_d_ref = universe_m_d_ref, #Signal Universe
      covariance_matrix = covariance_matrix,
      liquidity_constraint_policy = liquidity_constraint_policy, #Liquidity constraints
      turnover_constraint_policy = turnover_constraint_policy, #Turnover constraints
      concentration_constraint_policy = concentration_constraint_policy, #Concentration constraints
      groups_m_d_ref = groups_m_d_ref, #Sectors for generate_sector_constraints
      n_random_ports = n_random_ports,  random_ports_method = random_ports_method, opt_objective = opt_objective, opt_method = opt_method #MVO methods
    )

  )
  ###################

  #Get results
  ###################
  universe_m_d_ref <- port_results_list$universe_m_d_ref

  #Calculate relative risk contribution
  if(!is.null(covariance_matrix)){
    relative_risk_contribution_df <- relative_risk_contribution(
      weights = universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(weights), #Weights
      covariance_matrix = covariance_matrix #Covariance
    )

    ##Attach relative risk contribution to universe_m_d_ref object
    universe_m_d_ref <- universe_m_d_ref %>% dplyr::left_join(relative_risk_contribution_df, by = "tickers") %>%
      dplyr::relocate(rel_risk_contr, .before = weights) #Move to the left of weights
    universe_m_d_ref$rel_risk_contr[which(is.na(universe_m_d_ref$rel_risk_contr))] <- 0 #Fill NAs with 0
  }

  #Create PORT Object
    ##Define eligible universe
    if(!port_construction_method == "custom_weights"){
      #For general port_construction_methods, only eligible assets can have weights different from zero.
      eligible_universe_m_d_ref <- universe_m_d_ref %>% dplyr::filter(is_eligible == 1)
    } else {
      #In case of custom weights, it is possible that non-eligible assets have weights (eg. theme_ss_bench_weights)
      eligible_universe_m_d_ref <- universe_m_d_ref %>% dplyr::filter(weights != 0)
    }

    ##Create the s4 obj
    port_obj <- new("port",
                  universe_m_d_ref = suppressWarnings(create_meta_dataframe(universe_m_d_ref %>% dplyr::arrange(id))), ##Re-order according to id
                  port_construction_method = port_construction_method,
                  eligible_assets = eligible_universe_m_d_ref %>% dplyr::pull(tickers),
                  exp_ret_score = if(port_construction_method %in% c("sw", "cs", "mvo")) eligible_universe_m_d_ref %>% dplyr::pull(exp_ret_score) else NULL,
                  covariance_matrix = covariance_matrix,
                  correlation_matrix = if(!is.null(covariance_matrix)) cov2cor(covariance_matrix) else NULL,
                  weights = eligible_universe_m_d_ref %>% dplyr::pull(weights),
                  rel_risk_contr = if(!is.null(covariance_matrix)) eligible_universe_m_d_ref %>% dplyr::pull(rel_risk_contr) else NULL,
                  mvo_port_spec = if(port_construction_method == "mvo") port_results_list$port_spec else NULL,
                  random_port_weights = if(port_construction_method == "mvo" & opt_method == "random") port_results_list$random_portfolios_weights_df %>% dplyr::select(-exp_ret_score) else NULL,
                  ind_max_weights = if(!is.null(concentration_constraint_policy$max_abs_active_individual_weight) && port_construction_method == "mvo") eligible_universe_m_d_ref %>% dplyr::pull(max_weight) else NULL,
                  ind_min_weights = if(!is.null(concentration_constraint_policy$max_abs_active_individual_weight) && port_construction_method == "mvo") eligible_universe_m_d_ref %>% dplyr::pull(min_weight) else NULL,
                  groups = if(!is.null(groups_m_d_ref)) groups_m_d_ref else NULL,
                  port_name = "not_identified"
  )

  #Return portfolio results
  ###################

  return(port_obj)

}
