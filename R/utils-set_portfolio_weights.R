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
#' @param covariance_matrix An optional covariance matrix for the eligible tickers. If \code{NULL}, the function will estimate the covariance matrix using the provided return data. Defaults to \code{NULL}.
#' @param cov_estimation_method An optional character string specifying the method for estimating the covariance matrix. Defaults to \code{NULL}.
#' @param liquidity_constraint_policy An optional list specifying the policy for liquidity constraints. Defaults to \code{NULL}.
#' @param turnover_constraint_policy An optional list specifying the policy for turnover constraints. Defaults to \code{NULL}.
#' @param eligible_returns_m_xts_upd_ref An optional `xts` object containing return data for the eligible tickers, used in covariance matrix estimation for Risk-Parity and MVO methods.
#' @param selected_benchmark_m_xts_upd_ref An optional `xts` object containing benchmark returns used to compute active returns (only if `active_returns = TRUE`).
#' @param active_returns Logical. If `TRUE`, covariance estimation will use active returns (asset returns minus benchmark). Defaults to `FALSE` if `selected_benchmark_m_xts_upd_ref` is `NULL`, otherwise `TRUE`.
#' @param cov_matrix_sample_size Integer. Number of time periods (rows in `eligible_returns_m_xts_upd_ref`) used to estimate the covariance matrix. If `NULL`, uses all available observations.
#' @param concentration_constraint_policy Optional list specifying concentration constraints for MVO optimization. Typically includes `max_abs_active_individual_weight` or other individual/sector limits.
#' @param ridge_pen Numeric. Ridge penalty for MVO optimization to improve numerical stability. Defaults to `NULL`.
#' @param opt_method Character. Optimization method for MVO. Defaults to `"random"` and can include methods like `"grid"`, `"bayesian"`, or `"differential_evolution"`.
#' @param rp_method Character. Method to compute the Risk Parity portfolio. Defaults to `"cyclical-spinu"`.
#' @param exp_ret_score_tilt Character argument specififying whether tilt must be applied during of after risk-parity weights
#' @param exp_ret_score_tilt_eta  Numeric. The intensity of the tilt effect when using `exp_ret_score_tilt`. Higher values increase the tilt effect.
#' @param linkage Character. Linkage method for hierarchical clustering in Risk Parity. Defaults to `"single"`.
#' @param custom_weights_m_d_ref A meta dataframe containing custom user-defined weights. Required when `port_construction_method = "custom_weights"`. Must contain columns `tickers`, `dates`, and `weights`.
#' @param n_random_ports An optional numeric value indicating the number of random portfolios to generate for optimization methods. Defaults to \code{NULL}.
#' @param random_ports_method An optional character string specifying the method for risk parity optimization. Defaults to \code{NULL}.
#' @param opt_objective Objective of mean-tracking error optimization. Defaults to \code{NULL}.
#' @param n_resamples Number of resamples for resampled MVO. Defaults to \code{0}.
#' @param exp_ret_score_jitter Numeric. Standard deviation of jitter added to expected return
#' scores during resampling. Defaults to \code{0}.
#' @param cov_eigval_jitter Numeric. Standard deviation of jitter added to covariance
#' matrix eigenvalues during resampling. Defaults to \code{0}.
#' @param parallel Logical. If `TRUE`, enables parallel processing for computationally intensive tasks. Defaults to `FALSE`.
#' @param mmaf_method Character. Method for Micro-Macro Allocation Framework. Options are `"bottom_up"` or `"top_down"`. Defaults to `"bottom_up"`.
#' @param top_down_proxy_port_method Character. Method for constructing the top-down proxy portfolio in MMAF. Options are `"ew"`, `"sw"`, `"cw"`, `"cs"`, `"rp"`, or `"mvo"`.
#' @param mmaf_group_col Character. Column name in `groups_m_d_ref` used to define groups for MMAF.
#' @param micro_port_construction_method Character micro method used to allocate within groups (e.g., `"ew"`, `"rp"`, `"hrp"`, `"mvo"`).
#' @param macro_port_construction_method Character macro method used to allocate across groups
#'   (e.g., `"ew"`, `"rp"`, `"hrp"`, `"mvo"`). For a strictly *neutral* top-down
#'   sector allocation, prefer `"ew"`, `"rp"` or `"hrp"` and keep `macro_exp_ret_score_tilt = NULL`.
#' @param macro_linkage Character, passed to macro hierarchical methods when applicable.
#' @param macro_concentration_constraint_policy Optional list with group-level weight caps.
#' @param macro_cap_weighting_metric Optional character for cap-based macro weighting (if used).
#' @param macro_n_random_ports Integer, number of random portfolios at macro level.
#' @param macro_random_ports_method Character, sampling method (macro).
#' @param macro_opt_objective Character, optimization objective at macro.
#' @param macro_opt_method Character, optimizer selector at macro.
#' @param macro_ridge_pen Numeric or `NULL`, ridge penalty for macro MVO.
#' @param macro_n_resamples Integer, number of resamples for macro MVO.
#' @param macro_exp_ret_score_jitter Numeric, jitter on sector ER (macro MVO).
#' @param macro_cov_eigval_jitter Numeric, jitter on macro covariance eigenvalues.
#' @param macro_rp_method Character, risk parity method at macro.
#' @param macro_exp_ret_score_tilt Optional numeric vector/column name for RP tilt (macro).
#' @param macro_exp_ret_score_tilt_eta Optional numeric, tilt intensity for macro RP.
#' @param lower_quantile_winsorization An optional numeric value for lower quantile winsorization when handling cap weighting and scaling. Defaults to \code{NULL}.
#' @param upper_quantile_winsorization An optional numeric value for upper quantile winsorization when handling cap weighting and scaling. Defaults to \code{NULL}.
#' @param selected_benchmark It controls whether a 'port' object for the benchmark should be created and added to portolio results.
#' @param bench_assets_returns_m_xts_upd_ref Optional. A 'xts' object containing returns for all stocks. Needed for computing benchmark and port stats.
#' @param level Character. Level of the portfolio object. Options are `"port"`, `"benchmark"`, or `"group"`. Defaults to `"port"`.
#' Helps in assessing which type of portfolio is being created.
#' @return A data frame or object (depending on the portfolio construction method) with the updated portfolio weights assigned based on the specified method and constraints.
#'
#' @export
set_portfolio_weights <- function(universe_m_d_ref, port_construction_method,
                                  liquidity_m_d_ref = NULL, cap_weighting_metric = NULL, #Cap-Weight and Cap-Scaled
                                  groups_m_d_ref = NULL, #Used for filling returns for covariance matrix estimation and/or setting group constraints
                                  covariance_matrix = NULL,
                                  eligible_returns_m_xts_upd_ref = NULL, selected_benchmark_m_xts_upd_ref = NULL, active_returns = if(is.null(selected_benchmark_m_xts_upd_ref)) FALSE else TRUE,  #Returns to estimate cov matrix
                                  cov_estimation_method = "sample", cov_matrix_sample_size = if(is.null(eligible_returns_m_xts_upd_ref)) NULL else nrow(eligible_returns_m_xts_upd_ref), #How to estimate covariance matrix?
                                  liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL, #Policies
                                  n_random_ports = 2000, random_ports_method = "sample", opt_objective = "sharpe", opt_method = "random", ridge_pen = NULL,
                                  n_resamples = 0, exp_ret_score_jitter = 0, cov_eigval_jitter = 0, #MVO
                                  rp_method = "cyclical-spinu", exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
                                  linkage = "single", #Risk Parity
                                  custom_weights_m_d_ref = NULL, #Custom Weights
                                  ## MMAF
                                  mmaf_method = "bottom_up", top_down_proxy_port_method, mmaf_group_col,
                                  micro_port_construction_method = NULL, #Micro portfolio construction method
                                  macro_port_construction_method = NULL, macro_concentration_constraint_policy = NULL,
                                  macro_cap_weighting_metric = NULL, macro_n_random_ports = 2000, macro_random_ports_method = "sample",
                                  macro_opt_objective = "sharpe", macro_opt_method = "random", macro_ridge_pen = NULL,
                                  macro_n_resamples = 0, macro_exp_ret_score_jitter = 0, macro_cov_eigval_jitter = 0,
                                  macro_rp_method = "cyclical-spinu", macro_exp_ret_score_tilt = NULL,  macro_exp_ret_score_tilt_eta = NULL,
                                  macro_linkage = "single",
                                  ## Benchmark Port Obj
                                  selected_benchmark = NULL, bench_assets_returns_m_xts_upd_ref = NULL,
                                  level = "port",
                                  lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975, parallel = FALSE, verbose = TRUE

){

  if (isTRUE(verbose) && level == "port"){
    cat("\n")
    cat(crayon::cyan("Computing portfolio..."))
    tictoc::tic()
  }

    ##Get eligible tickers
  if (port_construction_method == "custom_weights"){
    ### For custom_weights, eligible tickers are those with weights > 0 in custom_weights_m_d_ref
    ### Thus positive weights dominates eligibility from classify_investment_universe
    ### For sub level == "group", all should be considered eligible, though
    if (level == "group") {
      eligible_tickers <- universe_m_d_ref %>% dplyr::pull(tickers)
    } else {
      eligible_tickers <- universe_m_d_ref %>%
        dplyr::left_join(custom_weights_m_d_ref %>%
                           dplyr::select(id, weights), by = "id") %>%
        dplyr::filter(weights > 0) %>%
        dplyr::pull(tickers)
    }
  } else {
    eligible_tickers <-  universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
  }

  #Calculate covariance matrix
  ###################
  ##Check if cov matrix is already provided
  if (is.null(covariance_matrix)){
    ###Check if there is returns data
    if(!is.null(eligible_returns_m_xts_upd_ref)){

      ####Run estimation function
      covariance_matrix <- estimate_covariance_matrix(
        tickers = eligible_tickers, #Eligible universe
        returns_m_xts_upd_ref = eligible_returns_m_xts_upd_ref, #Return sample
        cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, #Cov estimation
        selected_benchmark_m_xts_upd_ref = selected_benchmark_m_xts_upd_ref, #Benchmark for calculating active returns
        groups_m_d_ref = groups_m_d_ref #Groups for correcting NAs
      )
    } else {
      covariance_matrix <- NULL
      if(port_construction_method %in% c("rp", "hrp", "mvo", "mmaf")){
        stop("Covariance matrix estimation requires returns data.")
      }
    }

  }

  ### Check if rownames match eligible tickers
  if (!is.null(covariance_matrix)){
    if(!identical(eligible_tickers, rownames(covariance_matrix)) ||
       !identical(eligible_tickers, colnames(covariance_matrix))){
      stop("Provided covariance matrix rownames/colnames do not match eligible tickers.")
    }

    ### Check if there are any NAs in rownames/colnames or NULLs
    if(any(is.na(rownames(covariance_matrix))) || any(is.na(colnames(covariance_matrix))) ||
       any(rownames(covariance_matrix) == "") || any(colnames(covariance_matrix) == "") ||
       is.null(rownames(covariance_matrix)) || is.null(colnames(covariance_matrix))){
      stop("Provided covariance matrix rownames/colnames contain NAs or NULLs.")
    }
  }

  ###################

  #Attach weights to universe_m_d_ref object
  ###################

  ## Create portfolio according to method
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
      exp_ret_score_tilt = exp_ret_score_tilt, #Tilt by exp_ret_score
      exp_ret_score_tilt_eta = exp_ret_score_tilt_eta, #Tilt intensity
      liquidity_constraint_policy = liquidity_constraint_policy, #Liquidity constraints
      turnover_constraint_policy = turnover_constraint_policy, #Turnover constraints
      concentration_constraint_policy = concentration_constraint_policy #Concentration constraints
    ),

    #Hierarchical Risk Parity
    hrp = create_hrp_portfolio(
      universe_m_d_ref = universe_m_d_ref, #Signal Universe
      covariance_matrix = covariance_matrix,
      linkage = linkage, #Linkage method
      exp_ret_score_tilt = exp_ret_score_tilt, #Tilt by exp_ret_score
      exp_ret_score_tilt_eta = exp_ret_score_tilt_eta #Tilt intensity
    ),

    #Mean-Variance Optimization
    mvo = create_resampled_mvo_portfolio(
      universe_m_d_ref = universe_m_d_ref, #Signal Universe
      covariance_matrix = covariance_matrix,
      liquidity_constraint_policy = liquidity_constraint_policy, #Liquidity constraints
      turnover_constraint_policy = turnover_constraint_policy, #Turnover constraints
      concentration_constraint_policy = concentration_constraint_policy, #Concentration constraints
      groups_m_d_ref = if (level == "sub_port") NULL else groups_m_d_ref, #Sectors for generate_sector_constraints
      n_random_ports = n_random_ports,  random_ports_method = random_ports_method, opt_objective = opt_objective, opt_method = opt_method, #MVO methods
      ridge_pen = ridge_pen, #Ridge penalty
      n_resamples = n_resamples, exp_ret_score_jitter = exp_ret_score_jitter, cov_eigval_jitter = cov_eigval_jitter #MVO
    ),

    #Micro-Macro Allocation Framework
    mmaf = create_mmaf_portfolio(
      universe_m_d_ref = universe_m_d_ref, #Signal Universe
      mmaf_method = mmaf_method, top_down_proxy_port_method = top_down_proxy_port_method, #MMAF method
      covariance_matrix = covariance_matrix,
      eligible_returns_m_xts_upd_ref = eligible_returns_m_xts_upd_ref,
      selected_benchmark_m_xts_upd_ref = selected_benchmark_m_xts_upd_ref,
      active_returns = active_returns,  #Returns to estimate cov matrix
      cov_estimation_method = cov_estimation_method,
      cov_matrix_sample_size = cov_matrix_sample_size, #How to estimate covariance matrix?
      groups_m_d_ref = groups_m_d_ref, mmaf_group_col = mmaf_group_col, #Sectors
      liquidity_m_d_ref = liquidity_m_d_ref,
      micro_port_construction_method = micro_port_construction_method, #Micro portfolio construction method
      linkage = linkage, #Micro HRP
      liquidity_constraint_policy = liquidity_constraint_policy, turnover_constraint_policy = turnover_constraint_policy,
      concentration_constraint_policy = concentration_constraint_policy, #Micro Policies
      cap_weighting_metric = cap_weighting_metric, #Micro Cap-Weight and Cap-Scaled
      n_random_ports = n_random_ports, random_ports_method = random_ports_method,
      opt_objective = opt_objective, opt_method = opt_method, ridge_pen = ridge_pen,
      n_resamples = n_resamples, exp_ret_score_jitter = exp_ret_score_jitter, cov_eigval_jitter = cov_eigval_jitter, #Micro MVO methods
      rp_method = rp_method, exp_ret_score_tilt = exp_ret_score_tilt, exp_ret_score_tilt_eta = exp_ret_score_tilt_eta, #Micro Risk Parity
      macro_port_construction_method = macro_port_construction_method, #Macro portfolio construction method
      macro_concentration_constraint_policy = macro_concentration_constraint_policy, #Macro concentration constraints
      macro_cap_weighting_metric = macro_cap_weighting_metric, #Macro cap weighting metric
      macro_n_random_ports = macro_n_random_ports,  macro_random_ports_method = macro_random_ports_method,
      macro_opt_objective = macro_opt_objective, macro_opt_method = macro_opt_method, macro_ridge_pen = macro_ridge_pen, #Macro MVO methods
      macro_n_resamples = macro_n_resamples, macro_exp_ret_score_jitter = macro_exp_ret_score_jitter, macro_cov_eigval_jitter = macro_cov_eigval_jitter, #Macro MVO
      macro_rp_method = macro_rp_method, macro_exp_ret_score_tilt = macro_exp_ret_score_tilt, macro_exp_ret_score_tilt_eta = macro_exp_ret_score_tilt_eta,
      macro_linkage = macro_linkage, #Macro Risk Parity
      lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization, #Winsorize cap scores
      selected_benchmark = selected_benchmark, bench_assets_returns_m_xts_upd_ref = bench_assets_returns_m_xts_upd_ref,
      parallel = parallel #Parallelization
    )

  )

  ###################

  #Create PORT Object
  ###################
  ## Get results
  universe_m_d_ref <- port_results_list$universe_m_d_ref

  ## Define eligible universe
  if(port_construction_method != "custom_weights"){
    #For general port_construction_methods, only eligible assets can have weights different from zero.
    eligible_universe_m_d_ref <- universe_m_d_ref %>% dplyr::filter(is_eligible == 1)
  } else {
    #In case of custom weights, it is possible that non-eligible assets have weights (eg. theme_ss_bench_weights)
    #For level == "group", just consider all as eligible
    if (level == "group") {
      eligible_universe_m_d_ref <- universe_m_d_ref
    } else {
      eligible_universe_m_d_ref <- universe_m_d_ref %>% dplyr::filter(weights != 0)
    }
  }

  ##Calculate relative risk contribution
  if (!is.null(covariance_matrix)){

    ### Make sure that covariance matrix and universe_m_d_ref are aligned
    if (!identical(eligible_universe_m_d_ref %>% dplyr::pull(tickers), rownames(covariance_matrix))){
      stop("universe_m_d_ref tickers and covariance matrix rownames do not match")
    }

    ### Compute relative risk contribution
    relative_risk_contribution_df <- relative_risk_contribution(
      weights = eligible_universe_m_d_ref %>% dplyr::pull(weights), #Weights
      covariance_matrix = covariance_matrix #Covariance
    )

    ###Attach relative risk contribution to universe_m_d_ref object
    universe_m_d_ref <- universe_m_d_ref %>% dplyr::left_join(relative_risk_contribution_df, by = "tickers") %>%
      dplyr::relocate(rel_risk_contr, .before = weights) #Move to the left of weights
    universe_m_d_ref$rel_risk_contr[which(is.na(universe_m_d_ref$rel_risk_contr))] <- 0 #Fill NAs with 0

    ###Also update eligible_universe_m_d_ref
    eligible_universe_m_d_ref <- eligible_universe_m_d_ref %>% dplyr::left_join(relative_risk_contribution_df, by = "tickers") %>%
      dplyr::relocate(rel_risk_contr, .before = weights)
  }

  ##Compute hierarchical clusters
  if (!is.null(covariance_matrix) && nrow(covariance_matrix) > 1){
    hc <- compute_hierarchical_clusters(correlation_matrix = stats::cov2cor(covariance_matrix), linkage = linkage)$hc
  } else {
    hc <- NULL
  }

  ##For active portfolios, build bench port object
  if (!is.null(selected_benchmark) &&
      paste0(selected_benchmark,"_bench_weights") %in% colnames(universe_m_d_ref) &&
      level %in% c("port", "sub_port")){

    ### Create a custom_weights_m_d_ref
    bench_weights_m_d_ref <- universe_m_d_ref %>%
      dplyr::select(id, tickers, dates, !!rlang::sym(paste0(selected_benchmark,"_bench_weights"))) %>%
      dplyr::rename(weights = !!rlang::sym(paste0(selected_benchmark,"_bench_weights")))

      #### If level is sub_port, normalize weights to sum to 1 and do not pass groups_m_d_ref
      if (level == "sub_port"){
        bench_weights_m_d_ref <- bench_weights_m_d_ref %>%
          dplyr::mutate(
            weights = weights/sum(weights)
          )
      }

    ### Create simple bench_universe_m_d_ref
    bench_universe_m_d_ref <- universe_m_d_ref %>%
      dplyr::select(-dplyr::any_of(c("min_weight", "max_weight", "weights", "rel_risk_contr"))) %>%
      dplyr::left_join(bench_weights_m_d_ref %>% dplyr::select(id, weights),
                       by = "id") %>%
      dplyr::mutate(is_eligible = ifelse(weights > 0, 1, 0)) %>%
      dplyr::select(-weights)

    ### Create benchmark port object
    if (isTRUE(verbose)){
      cat("\n")
      cat(crayon::blurred(paste0("Calculating ", selected_benchmark, " portfolio...\n")))
    }

    selected_benchmark_port_obj <- set_portfolio_weights(
      universe_m_d_ref = bench_universe_m_d_ref, #Universe
      port_construction_method = "custom_weights",
      custom_weights_m_d_ref = bench_weights_m_d_ref,
      eligible_returns_m_xts_upd_ref = bench_assets_returns_m_xts_upd_ref, #Return sample
      cov_matrix_sample_size = cov_matrix_sample_size, #Cov estimation
      cov_estimation_method = cov_estimation_method,
      groups_m_d_ref = groups_m_d_ref,
      selected_benchmark = NULL, #Avoid infinite recursion
      level = if (level == "sub_port") "sub_benchmark" else "benchmark"
    )
    if (isTRUE(verbose)){
      cat("\n")
      cat(crayon::blurred("Finished benchmark portfolio.\n"))
    }

    selected_benchmark_port_obj@port_name <- selected_benchmark
    bench_universe_m_d_ref <- selected_benchmark_port_obj@universe_m_d_ref@data
    bench_cov_matrix <- selected_benchmark_port_obj@covariance_matrix

  } else {

      selected_benchmark_port_obj <- NULL
      bench_universe_m_d_ref <- NULL
      bench_cov_matrix <- NULL
  }

  ##Calculate macro portfolio
  if (port_construction_method != "mmaf"){
    if (!is.null(groups_m_d_ref) &&
        level %in% c("port", "benchmark")){

        ### Get group column
        group_col <- names(groups_m_d_ref)[4] #First column is tickers

        ### Compute macro objects
        macro_objects_list <- compute_agg_macro_objects(
          universe_m_d_ref = universe_m_d_ref,
          covariance_matrix = covariance_matrix,
          group_col = group_col,
          liquidity_m_d_ref = liquidity_m_d_ref,
          micro_universe_m_d_ref_list = NULL
        )

        ### Create a custom_weights_m_d_ref
        group_weights_m_d_ref <- macro_objects_list$group_universe_m_d_ref %>%
          dplyr::select(id, tickers, dates, weights)

        ### Create benchmark port object
        if (isTRUE(verbose)){
          cat("\n")
          cat(crayon::blurred("Calculating macro portfolio...\n"))
        }
        macro_port_obj <- set_portfolio_weights(
          universe_m_d_ref = macro_objects_list$group_universe_m_d_ref %>%
            dplyr::select(-weights), #Universe
          port_construction_method = "custom_weights",
          custom_weights_m_d_ref = group_weights_m_d_ref,
          covariance_matrix = macro_objects_list$group_covariance_matrix,
          groups_m_d_ref = NULL,
          selected_benchmark = NULL, #Avoid infinite recursion
          level = "group"
        )
        if (isTRUE(verbose)){
          cat("\n")
          cat(crayon::blurred("Finished macro portfolio.\n"))
        }

        macro_port_obj@port_name <- group_col
        group_universe_m_d_ref <- macro_objects_list$group_universe_m_d_ref
        group_cov_matrix <- macro_objects_list$group_covariance_matrix

    } else {
      macro_port_obj <- NULL
      group_col <- NULL
      group_universe_m_d_ref <- NULL
      group_cov_matrix <- NULL
    }
  } else {

      macro_port_obj <- port_results_list$macro
      group_col <- mmaf_group_col
      group_universe_m_d_ref <- macro_port_obj@universe_m_d_ref@data
      group_cov_matrix <- macro_port_obj@covariance_matrix
  }

  ##Calculate port stats
    ### Call 'calculate_port_stats'
    if (isTRUE(verbose)){
      cat("\n")
      cat("Calculating portfolio statistics...\n")
    }

    ### Resolver all_returns_m_xts_upd_ref
    if (level %in% c("benchmark", "sub_benchmark")){
      all_returns_m_xts_upd_ref <- eligible_returns_m_xts_upd_ref
    }
    if (level %in% c("port", "sub_port")) {
      #### Grab all tickers from eligible and bench universes
      if (is.null(bench_universe_m_d_ref)) {
        tickers_use <- eligible_universe_m_d_ref %>%
          dplyr::filter(is_eligible == 1L) %>%
          dplyr::pull(tickers)
      } else {
        tickers_use <- dplyr::union(
          eligible_universe_m_d_ref %>%
            dplyr::filter(is_eligible == 1L) %>%
            dplyr::pull(tickers),
          bench_universe_m_d_ref %>%
            dplyr::filter(is_eligible == 1L) %>%
            dplyr::pull(tickers)
        )
      }

      #### Bind eligible and bench returns and subset with tickers_use
      all_returns_m_xts_upd_ref <- if (is.null(eligible_returns_m_xts_upd_ref) &&
                                       is.null(bench_assets_returns_m_xts_upd_ref)) {
          NULL

        } else if (is.null(eligible_returns_m_xts_upd_ref)) {
          bench_assets_returns_m_xts_upd_ref[, col_match(bench_assets_returns_m_xts_upd_ref, tickers_use), drop = FALSE]

        } else if (is.null(bench_assets_returns_m_xts_upd_ref)) {
          eligible_returns_m_xts_upd_ref[, col_match(eligible_returns_m_xts_upd_ref, tickers_use), drop = FALSE]

        } else {
          # Both exist - combine them
          bench_only_tickers <- setdiff(
            colnames(bench_assets_returns_m_xts_upd_ref),
            colnames(eligible_returns_m_xts_upd_ref)
          )

          combined <- if (length(bench_only_tickers) > 0) {
            cbind(eligible_returns_m_xts_upd_ref,
                  bench_assets_returns_m_xts_upd_ref[, bench_only_tickers, drop = FALSE])
          } else {
            eligible_returns_m_xts_upd_ref
          }

          combined[, col_match(combined, tickers_use), drop = FALSE]
      }
    }
    if (level == "group"){
      all_returns_m_xts_upd_ref <- NULL
    }

    stats_res <- calculate_port_stats(
      universe_m_d_ref = universe_m_d_ref,
      #### For level == "port", it is safer to recalculate cov matrix to assume
      #### full coverage in case of benchmark. The same happens for level == "benchmark"
      #### For level == "group", one can reuse covariance_matrix
      covariance_matrix = if (level %in% c("group")) covariance_matrix else NULL,
      #### For level == "groups", we should not pass group metrics
      group_universe_m_d_ref = if (level %in% c("port", "benchmark")) group_universe_m_d_ref else NULL,
      group_cov_matrix = if (level %in% c("port", "benchmark")) group_cov_matrix else NULL,
      #### A benchmark is only to be provided for level port
      selected_benchmark = if (level %in% c("port", "sub_port") && !is.null(bench_universe_m_d_ref)) selected_benchmark else NULL,
      bench_universe_m_d_ref = if (level %in% c("port", "sub_port") && !is.null(selected_benchmark)) bench_universe_m_d_ref else NULL,
      #### Reestimate only for port level
      all_returns_m_xts_upd_ref = if (level %in% c("port", "sub_port", "benchmark", "sub_benchmark")) all_returns_m_xts_upd_ref else NULL, #Return sample
      cov_matrix_sample_size = if (level %in% c("port", "sub_port", "benchmark", "sub_benchmark")) cov_matrix_sample_size else NULL, #Cov estimation
      cov_estimation_method = if (level %in% c("port", "sub_port", "benchmark", "sub_benchmark")) cov_estimation_method else NULL,
      groups_m_d_ref = if (level %in% c("port", "sub_port", "benchmark", "sub_benchmark")) groups_m_d_ref else NULL
    )

  port_stats <- stats_res$port_stats

    #### If level is 'port' and a benchmark is provided to port_stats, add act_weights and act_rel_risk_contr columns
    if (level %in% c("port", "sub_port") && !is.null(selected_benchmark)){
      universe_m_d_ref <- universe_m_d_ref %>%
        dplyr::left_join(
          stats_res$assets_stats %>%
            dplyr::select(dplyr::any_of(c("tickers", "rel_risk_contr", "weights"))) %>%
            ##### Rename weights and rel_risk_contr to active versions (if rel_risk_contr exists)
            dplyr::rename_with(
              .cols = dplyr::any_of(c("weights", "rel_risk_contr")),
              .fn = ~ paste0("act_", .)
            ),
          by = "tickers"
        )
      ### Replace NAs with zero
      universe_m_d_ref$act_weights[which(is.na(universe_m_d_ref$act_weights))] <- 0
      if ("act_rel_risk_contr" %in% colnames(universe_m_d_ref)){
        universe_m_d_ref$act_rel_risk_contr[which(is.na(universe_m_d_ref$act_rel_risk_contr))] <- 0
      }
    }


  ##Create the s4 obj
  eligible_assets <- eligible_universe_m_d_ref %>% dplyr::pull(tickers)
  port_obj <- methods::new("port",
                            universe_m_d_ref = suppressMessages(create_meta_dataframe(universe_m_d_ref %>% dplyr::arrange(id))), ##Re-order according to id
                            port_construction_method = port_construction_method,
                            eligible_assets = eligible_assets,
                            exp_ret_score = if (port_construction_method %in% c("sw", "cs", "mvo", "mmaf") || (port_construction_method %in% c("rp", "hrp") && !is.null(exp_ret_score_tilt) && exp_ret_score_tilt != "none")){
                              eligible_universe_m_d_ref %>% dplyr::pull(exp_ret_score)
                            } else NULL,
                            covariance_matrix = covariance_matrix,
                            correlation_matrix = if (!is.null(covariance_matrix)) stats::cov2cor(covariance_matrix) else NULL,
                            weights = eligible_universe_m_d_ref %>% dplyr::pull(weights),
                            rel_risk_contr = if (!is.null(covariance_matrix)) eligible_universe_m_d_ref %>% dplyr::pull(rel_risk_contr) else NULL,
                            clusters = hc,
                            mvo_port_spec = if (port_construction_method == "mvo") port_results_list$port_spec else NULL,
                            random_port_weights = if (port_construction_method == "mvo" && opt_method == "random" && length(eligible_assets) > 1) port_results_list$random_portfolios_weights_df %>% dplyr::select(-exp_ret_score) else NULL,
                            ind_max_weights = if (!is.null(concentration_constraint_policy$max_abs_active_individual_weight) && port_construction_method == "mvo" && length(eligible_assets) > 1) eligible_universe_m_d_ref %>% dplyr::pull(max_weight) else NULL,
                            ind_min_weights = if (!is.null(concentration_constraint_policy$max_abs_active_individual_weight) && port_construction_method == "mvo" && length(eligible_assets) > 1) eligible_universe_m_d_ref %>% dplyr::pull(min_weight) else NULL,
                            groups = if (!is.null(groups_m_d_ref)) groups_m_d_ref else NULL,
                            group_col = group_col,
                            mmaf_method = if (port_construction_method == "mmaf") mmaf_method else NULL,
                            group_cov_matrix = group_cov_matrix,
                            micro = if (port_construction_method == "mmaf") port_results_list$micro else NULL,
                            macro = macro_port_obj,
                            port_stats = port_stats,
                            selected_benchmark_port = selected_benchmark_port_obj,
                            port_name = "not_identified"
  )

  #Return portfolio results
  ###################
  if (isTRUE(verbose) && level == "port"){
    cat("\n")
    cat(crayon::cyan("Finished portfolio.\n"))
    tictoc::toc()
  }
  return(port_obj)

}
