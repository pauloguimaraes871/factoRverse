#' Create a MVO portfolio
#'
#' @param universe_m_d_ref A dataframe with tickers, is_eligible and exp_ret_score columns
#' @param covariance_matrix The covariance matrix of all eligible stocks in universe_m_d_ref.
#' @param random_ports_method A character indicating the method to be used for generating random portfolios. Possible values are "random" or "grid".
#' @param n_random_ports An integer indicating the number of random portfolios to be generated.
#' @param liquidity_constraint_policy Optional. A named list containing objects used to apply liquidity constraints. Possible elements of the list are:
#' - `liquidity_floor_rule`: A character indicating the liquidity classification (e.g., micro_caps, small_caps) used to filter stocks. Stocks with less liquidity than specified in `liquidity_floor_rule` will be considered ineligible.
#'   In the case of the `generate_box_constraints` function, `liquidity_constraint_policy` can also contain:
#' - `liquidity_cap_rule` lists: One or many lists used to create upper bounds for weights based on a liquidity classification. Each list must contain:
#'   - `liquidity_classification`: A character indicating the classification for the cap.
#'   - `liquidity_cap`: A numeric value indicating the cap (upper bound) for stocks with that liquidity classification.
#'   Many liquidity caps might be created, and in this case, each `liquidity_cap_rule` must be identified with a number (e.g., liquidity_cap_rule_1, liquidity_cap_rule_2, and so on).
#' @param turnover_constraint_policy A named list containing objects used to build buffer zones and apply turnover constraints.
#' - Each element will constitute a `buffer_zone`, being a list with three elements:
#'   - `liquidity_classification` element: A liquidity classification (e.g., "micro_caps", "small_caps") for that buffer zone.
#'   - `top_assets_quantile_buffer`: A numeric value indicating a buffer value that relaxes `top_assets_quantile` for stocks with the specified liquidity classification.
#'   - `turnover_cap`: A numeric value specifying the turnover cap.
#'   Stocks that are less liquid than specified for a buffer zone and have a signal higher than the respective buffer quantile will be considered eligible, even if they do not meet the `liquidity_floor_rule`.
#' @param concentration_constraints_policy A named list containing up to four elements:
#' - `benchmark`: A character vector describing the benchmark to be used to apply constraint.
#' Must have a correspondence in `benchmark_weights_m_d_ref`
#' - `max_abs_active_individual_weight`: The maximum absolute individual active weights.
#' - `max_abs_active_group_weight`: The maximum absolute group active weight used for creating group constraints in `generate_group_constraints`.
#' If a given group has no eligible stock, the one with the greatest signal will be automatically promoted.
#' Note that, in the context of `generate_group_constraints`, a `benchmark_weights_m_d_ref` data frame must also be supplied.
#' @param groups_m_d_ref A data frame containing columns for id, tickers, dates, and group classification columns following a given classification method.
#' All tickers in the current stock universe must have a unique correspondence in the data frame.
#' @param opt_objective A character describing the objective to maximize in order to choose the best portfolio. One of "return (max return)",
#' "risk (min risk)" or "sharpe (max sharpe-ratio)"
#'
create_mvo_portfolio <- function(universe_m_d_ref,
                                 covariance_matrix,
                                 liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL, #Constraints
                                 groups_m_d_ref = NULL,
                                 n_random_ports = 2000, random_ports_method = "sample", opt_objective = "sharpe", opt_method = "random",
                                 verbose = TRUE
){

  #Message
  if(verbose){
    tictoc::tic()
    cat("\n")
    cat("Deriving weights through MVO...")
    cat("\n")
    cat(paste0("Optimization objective: ", opt_objective, "."))
  }

  #Eligible tickers
  eligible_tickers <- universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

  #Create portfolio_specification
  port_spec <- PortfolioAnalytics::portfolio.spec(assets = eligible_tickers)

  #Constraints
  ############

  ##Full Investment Constraint
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "full_investment")

  ##Box Constraints
  if(!is.null(concentration_constraint_policy$max_abs_active_individual_weight)){
    ###Message
    if(verbose){
      cat("\n")
      cat("Defining box constraints")
    }

    ###Generate box contraints
    eligible_universe_m_d_ref <- generate_box_constraints(universe_m_d_ref = universe_m_d_ref, #Current Stock Universe
                                                          concentration_constraint_policy = concentration_constraint_policy, #Concentration Constraints Policy
                                                          liquidity_constraint_policy = liquidity_constraint_policy, #Liquidity Constraint Policy
                                                          turnover_constraint_policy = turnover_constraint_policy #Turnover Constraint Policy
    )
    ###Add box constraints
    port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec_constrained, type = "box", #Box Constraints
                                                                min = eligible_universe_m_d_ref$min_weight, max = eligible_universe_m_d_ref$max_weight) #Max and min box constraints

    ###Include box constraints in universe_m_d_ref just for refernce
    universe_m_d_ref <- dplyr::left_join(universe_m_d_ref, dplyr::select(eligible_universe_m_d_ref, tickers, max_weight, min_weight), by = "tickers")
    universe_m_d_ref$max_weight[which(is.na(universe_m_d_ref$max_weight))] <- 0
    universe_m_d_ref$min_weight[which(is.na(universe_m_d_ref$min_weight))] <- 0

  } else {
    #Create eligible_universe_m_d_ref
    eligible_universe_m_d_ref <- universe_m_d_ref %>% dplyr::filter(is_eligible == 1)

    #Long-only box constraint
    port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "box")
  }

  ##Group Constraints
  if(!is.null(concentration_constraint_policy$max_abs_active_group_weight)){
    ###Message
    if(verbose){
      cat("\n")
      cat("Defining group constraints following: ", colnames(groups_m_d_ref)[-c(1:3)])
    }
    ###Generate group constraints
    group_constraints_list <- generate_group_constraints(universe_m_d_ref = universe_m_d_ref, #Current Stock Universe
                                                         #Universe_m_d_ref is used instead of eligible_universe as there might be benchmark assets that are not eligible.
                                                         concentration_constraint_policy = concentration_constraint_policy, #Concentration Constraints Policy
                                                         groups_m_d_ref = groups_m_d_ref #group data
    )

    ###Add group constraints
    port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec_constrained, type = "group",
                                                                groups = group_constraints_list$eligible_assets_group_membership_list, #Set groups members
                                                                group_min = group_constraints_list$group_constraint_min, #Min Weight for groups
                                                                group_max = group_constraints_list$group_constraint_max #Max Weight for groups
    )
  }

  ############

  #Generate random portfolios
  ###########################
  ##Message
  if(verbose){
    cat("\n")
    cat(paste0("Beginning numerical optimization with ", n_random_ports, " portfolios and ", random_ports_method, " method."))
  }
  ##Numeric optimization
  random_portfolios_weights <- PortfolioAnalytics::random_portfolios(portfolio = port_spec_constrained,
                                                                     permutations = n_random_ports,
                                                                     random_ports_method = random_ports_method
  )
  ##Random weights DF
  random_portfolios_weights_df <- as.data.frame(t(random_portfolios_weights)) %>% tibble::rownames_to_column("tickers")

  random_portfolios_weights_df <- dplyr::left_join(dplyr::select(eligible_universe_m_d_ref, c(tickers, exp_ret_score)), #Get eligible stocks
                                                   random_portfolios_weights_df, #Get weights
                                                   by = "tickers")


  ###########################

  #Calculate portfolio metrics
  ####################
  ##Calculate Portfolios Expected Returns
  expected_returns <-
    apply(dplyr::select(random_portfolios_weights_df, c(-tickers, -exp_ret_score)), 2, function(col){ ##Multiply each weights col to final signal
      sum(col * random_portfolios_weights_df$exp_ret_score) #Calculate active returns
    })


  ##t(w) * Cov * w
  expected_risk <-
    apply(dplyr::select(random_portfolios_weights_df, c(-tickers, -exp_ret_score)), 2, function(col){
      sqrt(t(col) %*% covariance_matrix %*% col) #Calculate tracking error
    })

  ##Get Sharpe Ratio
  expected_sharpe <- expected_returns/expected_risk

  ####################

  #Get best portfolio
  ###################
  ##Which stock maximizes objective?
  best_portfolio <- names(switch(opt_objective,
                                 "sharpe" = which.max(expected_sharpe), #Max Sharpe
                                 "return" = which.max(expected_returns), #Max Return
                                 "risk" = which.min(expected_risk) #Min Risk
  ))

  #Get best portfolio
  mvo_weights <- data.frame(tickers = random_portfolios_weights_df$tickers,
                            weights = random_portfolios_weights_df[,best_portfolio]) #get weights and relative risk contribution

  #Merge with current_stock_universe
  universe_m_d_ref <- dplyr::left_join(universe_m_d_ref, mvo_weights, by = "tickers")

  #Replace NAs with zeros
  universe_m_d_ref[which(is.na(universe_m_d_ref$weights)),"weights"] <- 0

  #Check for weights different from 1
  if (abs(sum(universe_m_d_ref$weights) - 1) > 0.02){
    stop("Weights do not sum to 1")
  }

  #Message
  if(verbose){
    cat("\n")
    cat(crayon::green(paste("Optimal weights succesfully defined")))
    cat("\n")
    cat(paste("Metrics for the portfolio were:"))
    cat("\n")
    cat(paste("Expected Return:", round(expected_returns[as.numeric(gsub("V", "", best_portfolio))],3)))
    cat("\n")
    cat(paste("Expected Risk:", round(expected_risk[as.numeric(gsub("V", "", best_portfolio))],3)))
    cat("\n")
    cat(paste("Expected Sharpe:", round(expected_sharpe[as.numeric(gsub("V", "", best_portfolio))],3)))
    cat("\n")
    tictoc::toc()
  }

  #Return
  mvo_results_list <- list(
    universe_m_d_ref = universe_m_d_ref,
    weights = universe_m_d_ref$weights,
    exp_ret_score = universe_m_d_ref$exp_ret_score,
    port_spec = port_spec_constrained,
    random_portfolios_weights_df = random_portfolios_weights_df
  )


  return(mvo_results_list)




}
