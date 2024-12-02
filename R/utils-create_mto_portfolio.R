#' Create a MTO portfolio
#'
#' @param universe_m_d_ref A dataframe with tickers, is_eligible and final_signal columns
#' @param returns_d_ref A dataframe in which columns represent tickers present in universe_m_d_ref and row represent days
#' It should include all stocks in current_stock_universe and a dates column with at least covariance_matrix_sample_size days before current_date
#' @param current_date Current date
#' @param covariance_matrix_sample_size Number of periods to subset returns_df sample when estimating the covariance_matrix. A high number will provide
#' higher degrees of freedom, but old returns might not reflect current risk due to parameter shift. A low number will tend to expose estimation
#' to dimensionality curse.
#' @param covariance_estimation_method One of SAM (Sample), EWMA, CC (Constant Correlation), PCA1, PCA2, Shrink_ID or Shrink_CC.
#' If NULL, blending_method can only be EW or IR
#' @param rp_method
#' @param n_random_portfolios
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
#' - `group_classification`: A character vector describing the group classification to be used to apply group constraints.
#' Must have a correspondence in `groups_m_d_ref`
#' - `max_abs_active_group_weight`: The maximum absolute group active weight used for creating group constraints in `generate_group_constraints`.
#' If a given group has no eligible stock, the one with the greatest signal will be automatically promoted.
#' Note that, in the context of `generate_group_constraints`, a `benchmark_weights_m_d_ref` data frame must also be supplied.
#' @param benchmark_weights_m_d_ref A data frame containing columns for id, tickers, dates, and current benchmark weights columns.
#'  All tickers in the current stock universe must have a unique correspondence in this data frame.
#' @param groups_m_d_ref A data frame containing columns for id, tickers, dates, and group classification columns following a given classification method.
#' All tickers in the current stock universe must have a unique correspondence in the data frame.
#' @param mto_port_objective A character describing the objective to maximize in order to choose the best portfolio. One of "AR (active return)",
#' "TE (tracking error)" or " IR (information ratio)"
#' @return
#' @export
#'
create_mto_portfolio <- function(universe_m_d_ref,
                                 returns_upd_ref, covariance_matrix_sample_size, covariance_estimation_method, #Covariance matrix estimation
                                 liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL, #Constraints
                                 groups_m_d_ref = NULL,
                                 n_random_portfolios = 2000, rp_method = "sample", mto_port_objective = "IR",
                                 verbose = TRUE
){

  #Message
  if(verbose){
    tictoc::tic()
    cat("\n")
    cat("Deriving weights through MTO...")
    cat("\n")
    cat(paste0("Optimization objective: ", mto_port_objective, "."))
  }

  #Eligible universe
  eligible_universe_m_d_ref <- universe_m_d_ref %>% dplyr::filter(is_eligible == 1)

  #Create portfolio_specification
  port_spec <- PortfolioAnalytics::portfolio.spec(assets = eligible_universe_m_d_ref$tickers)

  #Constraints
  ############

  ##Full Investment Constraint
  port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "full_investment")

  ##Box Constraints
  if(!is.null(concentration_constraint_policy)){
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
    #Long-only box constraint
    port_spec_constrained <- PortfolioAnalytics::add.constraint(portfolio = port_spec, type = "box")
  }

  ##Group Constraints
  if(!is.null(concentration_constraint_policy$max_abs_active_group_weight)){
    ###Message
    if(verbose){
      cat("\n")
      cat("Defining group constraints following: ", colnames(groups_m_d_ref[,-c(1:3)]))
    }
    ###Generate group constraints
    group_constraints_list <- generate_group_constraints(universe_m_d_ref = universe_m_d_ref, #Current Stock Universe
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
    cat(paste0("Beginning numerical optimization with ", n_random_portfolios, " portfolios and ", rp_method, " method."))
  }
  ##Numeric optimization
  random_portfolios_weights <- PortfolioAnalytics::random_portfolios(portfolio = port_spec_constrained,
                                                                     permutations = n_random_portfolios,
                                                                     rp_method = rp_method
  )
  ##Random weights DF
  random_portfolios_weights_df <- as.data.frame(t(random_portfolios_weights)) %>% tibble::rownames_to_column()
  colnames(random_portfolios_weights_df)[1] <- "tickers"

  random_portfolios_weights_df <- dplyr::left_join(dplyr::select(eligible_universe_m_d_ref, c(tickers, final_signal)), #Get eligible stocks
                                                   random_portfolios_weights_df, #Get weights
                                                   by = "tickers")


  ###########################

  #Calculate portfolio metrics
  ####################
  ##Calculate Portfolios Expected Returns
  expected_returns <-
    apply(dplyr::select(random_portfolios_weights_df, c(-tickers, -final_signal)), 2, function(col){ ##Multiply each weights col to final signal
      sum(col * random_portfolios_weights_df$final_signal) #Calculate active returns
    })

  ##Calculate Portfolio Covariance
  covariance_matrix <- estimate_covariance_matrix(tickers = eligible_universe_m_d_ref$tickers, #Eligible universe
                                                  returns_upd_ref = returns_upd_ref, #Return sample
                                                  covariance_matrix_sample_size = covariance_matrix_sample_size, covariance_estimation_method = covariance_estimation_method, #Cov estimation
                                                  groups_m_d_ref = groups_m_d_ref) #Groups

  ##t(w) * Cov * w
  expected_risk <-
    apply(dplyr::select(random_portfolios_weights_df, c(-tickers, -final_signal)), 2, function(col){
      sqrt(t(col) %*% covariance_matrix %*% col) #Calculate tracking error
    })

    ##Get IR
    expected_ir <- expected_returns/expected_risk

  ####################

  #Get best portfolio
  ###################
  ##Which stock maximizes objective?
  best_portfolio <- names(switch(mto_port_objective,
                                 "IR" = which.max(expected_ir), #Max IR
                                 "AR" = which.max(expected_returns), #Max AR
                                 "TE" = which.min(expected_risk) #Min Risk
  ))

  #Get best portfolio
  mto_weights <- data.frame(tickers = random_portfolios_weights_df$tickers, weights = random_portfolios_weights_df[,best_portfolio]) #get weights and relative risk contribution

  #Merge with current_stock_universe
  universe_m_d_ref <- dplyr::left_join(universe_m_d_ref, mto_weights, by = "tickers")

  #Replace NAs with zeros
  universe_m_d_ref[which(is.na(universe_m_d_ref$weights)),"weights"] <- 0

  #Message
  if(verbose){
    cat("\n")
    cat(crayon::green(paste("Optimal weights succesfully defined")))
    cat("\n")
    cat(paste("Metrics for the portfolio were:"))
    cat("\n")
    cat(paste("Expected Active Return:", round(expected_returns[as.numeric(gsub("V", "", best_portfolio))],3)))
    cat("\n")
    cat(paste("Expected Tracking Error:", round(expected_risk[as.numeric(gsub("V", "", best_portfolio))],3)))
    cat("\n")
    cat(paste("Expected Information Ratio:", round(expected_ir[as.numeric(gsub("V", "", best_portfolio))],3)))
    cat("\n")
    elapsed_time <- tictoc::toc()
  }

  #Return
  return(universe_m_d_ref)




}
