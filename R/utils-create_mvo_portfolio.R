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
#' @param concentration_constraint_policy A named list containing up to four elements:
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
#' @param opt_method A character describing the optimization method to be used. One of "random" or "DEoptim".
#' @param ridge_pen Optional. A numeric value representing the ridge penalty to be applied when a target portfolio is provided.
#' Higher values will increase the importance of being close to the target portfolio.
#' @param verbose A logical indicating whether to print messages during the execution of the function.
#'
create_mvo_portfolio <- function(universe_m_d_ref,
                                 covariance_matrix,
                                 liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL, concentration_constraint_policy = NULL, #Constraints
                                 groups_m_d_ref = NULL,
                                 n_random_ports = 2000, random_ports_method = "sample", opt_objective = "sharpe", opt_method = "random",
                                 ridge_pen = NULL,
                                 verbose = TRUE
){

  #Message
  if (verbose) tictoc::tic()

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

  ##Get risk-return trade-off
  expected_sharpe <- expected_returns/expected_risk

  ####################

  #Get best portfolio
  ###################
  ##Which stock maximizes objective?
  if (is.null(ridge_pen)){

    ### Pick best portfolio overall
    best_portfolio <- names(switch(opt_objective,
                                   "sharpe" = which.max(expected_sharpe), #Max Sharpe
                                   "return" = which.max(expected_returns), #Max Return
                                   "risk" = which.min(expected_risk) #Min Risk
    ))

  } else {

    ## Calculate distance to target portfolio
    distance_to_target <- random_portfolios_weights_df %>%
      ### Join target portfolio
      dplyr::left_join(universe_m_d_ref %>%
                         dplyr::select(tickers, target_weights),
                       by = "tickers") %>%
      ### Calculate squared differences to target weights
      dplyr::mutate(
        dplyr::across(
          .cols = starts_with("V"),
          .fns = ~ (. - target_weights)^2,
          .names = "diff_{col}"
        )
      ) %>%
      ### Sum squared differences for each portfolio
      dplyr::select(dplyr::starts_with("diff_V")) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), \(x) sum(x, na.rm = TRUE))) %>%
      ### Convert to numeric vector with portfolio names
      { vec <- as.numeric(.); names(vec) <- sub("^diff_", "", names(.)); vec }

    ## Pick best portfolio considering ridge penalty
    best_portfolio <- names(
      switch(opt_objective,
             "sharpe" = which.max(expected_sharpe  - distance_to_target * ridge_pen), #Max Sharpe
             "return" = which.max(expected_returns - distance_to_target * ridge_pen), #Max Return
             "risk"   = which.min(expected_risk    + distance_to_target * ridge_pen) #Min Risk
    ))

  }


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
    ## If ridge penalty is not NULL, print distance to target
    if (!is.null(ridge_pen)){
      cat(paste("Distance to target portfolio:", round(distance_to_target[as.numeric(gsub("V", "", best_portfolio))],6)))
      cat("\n")
    }
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

#' Create a Resampled MVO portfolio
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
#' @param concentration_constraint_policy A named list containing up to four elements:
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
#' @param opt_method A character describing the optimization method to be used. One of "random" or "DEoptim".
#' @param target_port_m_d_ref Optional. A dataframe with tickers and target_weights columns representing a target portfolio.
#' If provided, the optimization will consider a ridge penalty to select a portfolio that is closer to the target portfolio.
#' @param ridge_pen Optional. A numeric value representing the ridge penalty to be applied when a target portfolio is provided.
#' Higher values will increase the importance of being close to the target portfolio.
#' @param verbose A logical indicating whether to print messages during the execution of the function.
#'
#' @export
create_resampled_mvo_portfolio <- function(universe_m_d_ref,
                                           covariance_matrix,
                                           liquidity_constraint_policy = NULL,
                                           turnover_constraint_policy = NULL,
                                           concentration_constraint_policy = NULL, #Constraints
                                           groups_m_d_ref = NULL,
                                           n_random_ports = 2000, random_ports_method = "sample",
                                           opt_objective = "sharpe", opt_method = "random", ridge_pen = NULL,
                                           n_resamples = 0, exp_ret_score_jitter = 0, cov_eigval_jitter = 0,
                                           parallel = FALSE,
                                           verbose = TRUE){

  #Message
  if(verbose){
    tictoc::tic()
    cat("\n")
    cat("Deriving weights through MVO...")
    cat("\n")
    cat(paste0("Optimization objective: ", opt_objective, "."))
    cat("\n")
    cat(paste0("Number of resamples: ", n_resamples, "."))
    cat("\n")
    if (!is.null(ridge_pen)) cat(paste0("Ridge penalty: ", ridge_pen, "."))
    cat("\n")
  }

  #Jitter Helpers

    ## Jitter exp_ret_score
    jitter_exp_ret_score <- function(exp_ret_score, exp_ret_score_jitter){

      ## For default case, return the exp return vector
      if (exp_ret_score_jitter <= 0) return(exp_ret_score)

      ## Calculate multiplier as the standard deviation of the expected returns
      ## This ensures that the jitter is scaled appropriately to the variability of the expected returns
      mult <- stats::sd(exp_ret_score, na.rm = TRUE)
      if (mult == 0 || is.na(mult) || !is.finite(mult)) mult <- 1

      ## Add jitter
      exp_ret_score +
        stats::rnorm(length(exp_ret_score), 0, exp_ret_score_jitter * mult)

    }


    ## Jitter covariance matrix
    jitter_sigma_eig <- function(covariance_matrix, cov_eigval_jitter){

      ## For default case, return the covariance matrix
      if (cov_eigval_jitter <= 0) return(covariance_matrix)

      ## Eigen decomposition S = V diag(lambda) V^T
      ev <- eigen(covariance_matrix, symmetric = TRUE)
      # ev$values  : eigenvalues (variances of orthogonal risk modes)
      # ev$vectors : eigenvectors (orthonormal directions / risk modes)

      # Jitter eigenvalues from lognormal (ensure positivity to preserve pos-definiteness)
      # meanlog = 0 to keep expected value unchanged
      mult <- exp(stats::rnorm(length(ev$values), 0, cov_eigval_jitter))
      lam2 <- pmax(ev$values * mult, 1e-12) # avoid numerical issues

      # Rebuild Σ~ = V diag(lam2) V^T (same modes, different mode variances)
      out <- ev$vectors %*% diag(lam2) %*% t(ev$vectors)
      dimnames(out) <- dimnames(covariance_matrix)
      out <- (out + t(out)) / 2 # ensure symmetry

      out

    }

  # Helper to run create_mvo_portfolio
  run_mvo <- function(k){

    ##Message
    if (verbose){
      if (k == 0) cat("Running base case (no jitter)...\n")
      else cat(paste0("Running MVO resample ", k, " out of ", n_resamples, "...\n"))
    }

    ## Jitter
      ### Exp Ret
      jittered_universe_m_d_ref <- universe_m_d_ref
      if (k > 0){
        jittered_universe_m_d_ref$exp_ret_score <- jitter_exp_ret_score(
          exp_ret_score = jittered_universe_m_d_ref$exp_ret_score,
          exp_ret_score_jitter = exp_ret_score_jitter
        )
      }

      ### Covariance
      jittered_covariance_matrix <- covariance_matrix
      if (k > 0){
        jittered_covariance_matrix <- jitter_sigma_eig(
          covariance_matrix = jittered_covariance_matrix,
          cov_eigval_jitter = cov_eigval_jitter
        )
      }

    ## Create MVO Portfolio
      ### Defensive checks
      if (!isTRUE(all.equal(nrow(jittered_covariance_matrix), sum(universe_m_d_ref$is_eligible == 1)))) {
        stop("Dimension mismatch: covariance_matrix rows must equal number of eligible assets.")
      }
      eligible_tickers <- universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)
      if (!identical(rownames(jittered_covariance_matrix), colnames(jittered_covariance_matrix))) {
        stop("Covariance matrix must have matching row/column names.")
      }
      if (!setequal(eligible_tickers, rownames(jittered_covariance_matrix))) {
        stop("Covariance matrix names must match eligible tickers.")
      }
      if (!identical(eligible_tickers, rownames(jittered_covariance_matrix))) {
        stop("Covariance matrix names must be in the same order as eligible tickers.")
      }
      ### Call
      res <- create_mvo_portfolio(
        universe_m_d_ref = jittered_universe_m_d_ref,
        covariance_matrix = jittered_covariance_matrix,
        liquidity_constraint_policy = liquidity_constraint_policy, #Constraints
        turnover_constraint_policy = turnover_constraint_policy,
        concentration_constraint_policy = concentration_constraint_policy, #Constraints
        groups_m_d_ref = groups_m_d_ref,
        n_random_ports = n_random_ports, random_ports_method = random_ports_method,
        opt_objective = opt_objective, opt_method = opt_method, ridge_pen = ridge_pen,
        verbose = FALSE
      )

      ### Get weights
      if (k > 0){
        return(res$universe_m_d_ref %>% dplyr::select(tickers, weights))
      } else {
        res
      }


  }

  # Run resamples
    ## First run base case (no jitter) to ensure at least one valid run
    base_case_mvo <- run_mvo(0)

    ## Run resamples if n_resample >= 1
    if (n_resamples >= 1){
      if (parallel){
        ##Parallel exceution using furrr::map
        mvo_weights_list <- furrr::future_map(
          seq_len(n_resamples), \(k) run_mvo(k),
          .progress = TRUE, .options = furrr::furrr_options(seed = TRUE)
        )
      } else {
        ##Sequential exceution using purrr::map
        mvo_weights_list <- purrr::map(
          seq_len(n_resamples), \(k) run_mvo(k)
        )
      }

      # Average weights using purrr::reduce and left_join
      avg_mvo_weights <- purrr::reduce(
        mvo_weights_list,
        \(df1, df2) dplyr::left_join(df1, df2, by = "tickers"),
        .init = base_case_mvo$universe_m_d_ref %>% dplyr::select(tickers, weights)
      ) %>%
        dplyr::mutate(weights = rowMeans(dplyr::select(., -tickers), na.rm = TRUE)) %>%
        dplyr::select(tickers, weights)
    } else {
      avg_mvo_weights <- base_case_mvo$universe_m_d_ref %>% dplyr::select(tickers, weights)
    }

  # Merge with current_stock_universe
  universe_m_d_ref <- dplyr::left_join(universe_m_d_ref, avg_mvo_weights, by = "tickers")

    ## If resampling was done, add base_case_mvo weights column for reference (position before weights col)
    if (n_resamples >= 1){
      universe_m_d_ref <- universe_m_d_ref %>%
        dplyr::left_join(base_case_mvo$universe_m_d_ref %>%
                           dplyr::select(tickers, base_weights = weights), by = "tickers") %>%
        dplyr::relocate(base_weights, .before = weights)
    }


  #Return
  mvo_results_list <- list(
    universe_m_d_ref = universe_m_d_ref,
    weights = universe_m_d_ref$weights,
    exp_ret_score = universe_m_d_ref$exp_ret_score,
    port_spec = base_case_mvo$port_spec,
    random_portfolios_weights_df = base_case_mvo$random_portfolios_weights_df
  )

  if (verbose) tictoc::toc()


  return(mvo_results_list)


}









