#' Calculate Returns from a Portfolio and Update Weights
#'
#' This function calculates the returns of a portfolio, including transaction costs and updates the portfolio weights based on the latest data.
#'
#' @param current_date A Date object representing the current date.
#' @param is_rebalancing_month A logical indicating whether the current month is a rebalancing month.
#' @param stock_universe_m_d_ref A data frame containing stock universe data with columns for tickers and current weights.
#' @param target_m_d_ref A data frame containing target stock data with columns for tickers and forward returns.
#' @param portfolio_weights_m_d_ref A data frame containing current portfolio weights with columns for tickers and weights.
#' @param portfolio_weights_m_lstd_ref A data frame containing portfolio weights from the last period, including delisted stocks.
#' @param portfolio_returns_df A data frame to which portfolio returns will be added, including raw and net returns.
#' @param benchmark_returns_df A data frame containing benchmark returns.
#' @param main_liquidity_metric A string specifying the liquidity metric used to estimate transaction costs. Default is "mean_volfin_3m".
#' @param liquidity_m_d_ref A data frame containing liquidity metrics for the stocks.
#' @param volatility_m_d_ref A data frame containing volatility metrics for the stocks.
#' @param transaction_costs_list A list containing parameters to estimate transaction costs, including:
#'   \itemize{
#'     \item \code{strategy_aum} - The assets under management for the strategy.
#'     \item \code{alpha} - The BARRA model alpha parameter.
#'     \item \code{lambda} - The BARRA model lambda parameter.
#'     \item \code{direct_transaction_cost} - The direct transaction cost per trade.
#'   }
#'
#' @return A list with updated portfolio data, including:
#'   \itemize{
#'     \item \code{portfolio_weights_m_d_ref} - Updated portfolio weights.
#'     \item \code{rebalancing_m_d_ref} - Data frame with rebalancing information including transaction costs.
#'     \item \code{portfolio_returns_df} - Data frame with updated portfolio returns.
#'     \item \code{updated_portfolio_weights_m_d_ref} - Updated portfolio weights after applying returns.
#'   }
#' @export
calculate_portfolio_returns <- function(
    current_date, is_rebalancing_month,
    stock_universe_m_d_ref = NULL, target_m_d_ref, portfolio_weights_m_d_ref, portfolio_weights_m_lstd_ref,
    #Portfolio and benchmark returns
    portfolio_returns_df, benchmark_returns_df,
    #Parameters and data to estimate direct and indirect transaction costs
    main_liquidity_metric = "mean_volfin_3m",
    liquidity_m_d_ref, volatility_m_d_ref,
    transaction_costs_list = NULL
    ){

  #Initial prep
  ####################
  return_calculation_date <- lubridate::add_with_rollback(current_date, months(1)) #Get next month date

  #Transaction cost info
  strategy_aum <- transaction_costs_list$strategy_aum
  alpha <- transaction_costs_list$alpha
  lambda <- transaction_costs_list$lambda
  direct_transaction_cost <- transaction_costs_list$direct_transaction_cost

  #If it is a rebalancing_month
  if(is_rebalancing_month){
    #Pass weights
    portfolio_weights_m_d_ref$portfolio_weights <- stock_universe_m_d_ref$weights

    #Create a rebalancing dataframe to support calculations
    #######################################################
     ##Multiple joins
     rebalancing_m_d_ref <- dplyr::left_join(stock_universe_m_d_ref, target_m_d_ref, by = "tickers") %>%  #Join weights and returns
       dplyr::select(tickers, weights, fwd_return_1m) %>% #Select ony relevant cols
       dplyr::left_join(dplyr::select(liquidity_m_d_ref, -id, -dates), by = "tickers") %>% #Join liquidity data
       dplyr::left_join(dplyr::select(volatility_m_d_ref, -id, -dates), by = "tickers") %>%
       dplyr::full_join(dplyr::select(portfolio_weights_m_lstd_ref, tickers, old_portfolio_weights), by = "tickers") #Full join because one wants to consider delisted stocks

     ##Remove NAs
      ###in old_portfolio_weights with 0 (NAs are possible if stocks are delisted (were present in last portfolio and not in current)
      rebalancing_m_d_ref$old_portfolio_weights[which(is.na(rebalancing_m_d_ref$old_portfolio_weights))] <- 0
      ###in liquidity_m_d_ref (NAs are possible for stocks delisted)
      rebalancing_m_d_ref[, main_liquidity_metric][which(is.na(rebalancing_m_d_ref[, main_liquidity_metric]))] <- quantile(rebalancing_m_d_ref[, main_liquidity_metric], 0.25, na.rm = TRUE)
      ###in volatility_m_d_ref (NAs are possible for stocks delisted)
      rebalancing_m_d_ref[, "daily_vol"][which(is.na(rebalancing_m_d_ref[, main_liquidity_metric]))] <- mean(rebalancing_m_d_ref[, "daily_vol"], na.rm = TRUE)

    #######################################################

    #Calculate transaction costs
    #######################################################
      if(!all(is.null(alpha), is.null(lambda), is.null(direct_transaction_cost), is.null(strategy_aum))){
        cat("\n")
        cat("Calculating net portfolio returns")
       ##Add order data
       rebalancing_m_d_ref <- rebalancing_m_d_ref %>%
         dplyr::mutate(delta = weights - old_portfolio_weights) %>%
         dplyr::mutate(order = delta*strategy_aum) %>%
         dplyr::mutate(relative_order_size = abs(order)/!!rlang::sym(main_liquidity_metric))

       ##BARRA model parameters
       alpha <- alpha
       if(lambda == "dynamic"){
         #Dynamically adjust lambda according to trade size
          ##For very small trades, use lambda = 1
          rebalancing_m_d_ref[which(rebalancing_m_d_ref$relative_order_size <= 0.002),"lambda"] <- 1
          ##For small trades, use lambda = 0.5
          rebalancing_m_d_ref[which(rebalancing_m_d_ref$relative_order_size > 0.002 & rebalancing_m_d_ref$relative_order_size <= 0.05),"lambda"] <- 0.5
          ##For medium trades, use lambda = 0.25
          rebalancing_m_d_ref[which(rebalancing_m_d_ref$relative_order_size > 0.05 & rebalancing_m_d_ref$relative_order_size <= 0.1),"lambda"] <- 0.25
          ##For medium trades, use lambda = 0.10
          rebalancing_m_d_ref[which(rebalancing_m_d_ref$relative_order_size > 0.1),"lambda"] <- 0.1
       } else {
         rebalancing_m_d_ref[,"lambda"] <- 0.5
       }

       ##Calculate market_impact cost
       rebalancing_m_d_ref <-  rebalancing_m_d_ref %>% dplyr::mutate(market_impact_cost = alpha/2*(relative_order_size^lambda)*daily_vol)

       ##Get costs
       market_impact_cost <- sum(rebalancing_m_d_ref$market_impact_cost)
       total_cost <- direct_transaction_cost + market_impact_cost
      }

  }
  #######################################################

  ##Calculate and add returns
  ######################################################
  ##Calculate
  raw_return <- as.numeric(portfolio_weights_m_d_ref$weights %*% target_m_d_ref$fwd_return_1m) #Calculate forward portfolio return
  selected_benchmark_return <- selected_benchmark_returns_df[which(selected_benchmark_returns_df$dates == return_calculation_date), 2] #Get benchmark return
  raw_active_return <- raw_return - selected_benchmark_return #Get active return
  if(is_rebalancing_month){
    net_return <- raw_return - total_cost #Net return in case of rebalancing month
    net_active_return <- net_return - selected_benchmark_return #Net active return
    turnover <- mean(abs(rebalancing_m_d_ref$delta))
  }

  ##Add to portfolio_returns_df
  ###Raw returns
  portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "raw_return"] <- raw_return
  portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "raw_active_return"] <- raw_active_return

  ###Net Returns
  try(portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "net_return"] <- net_return, silent = TRUE)
  try(portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "net_active_return"] <- net_active_return, silent = TRUE)
  ###Costs
  try(portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "total_cost"] <- total_cost, silent = TRUE)
  try(portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "turnover"] <- turnover, silent = TRUE)

  ###Benchmark returns
  portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "selected_benchmark_return"] <- selected_benchmark_return


  ######################################################

  ##Update portfolio_weights
  updated_portfolio_weights_m_d_ref <- portfolio_weights_m_d_ref
  updated_portfolio_weights_m_d_ref$portfolio_weights <- portfolio_weights_m_d_ref$portfolio_weights * (target_m_d_ref$fwd_return_1m/100+1) #Multiply by returns
  updated_portfolio_weights_m_d_ref$portfolio_weights <- updated_portfolio_weights_m_d_ref$portfolio_weights/sum(updated_portfolio_weights_m_d_ref$portfolio_weights)

  ##Gather results
  portfolio_returns_results_list <- list(
    portfolio_weights_m_d_ref = portfolio_weights_m_d_ref,
    rebalancing_m_d_ref = rebalancing_m_d_ref,
    portfolio_returns_df = portfolio_returns_df,
    updated_portfolio_weights_m_d_ref = updated_portfolio_weights_m_d_ref
  )

  return(portfolio_returns_results_list)

}
