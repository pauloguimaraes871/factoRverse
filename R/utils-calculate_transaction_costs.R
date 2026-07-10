#' Calculate Transaction Costs (Including Market Impact)
#'
#' @description
#' Estimates total transaction costs for a set of trades, incorporating both direct and market impact costs.
#'
#' @param transactions_m_d_ref A data frame containing transaction data. Must include the columns: \code{order}, \code{relative_order_size}, \code{daily_vol}, and \code{delta} (the last used to compute turnover).
#' @param alpha Numeric. Scaling factor for market impact cost.
#' @param lambda Either a numeric value or the string \code{"dynamic"}. When \code{"dynamic"}, the lambda value varies based on the relative order size.
#' @param direct_transaction_cost Numeric. Direct transaction cost applied per trade (e.g., 0.0005 for 0.05%).
#' @param strategy_aum Numeric. The total assets under management for the strategy.
#' @param verbose Logical. If \code{TRUE}, prints detailed transaction cost information.
#'
#' @details
#' The function computes two cost components: a direct cost based on the trade size and a market impact cost based on the relative order size and volatility.
#'
#' When \code{lambda = "dynamic"}, lambda is set according to the relative order size, using predefined thresholds.
#'
#' @return A named list with two components:
#' \describe{
#'   \item{\code{transactions_and_costs_m_d_ref}}{The input transactions augmented with per-trade \code{alpha}, \code{lambda}, \code{direct_cost}, \code{market_impact_cost} and \code{total_cost} columns.}
#'   \item{\code{port_costs_d_ref}}{A one-row data frame with portfolio-level \code{direct_cost}, \code{market_impact_cost}, \code{total_cost} and \code{turnover}.}
#' }
calculate_transaction_costs <- function(transactions_m_d_ref,
                                        alpha, lambda,
                                        direct_transaction_cost,
                                        strategy_aum,
                                        verbose){

  #Init transactions_and_costs_m_d_ref
  transactions_and_costs_m_d_ref <- transactions_m_d_ref

  #Get BARRA Model Parameters
  ##########################
  ####Alpha
  transactions_and_costs_m_d_ref[,"alpha"] <- alpha
  ####Lambda
  if (lambda == "dynamic"){
    #Dynamically adjust lambda according to trade size
    transactions_and_costs_m_d_ref <- transactions_and_costs_m_d_ref %>%
      dplyr::mutate(lambda = dplyr::case_when(
        ##For very small trades, use lambda = 1
        relative_order_size <= 0.002 ~ 1,
        ##For small trades, use lambda = 0.5
        relative_order_size <= 0.05 ~ 0.5,
        ##For medium trades, use lambda = 0.25
        relative_order_size <= 0.1 ~ 0.25,
        ##For large trades, use lambda = 0.1
        TRUE ~ 0.1
      ))
  } else {
    transactions_and_costs_m_d_ref[,"lambda"] <- lambda
  }

  ##########################

  #Calculate Transaction Costs
  ##########################
  transactions_and_costs_m_d_ref <- transactions_and_costs_m_d_ref %>%
    dplyr::mutate(direct_cost = (direct_transaction_cost * abs(order))/strategy_aum) %>%
    dplyr::mutate(market_impact_cost = (alpha*(relative_order_size^lambda)*daily_vol)*abs(order)/strategy_aum) %>% ##Barra Model
    dplyr::mutate(total_cost = direct_cost + market_impact_cost)
  ##########################

  ##Get costs
  ##########################
  direct_cost <- sum(transactions_and_costs_m_d_ref$direct_cost)
  market_impact_cost <- sum(transactions_and_costs_m_d_ref$market_impact_cost)
  total_cost <- direct_cost + market_impact_cost
  turnover <- sum(abs(transactions_and_costs_m_d_ref$delta))/2

  ###Aggregate costs
  port_costs_d_ref <- data.frame(
    direct_cost = direct_cost, #Direct Costs
    market_impact_cost = market_impact_cost, #Indirect costs
    total_cost = total_cost, #Total costs
    turnover = turnover #Turnover
  )

  ###Warns if total cost is too high
  if (total_cost > 1){
    warning("Total cost higher than 1.0%. Consider changing backtest parameters or implementing a stricter liquidity_floor_rule constraint.")
  }

  ##########################

  ###Print message
  ###Messages
  if(verbose){
    cat("\n")
    cat(crayon::green("Transaction costs:"))
    cat("\n")
    message("Total Direct Cost: ", crayon::red(round(direct_cost, 2)))
    message("Total Market Impact Cost: ", crayon::red(round(market_impact_cost, 2)))
    message("Total Cost: ", crayon::red(round(total_cost, 2)))
    message("Turnover: ", round(turnover, 2))
  }

  ##Get brokerage statement
  transaction_costs_results_list <- list(
    transactions_and_costs_m_d_ref = transactions_and_costs_m_d_ref,
    port_costs_d_ref = port_costs_d_ref
  )

  return(transaction_costs_results_list)


}
