#' Calculate Transaction Costs (Including Market Impact)
#'
#' @description Estimates total transaction costs, including direct and market impact costs.
#'
#' This function estimates the total transaction costs for a set of trades,
#' incorporating both direct transaction costs and market impact costs.
#' The market impact cost follows a \emph{Barra}-style functional form,
#' controlled by the parameters \code{alpha} and \code{lambda}, where
#' \code{relative_order_size} is raised to the power of \code{lambda}.
#'
#' @param transactions_m_d_ref A data frame containing transaction information.
#'   It must include (at least) the following columns:
#'   \itemize{
#'     \item \code{order}: The number of shares (or units) traded in each transaction.
#'     \item \code{strategy_aum}: The total Assets Under Management (AUM) for the strategy (used for scaling).
#'     \item \code{relative_order_size}: Relative size of the order (e.g., \code{order / shares_outstanding}, or some
#'       other standard measure that captures the trade size relative to liquidity).
#'     \item \code{daily_vol}: Some measure of daily volatility or impact factor for each stock.
#'   }
#' @param alpha A numeric value that scales the market impact cost. Typically, \code{alpha} is determined by
#'   the liquidity or volatility of the market.
#' @param lambda A numeric value or the string \code{"dynamic"}. If set to \code{"dynamic"}, \code{lambda} is set
#'   based on \code{relative_order_size}:
#'   - lambda = 1 when relative_order_size ≤ 0.002
#'   - lambda = 0.5 when 0.002 < relative_order_size ≤ 0.05
#'   - lambda = 0.25 when 0.05 < relative_order_size ≤ 0.1
#'   - lambda = 0.1 when relative_order_size > 0.1
#'
#' @param direct_transaction_cost A numeric value representing the direct (e.g., brokerage)
#'   transaction cost percentage to be applied per trade (e.g., if \code{direct_transaction_cost = 0.0005}, this corresponds to 0.05\%).
#'
#' @details
#' The function computes two cost components:
#' \enumerate{
#'   \item \strong{Direct Cost}:
#'     \deqn{\text{direct\_cost} = \bigl(\text{direct\_transaction\_cost} \times |\,\text{order}\,|\bigr) \,/\,\text{strategy\_aum}}
#'   \item \strong{Market Impact Cost}:
#'     \deqn{\text{market\_impact\_cost} = \alpha \times \bigl(\text{relative\_order\_size}\bigr)^\lambda \times \text{daily\_vol}}
#' }
#'
#' @return A named list with three components:total_direct_cost, total_market_impact_cost and total_cost
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
