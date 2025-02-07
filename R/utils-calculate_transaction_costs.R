#' Calculate Transaction Costs (Including Market Impact)
#'
#' This function estimates the total transaction costs for a set of trades,
#' incorporating both direct transaction costs and market impact costs.
#' The market impact cost follows a \emph{Barra}-style functional form,
#' controlled by the parameters \code{alpha} and \code{lambda}, where
#' \code{relative_order_size} is raised to the power of \code{lambda}.
#'
#' @param transactions_m_d_ref A data frame containing transaction information.
#'   It must include (at least) the following columns:
#'   \describe{
#'     \item{\code{order}}{The number of shares (or units) traded in each transaction.}
#'     \item{\code{strategy_aum}}{The total Assets Under Management (AUM) for the strategy (used for scaling).}
#'     \item{\code{relative_order_size}}{Relative size of the order (e.g., \code{order / shares_outstanding}, or some
#'       other standard measure that captures the trade size relative to liquidity).}
#'     \item{\code{daily_vol}}{Some measure of daily volatility or impact factor for each stock.}
#'   }
#' @param alpha A numeric value that scales the market impact cost. Typically, \code{alpha} is determined by
#'   the liquidity or volatility of the market.
#' @param lambda A numeric value or the string \code{"dynamic"}.
#'   \describe{
#'     \item{Numeric value}{If a single numeric value is provided, the exponent \code{lambda} is applied uniformly to all trades.}
#'     \item{\code{"dynamic"}}{If set to \code{"dynamic"}, then \code{lambda} is determined by \code{relative_order_size}:
#'       \itemize{
#'         \item \code{lambda = 1} when \code{relative_order_size} \eqn{\le 0.002}
#'         \item \code{lambda = 0.5} when \code{0.002 < relative_order_size \le 0.05}
#'         \item \code{lambda = 0.25} when \code{0.05 < relative_order_size \le 0.1}
#'         \item \code{lambda = 0.1} when \code{relative_order_size > 0.1}
#'       }
#'     }
#'   }
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
#' @return A named list with three components:
#'   \describe{
#'     \item{\code{total_direct_cost}}{Sum of direct transaction costs over all rows.}
#'     \item{\code{total_market_impact_cost}}{Sum of market impact costs over all rows.}
#'     \item{\code{total_cost}}{Total transaction cost, i.e. direct + market impact.}
#'   }
#'
calculate_transaction_costs <- function(transactions_m_d_ref,
                                        alpha, lambda,
                                        direct_transaction_cost,
                                        strategy_aum){

  #Get BARRA Model Parameters
  ##########################
    ####Alpha
    transactions_m_d_ref[,"alpha"] <- alpha
    ####Lambda
    if (lambda == "dynamic"){
        #Dynamically adjust lambda according to trade size
        transactions_m_d_ref <- transactions_m_d_ref %>%
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
        transactions_m_d_ref[,"lambda"] <- lambda
      }

    ##########################

    #Calculate Transaction Costs
    ##########################
    transactions_m_d_ref <- transactions_m_d_ref %>%
      dplyr::mutate(direct_cost = (direct_transaction_cost * abs(order))/strategy_aum) %>%
      dplyr::mutate(market_impact_cost = alpha/2*(relative_order_size^lambda)*daily_vol) %>% ##Barra Model
      dplyr::mutate(total_cost = direct_cost + market_impact_cost)
    ##########################

    ##Get costs
    ##########################
    total_direct_cost <- sum(transactions_m_d_ref$direct_cost)
    total_market_impact_cost <- sum(transactions_m_d_ref$market_impact_cost)
    total_cost <- total_direct_cost + total_market_impact_cost

    ##########################

    ##Get brokerage statement
    brokerage_statement_list <- list(
      transactions_m_d_ref = transactions_m_d_ref,
      total_direct_cost = total_direct_cost,
      total_market_impact_cost = total_market_impact_cost,
      total_cost = total_cost
    )

    return(brokerage_statement_list)


}
