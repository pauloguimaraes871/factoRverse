#' Calculate Portfolio Returns
#'
#' This function calculates the portfolio's forward returns, active returns, net returns, and turnover,
#' given stock- and benchmark-level forward returns, transactions information, and cost data.
#'
#' @param clean_fwd_return_1m_m_d_ref A data frame containing forward stock returns. Must include
#'   a column named \code{id} (or an equivalent identifier) and a column named \code{fwd_return_1m} for returns.
#' @param selected_benchmark_fwd_return Numeric. The forward return of the selected benchmark.
#' @param transactions_m_d_ref A data frame of transactions. Must include columns:
#'   \itemize{
#'     \item \code{id}: Identifier matching the stock IDs in \code{clean_fwd_return_1m_m_d_ref}.
#'     \item \code{eop_port_weights}: End-of-period portfolio weights for each stock.
#'     \item \code{delta}: Change in holdings (used for turnover calculation).
#'   }
#' @param total_direct_cost Numeric. The total direct cost of the portfolio's transactions.
#' @param total_market_impact_cost Numeric. The total market impact cost of the portfolio's transactions.
#' @param total_cost Numeric. The total cost of all transactions, typically \code{total_direct_cost + total_market_impact_cost}.
#' @param verbose Logical. If \code{TRUE}, the function may provide additional messages. Defaults to \code{TRUE}.
#'
#' @details
#' The function first joins \code{transactions_m_d_ref} with \code{clean_fwd_return_1m_m_d_ref} by \code{id}.
#' Any missing forward returns are replaced with 0.
#' It then calculates several return metrics:
#' \itemize{
#'   \item \strong{Raw Return:} Weighted sum of \code{fwd_return_1m}.
#'   \item \strong{Raw Active Return:} Raw Return minus the benchmark forward return.
#'   \item \strong{Net Return:} Raw Return minus the total cost.
#'   \item \strong{Net Active Return:} Net Return minus the benchmark forward return.
#'   \item \strong{Turnover:} Mean absolute value of \code{delta}.
#' }
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{transactions_m_d_ref}}{Data frame with the updated forward returns after joining with \code{clean_fwd_return_1m_m_d_ref}.}
#'   \item{\code{fwd_raw_return}}{Numeric. The raw weighted forward return.}
#'   \item{\code{fwd_raw_active_return}}{Numeric. The raw active return.}
#'   \item{\code{fwd_net_return}}{Numeric. The net return (subtracting total costs from the raw return).}
#'   \item{\code{fwd_net_active_return}}{Numeric. The net active return (subtracting the benchmark forward return from \code{fwd_net_return}).}
#'   \item{\code{turnover}}{Numeric. The average of the absolute \code{delta} values.}
#' }
#'
calculate_port_returns <- function(
  #Fwd Stock and Benchmark Returns
  clean_fwd_return_1m_m_d_ref, selected_benchmark_fwd_return,
  #Transactions
  transactions_m_d_ref,
  #Costs
  transactions_costs_m_xts_d_ref,
  #Misc
  verbose = TRUE
){

  #Initial prep
  ####################
    ##Extract costs
    total_direct_cost <- transactions_costs_m_xts_d_ref[, "total_direct_cost"] %>% as.numeric()
    total_market_impact_cost <- transactions_costs_m_xts_d_ref[, "total_market_impact_cost"] %>% as.numeric()
    total_cost <- transactions_costs_m_xts_d_ref[, "total_cost"] %>% as.numeric()

    ##Join target_m_d_ref to transactions_m_d_ref and treat NAs
    filled_transactions_m_d_ref <- transactions_m_d_ref %>% #Nmae change because transactions_m_d_ref is now complete
      dplyr::left_join(clean_fwd_return_1m_m_d_ref, by = "id") %>% #By id as transactions_m_d_ref contains data for past tickers
      dplyr::mutate(fwd_return_1m = dplyr::if_else(is.na(fwd_return_1m), 0, fwd_return_1m)) %>%
      dplyr::relocate(obs, .after = dplyr::last_col()) ##Re-order transactions

  ####################

  #Calculate Portfolio Returns
  ####################
    ##Raw
      ###Raw Return
      fwd_raw_return <- sum(transactions_m_d_ref$eop_port_weights * transactions_m_d_ref$fwd_return_1m, na.rm = TRUE)
      ###Raw Active Return
      fwd_raw_active_return <- fwd_raw_return - selected_benchmark_fwd_return

    ##Active
      ###Net Return
      fwd_net_return <- fwd_raw_return - total_cost
      ###Net Active Return
      fwd_net_active_return <- fwd_net_return - selected_benchmark_fwd_return

    ##Turnover
      turnover <- mean(abs(transactions_m_d_ref$delta))

      ####################

  #Print message
     ##Messages
     if(verbose){
       cat("\n")
       cat(crayon::green("Portfolio returns succesfully calculated:"))
       cat("\n")
       message("Raw Return: ", if (fwd_raw_return > 0) crayon::green(fwd_raw_return) else crayon::red(fwd_raw_return))
       message("Raw Active Return: ", if (fwd_raw_active_return > 0) crayon::green(fwd_raw_active_return) else crayon::red(fwd_raw_active_return))
       message("Net Return: ", if (fwd_net_return > 0) crayon::green(fwd_net_return) else crayon::red(fwd_net_return))
       message("Net Active Return: ", if (fwd_net_active_return > 0) crayon::green(fwd_net_active_return) else crayon::red(fwd_net_active_return))
       message("Turnover: ", turnover)
     }

  #Results
  port_returns_results_list <- list(
    filled_transactions_m_d_ref = filled_transactions_m_d_ref,
    fwd_raw_return = fwd_raw_return,
    fwd_raw_active_return = fwd_raw_active_return,
    fwd_net_return = fwd_net_return,
    fwd_net_active_return = fwd_net_active_return,
    turnover = turnover
  )


  return(port_returns_results_list)

}
