#' Calculate Portfolio Returns
#'
#' This function calculates the portfolio's forward returns, active returns, net returns, and turnover,
#' given stock- and benchmark-level forward returns, transactions information, and cost data.
#'
#' @param clean_fwd_return_1m_m_d_ref A data frame containing forward stock returns. Must include
#'   a column named \code{id} (or an equivalent identifier) and a column named \code{fwd_return_1m} for returns.
#' @param fwd_selected_benchmark_return Numeric. The forward return of the selected benchmark.
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
  clean_fwd_return_1m_m_d_ref, fwd_selected_benchmark_return,
  #Transactions
  port_weights_m_d_ref,
  #Costs
  total_cost,
  #Misc
  verbose = TRUE
){

  #Initial prep
  ####################
  ##Join target_m_d_ref to port_weights_m_d_ref
  port_weights_and_fwd_returns_m_d_ref <- port_weights_m_d_ref %>% dplyr::left_join(clean_fwd_return_1m_m_d_ref, by = "id") #By id

  ####################

  #Calculate Portfolio Returns
  ####################
  ##Raw
  ###Raw Return
  fwd_raw_return <- sum(port_weights_and_fwd_returns_m_d_ref$eop_port_weights * port_weights_and_fwd_returns_m_d_ref$fwd_return_1m, na.rm = TRUE)
  ###Raw Active Return
  if (!is.null(fwd_selected_benchmark_return)){
    fwd_raw_active_return <- fwd_raw_return - fwd_selected_benchmark_return
  } else {
    fwd_raw_active_return <- NA
  }


  ##Net
  ###Net Return
  fwd_net_return <- fwd_raw_return - total_cost
  ###Net Active Return
  if (!is.null(fwd_selected_benchmark_return)){
  fwd_net_active_return <- fwd_net_return - fwd_selected_benchmark_return
  } else {
    fwd_net_active_return <- NA
  }

  ##Aggregate
  fwd_port_returns_d_ref <- data.frame(fwd_raw_return = fwd_raw_return, fwd_net_return = fwd_net_return) #Raw and Net Returns
  if (!is.null(fwd_selected_benchmark_return)){
    fwd_port_returns_d_ref <- fwd_port_returns_d_ref %>%
      dplyr::mutate(fwd_selected_bench_return = fwd_selected_benchmark_return, #Benchmark Returns
                    fwd_raw_active_return = fwd_raw_active_return, #Active Raw Returns
                    fwd_net_active_return = fwd_net_active_return #Active Net Returns
                    )
  }


  ####################

  #Print message
  ##Messages
  if(verbose){
    cat("\n")
    cat(crayon::green("Portfolio returns:"))
    cat("\n")
    ###Raw Returns
    message("Raw Return: ", if (fwd_raw_return > 0) crayon::green(round(fwd_raw_return, 2)) else crayon::red(round(fwd_raw_return, 2)))
    if (is.null(fwd_selected_benchmark_return)){
      message("Raw Active Return: ", crayon::red("No benchmark selected"))
    } else {
      message("Raw Active Return: ", if (fwd_raw_active_return > 0) crayon::green(round(fwd_raw_active_return, 2)) else crayon::red(round(fwd_raw_active_return, 2)))
    }
    ###Net Returns
    message("Net Return: ", if (fwd_net_return > 0) crayon::green(round(fwd_net_return, 2)) else crayon::red(round(fwd_net_return, 2)))
    if (is.null(fwd_selected_benchmark_return)){
      message("Net Active Return: ", crayon::red("No benchmark selected"))
    } else {
    message("Net Active Return: ", if (fwd_net_active_return > 0) crayon::green(round(fwd_net_active_return, 2)) else crayon::red(round(fwd_net_active_return, 2)))
    }
  }

  #Results
  return(fwd_port_returns_d_ref)

}
