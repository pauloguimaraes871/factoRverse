#' Calculate Portfolio Returns
#'
#' This function calculates the portfolio's forward returns, active returns, net returns, and turnover,
#' given stock- and benchmark-level forward returns, transactions information, and cost data.
#'
#' @param clean_fwd_return_1m_m_d_ref A data frame containing forward stock returns. Must include
#'   a column named \code{id} (or an equivalent identifier) and a column named \code{fwd_return_1m} for returns.
#' @param fwd_selected_benchmark_return Numeric. The forward return of the selected benchmark.
#' @param port_weights_m_d_ref A data frame containing portfolio weights. Must include an \code{id} column (for the join) and an \code{eop_port_weights} column (end-of-period weights used to weight forward returns).
#' @param total_cost Numeric. The total cost associated with the portfolio.
#' @param verbose Logical. If `TRUE`, messages will be printed about the calculation process.
#'
#' @details
#' The function joins \code{port_weights_m_d_ref} with \code{clean_fwd_return_1m_m_d_ref} by \code{id} and computes:
#' \itemize{
#'   \item \strong{Raw Return:} Weighted sum of \code{fwd_return_1m} using \code{eop_port_weights} (NA returns are ignored).
#'   \item \strong{Net Return:} Raw Return minus \code{total_cost}.
#'   \item \strong{Raw Active Return:} Raw Return minus the benchmark forward return (only when \code{fwd_selected_benchmark_return} is supplied).
#'   \item \strong{Net Active Return:} Net Return minus the benchmark forward return (only when \code{fwd_selected_benchmark_return} is supplied).
#' }
#'
#' @return A one-row data frame (\code{fwd_port_returns_d_ref}) with columns \code{fwd_raw_return} and
#'   \code{fwd_net_return}. When \code{fwd_selected_benchmark_return} is supplied, it also includes
#'   \code{fwd_selected_bench_return}, \code{fwd_raw_active_return}, and \code{fwd_net_active_return}.
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
