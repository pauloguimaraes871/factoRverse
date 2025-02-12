#' Roll Portfolio Weights and Calculate Forward Returns
#'
#' This function calculates forward portfolio returns and rolls the current portfolio weights to the next period
#' based on forward one-month stock returns. It filters the target return data to remove missing values, computes
#' net portfolio returns (using a helper function), and then rolls the portfolio weights (using another helper function).
#'
#' @param fwd_return_m_d_ref A data frame containing stock return information with at least the columns:
#'   \code{id}, \code{tickers}, \code{dates}, and \code{fwd_return_1m} (forward 1-month returns).
#' @param fwd_selected_benchmark_return The forward return of the selected benchmark. This can be a numeric value or a data structure as required by the helper functions.
#' @param port_weights_m_d_ref A data frame of current portfolio weights.
#' @param total_cost A numeric value representing the total transaction cost.
#' @param verbose Logical; if \code{TRUE}, progress messages will be printed.
#'
#' @return A list with two components:
#'   \itemize{
#'     \item \code{rolled_fwd_port_weights_m_d_ref}: A data frame of portfolio weights rolled forward to the next period.
#'     \item \code{fwd_port_returns_d_ref}: A data frame (or numeric) containing the net forward portfolio returns.
#'   }
#'
#' @details
#' The function follows these steps:
#' \enumerate{
#'   \item Extracts the forward 1-month stock returns from \code{fwd_return_m_d_ref}, dropping any rows with missing values.
#'   \item If there are valid returns (i.e., at least one row remains), it prints a message (if \code{verbose} is \code{TRUE}),
#'         calculates the net portfolio return using \code{calculate_port_returns()}, and then rolls the portfolio weights
#'         to the next period using \code{roll_fwd_port_weights()}.
#'   \item If no valid forward returns are available, both the returns and rolled weights are set to \code{NULL}.
#' }
#'
roll_port <- function(
    #Return information
    fwd_return_m_d_ref, fwd_selected_benchmark_return,
    #Portfolio weights
    port_weights_m_d_ref,
    #Transaction costs
    total_cost,
    #Misc
    verbose
    ){

  #Calculate returns and roll portfolio weights
  ############################

    ##Calculate port returns
      ###Get fwd_1m stock returns
      clean_fwd_return_1m_m_d_ref <- fwd_return_m_d_ref %>%
        dplyr::select(id, tickers, dates, fwd_return_1m) %>% #Select only relevant columns
        tidyr::drop_na(fwd_return_1m) #Filter out NA values

      ###Checks if it is an up-to-date month
      if (nrow(clean_fwd_return_1m_m_d_ref) > 0){

        ####Print
        if(verbose){
          cat("\n")
          cat("Rolling portfolio to next period and calculating net portfolio return")
        }

        ####Calculate portfolio returns
        fwd_port_returns_d_ref <- calculate_port_returns(
          #Fwd Stock and Benchmark Returns
          clean_fwd_return_1m_m_d_ref = clean_fwd_return_1m_m_d_ref, fwd_selected_benchmark_return = fwd_selected_benchmark_return,
          #Transactions
          port_weights_m_d_ref = port_weights_m_d_ref,
          #Costs
          total_cost = total_cost,
          #Misc
          verbose = verbose
        )

        ####Roll portfolio weights to next period
        rolled_fwd_port_weights_m_d_ref <- roll_fwd_port_weights(
          #Port Weights
          port_weights_m_d_ref = port_weights_m_d_ref,
          #Port Returns
          clean_fwd_return_1m_m_d_ref = clean_fwd_return_1m_m_d_ref
        )

      } else {

        ####Print
        if(verbose){
          cat("\n")
          message("End of backtest. No more dates to roll port")
        }

        fwd_port_returns_d_ref <- NULL
        rolled_fwd_port_weights_m_d_ref <- NULL
      }

      ############################

      #Return results
      rolled_port_results_list <- list(
        rolled_fwd_port_weights_m_d_ref = rolled_fwd_port_weights_m_d_ref,
        fwd_port_returns_d_ref = fwd_port_returns_d_ref
      )

      return(rolled_port_results_list)


      ####################


}
