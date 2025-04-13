#' Create a Signal-Weighted Portfolio for Signals or Stocks
#'
#' This function calculates portfolio weights based on signal scores (e.g., expected return scores) for eligible assets. It is typically used after applying selection filters to the investment universe.
#'
#' @param universe_m_d_ref A data.frame containing the investment universe with the following columns:
#' \describe{
#'   \item{\code{tickers}}{Asset or signal identifier.}
#'   \item{\code{is_eligible}}{Binary flag (0/1) indicating if the asset is eligible for inclusion.}
#'   \item{\code{exp_ret_score}}{Expected return score or signal strength, used to derive portfolio weights.}
#' }
#' This object can be the result of \code{filter_stock_universe()} or constructed inside a custom signal blending function.
#'
#' @param verbose Logical. If \code{TRUE}, prints timing and status messages to the console using \code{tictoc} and \code{crayon}.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{universe_m_d_ref}}{The original data frame augmented with the computed weights.}
#'   \item{\code{weights}}{A numeric vector of portfolio weights summing to 1 (within tolerance).}
#'   \item{\code{exp_ret_score}}{The original expected return score vector used to define the weights.}
#' }
#'
create_signal_weighted_portfolio <- function(universe_m_d_ref, verbose = TRUE){

  #Message
  if(verbose){
    tictoc::tic()
    cat("\n")
    cat(paste0("Deriving weights through SW."))
  }


  #Calculate Signal-Weights
  sw_weights <- universe_m_d_ref %>% dplyr::select(tickers, is_eligible, exp_ret_score) %>% #Select only two colums
    dplyr::mutate(weights = dplyr::if_else(is_eligible == 1, exp_ret_score/sum(exp_ret_score[is_eligible == 1]), 0)) %>% #Calculate weights based on exp_ret_score
    dplyr::select(-is_eligible, -exp_ret_score)


  #Left Join back to portfolio
  universe_m_d_ref <- dplyr::left_join(universe_m_d_ref, sw_weights, by = "tickers") #Left join

  #Check for weights different from 1
  if (abs(sum(universe_m_d_ref$weights) - 1) > 0.02){
    stop("Weights do not sum to 1")
  }

  #Message
  if(verbose){
    cat("\n")
    cat(crayon::green(paste("Signal weights succesfully defined")))
    cat("\n")
    tictoc::toc()
  }


  #Return
  sw_results_list <- list(
    universe_m_d_ref = universe_m_d_ref,
    weights = universe_m_d_ref$weights,
    exp_ret_score = universe_m_d_ref$exp_ret_score
  )

  return(sw_results_list)
}
