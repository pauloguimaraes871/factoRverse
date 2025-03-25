#' Create a signal_weighted portfolio for signals or stocks
#'
#' @param universe_m_d_ref A dataframe with identifiers (tickers or signal column), is_eligible and a weighting column as defined by signal_weighting metric.
#' This object could either be the result of filter_stock_universe or created in the context of the blend_signals_function
#'
#' @return
#' @export
#'
#' @examples
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
