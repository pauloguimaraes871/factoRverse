#' Create a equal_weighted portfolio for signals or stocks
#'
#' @param universe_m_d_ref A dataframe with identifiers (tickers or signal column), is_eligible and a weighting column as defined by signal_weighting metric.
#' This object could either be the result of filter_stock_universe or created in the context of the blend_signals_function
#'
#' @return
#' @export
#'
#' @examples
create_equal_weighted_portfolio <- function(universe_m_d_ref, verbose = TRUE){

  #Message
  if(verbose){
    tictoc::tic()
    cat("\n")
    cat("Deriving weights through EW")
  }


  #Calculate Equal-Weights
  ew_weights <- universe_m_d_ref %>% dplyr::select(tickers, is_eligible) %>% #Select only two colums
    dplyr::filter(is_eligible == 1) %>% #Filter only eligibles
    dplyr::mutate(weights = 1/sum(is_eligible)) %>% #Calculate equal-weights
    dplyr::select(-is_eligible) #Drop

  #Left Join back to portfolio
  universe_m_d_ref <- dplyr::left_join(universe_m_d_ref, ew_weights, by = "tickers") #Left join

  #Replace NAs with zeros
  universe_m_d_ref[which(is.na(universe_m_d_ref$weights)),"weights"] <- 0

  #Check for weights different from 1
  if (abs(sum(universe_m_d_ref$weights) - 1) > 0.02){
    stop("Weights do not sum to 1")
  }

  #Message
  if(verbose){
    cat("\n")
    cat(crayon::green(paste("Equal-weights succesfully defined")))
    cat("\n")
    tictoc::toc()
  }

  #Return
  ew_results_list <- list(
    universe_m_d_ref = universe_m_d_ref,
    weights = universe_m_d_ref$weights
  )

  return(ew_results_list)
}
