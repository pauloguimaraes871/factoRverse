#' Create a custom_weighted portfolio for signals or stocks
#'
#' @param universe_m_d_ref A dataframe with identifiers (tickers or signal column), is_eligible and a weighting column as defined by signal_weighting metric.
#' This object could either be the result of filter_stock_universe or created in the context of the blend_signals_function
#' @param custom_weights_m_d_ref A dataframe with identifiers (tickers or signal column) and custom weights.
#' @param verbose If TRUE, will print messages to the console
create_custom_weighted_portfolio <- function(universe_m_d_ref, custom_weights_m_d_ref, verbose = TRUE){

  #Message
  if(verbose){
    tictoc::tic()
    cat("\n")
    cat("Deriving weights through custom weights")
  }

  #Check if custom weights are appropriate
  if (!"weights" %in% colnames(custom_weights_m_d_ref)){
    stop("Custom weights should contain a column named 'weights'")
  }
  if (abs(sum(custom_weights_m_d_ref$weights) - 1) > 0.05){
    stop("Custom weights should sum to 1")
  }
  if (abs(sum(custom_weights_m_d_ref$weights) - 1) > 0.02 &
      abs(sum(custom_weights_m_d_ref$weights) - 1) <= 0.05){
    warning("Custom weights do not sum to 1, but are within the acceptable range of 5%")
  }
  if (any(!universe_m_d_ref$tickers %in% custom_weights_m_d_ref$tickers)){
    stop("Custom weights should contain all tickers in the universe")
  }

  #Get custom weights
  custom_weights <- universe_m_d_ref %>% dplyr::select(tickers) %>% #Select only two colums
    dplyr::left_join(custom_weights_m_d_ref %>% dplyr::select(-id, -dates), by = "tickers")

  #Left Join back to portfolio
  universe_m_d_ref <- dplyr::left_join(universe_m_d_ref, custom_weights, by = "tickers") #Left join

  #Replace NAs with zeros
  universe_m_d_ref[which(is.na(universe_m_d_ref$weights)),"weights"] <- 0

  #Check for weights different from 1
  if (abs(sum(universe_m_d_ref$weights) - 1) > 0.05){
    stop("Weights do not sum to 1")
  }
  if (abs(sum(universe_m_d_ref$weights) - 1) > 0.02 &
      abs(sum(universe_m_d_ref$weights) - 1) <= 0.05){
    warning("Weights do not sum to 1, but are within the acceptable range of 5%")
  }

  #Message
  if(verbose){
    cat("\n")
    cat(crayon::green(paste("Custom-weights succesfully defined")))
    cat("\n")
    elapsed_time <- tictoc::toc()
  }

  #Return
  custom_weights_results_list <- list(
    universe_m_d_ref = universe_m_d_ref,
    weights = universe_m_d_ref$weights
  )

  return(custom_weights_results_list)
}
