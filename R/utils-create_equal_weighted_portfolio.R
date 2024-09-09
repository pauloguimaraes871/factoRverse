#' Create a equal_weighted portfolio for signals or stocks
#'
#' @param universe_m_d_ref A dataframe with identifiers (tickers or signal column), is_eligible and a weighting column as defined by signal_weighting metric.
#' This object could either be the result of filter_stock_universe or created in the context of the blend_signals_function
#'
#' @return
#' @export
#'
#' @examples
create_equal_weighted_portfolio <- function(universe_m_d_ref){

  #Calculate Equal-Weights
  ew_weights <- universe_m_d_ref %>% dplyr::select(tickers, is_eligible) %>% #Select only two colums
        dplyr::filter(is_eligible == 1) %>% #Filter only eligibles
        dplyr::mutate(weights = 1/sum(is_eligible)) %>% #Calculate equal-weights
        dplyr::select(-is_eligible) #Drop

  #Left Join back to portfolio
  universe_m_d_ref <- dplyr::left_join(universe_m_d_ref, ew_weights, by = "tickers") #Left join

  #Replace NAs with zeros
  universe_m_d_ref[which(is.na(universe_m_d_ref$weights)),"weights"] <- 0

  #Return
  return(universe_m_d_ref)
}
