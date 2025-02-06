merge_and_rescale_weights <- function(port_weights_m_d_ref, updated_port_weights_m_lstd_ref, stock_universe_m_d_ref = NULL, verbose = TRUE){

  #Get portfolio compositions
  ###########################
    ##Lagged Universe (use portfolio with last composition but updated weights)
    tickers_old_universe <- updated_port_weights_m_lstd_ref %>% dplyr::pull(tickers)
    tickers_old_portfolio <- updated_port_weights_m_lstd_ref %>% dplyr::filter(bop_port_weights > 0) %>% dplyr::pull(tickers)

    ##Current Universe
    tickers_current_universe <- port_weights_m_d_ref %>% dplyr::pull(tickers)

    ##Tickers in common
    tickers_both_universes <- dplyr::intersect(tickers_old_universe, tickers_current_universe)

    ##Delisted stocks
    delisted_tickers_old_universe <- dplyr::setdiff(tickers_old_universe, tickers_current_universe)
    delisted_tickers_old_portfolio <- dplyr::setdiff(tickers_old_portfolio, tickers_current_universe)

    ##IPOs (new tickers)
    ipo_tickers <- dplyr::setdiff(tickers_current_universe, tickers_old_universe)

      ###Print changes
      if (verbose){
        ###Deslisted tickers
        if (length(delisted_tickers_old_universe) != 0){
          cat("\n")
          message(paste0(
            "Delisted tickers: ", delisted_tickers_old_universe, ". Of those, the following were in the portfolio: ",
            if (length(delisted_tickers_old_portfolio) != 0) crayon::yellow(delisted_tickers_old_portfolio)))
        }
        ###IPOs
        if (length(ipo_tickers) != 0){
          cat("\n")
          message(paste("IPOs:", ipo_tickers))
        }
      }

  ###########################

  #Elaborate new portfolio
  ###########################
    ##If stock_universe_m_d_ref is not NULL, use new weights
    if (!is.null(stock_universe_m_d_ref)){
      port_weights_m_d_ref <- port_weights_m_d_ref %>%
        dplyr::left_join(stock_universe_m_d_ref %>% dplyr::select(id, weights), by = "id") %>% #Get rebalanced weights from stock_universe
        dplyr::mutate(eop_port_weights = weights) %>% #Make the from -> to
        dplyr::select(-weights) #Unselect weights
    } else {
    ##Otherwise, get updated weights from last period
      port_weights_m_d_ref <- port_weights_m_d_ref %>%
        dplyr::left_join(updated_port_weights_m_lstd_ref %>% dplyr::select(tickers, bop_port_weights), by = "tickers") %>% #Get updated weights from last period
        dplyr::mutate(eop_port_weights = bop_port_weights) %>% #Make the from -> to
        dplyr::select(-bop_port_weights) #Unselect weights
    ##Rescale to 100%
      sum_weights <- sum(port_weights_m_d_ref$eop_port_weights[!is.na(port_weights_m_d_ref$eop_port_weights)])
      if (sum_weights == 0) stop("Sum of weights is 0. Can't rescale weights.")

      port_weights_m_d_ref <- port_weights_m_d_ref %>%
        dplyr::mutate(eop_port_weights =  #Change eop_port_weights
                        dplyr::if_else(is.na(eop_port_weights),
                                       0, #If there is a NA, it is an IPO stock
                                       eop_port_weights/sum_weights #Else rescale
                                       )
                      )
    }
      ###Check if weights sum to 1
      if ((sum(port_weights_m_d_ref$eop_port_weights, na.rm = TRUE) - 1) > 0.02) stop("Weights do not sum to 1.")
    ###########################

    return(port_weights_m_d_ref)
}
