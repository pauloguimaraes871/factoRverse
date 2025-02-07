#' Calculate Trade Orders
#'
#' @description
#' This function calculates the required trades to move from a beginning-of-period (BOP)
#' portfolio weight to an end-of-period (EOP) portfolio weight, given liquidity and volatility data.
#'
#' @param merged_port_results_list A list of results from merge_and_rescale_weights.
#' @param stock_universe_m_d_ref optional data frame containing the new universe of rebalanced weights (columns \code{tickers}, \code{weights}).
#' @param liquidity_m_d_ref data frame containing liquidity information; must have a column matching \code{main_liquidity_metric}.
#' @param volatility_m_d_ref data frame containing volatility information; must contain \code{daily_vol}.
#' @param strategy_aum numeric, the AUM used to size trades.
#' @param main_liquidity_metric character string naming the column in \code{liquidity_m_d_ref} with liquidity data.
#' @param verbose logical. If TRUE, prints IPO/delisting info to console.
#'
#' @return A data frame with columns including \code{tickers}, \code{bop_port_weights}, \code{eop_port_weights},
#'   \code{delta}, \code{order}, \code{relative_order_size}, and any liquidity/volatility columns.
#' @export
#'
calculate_trade_orders <- function(merged_port_results_list,
                                   updated_port_weights_m_lstd_ref,
                                   liquidity_m_d_ref, volatility_m_d_ref,
                                   main_liquidity_metric, strategy_aum,
                                   verbose = TRUE
){

  #Get objects from merged_port_results
  ###########################
  port_weights_m_d_ref <- merged_port_results_list$port_weights_m_d_ref
  delisted_tickers_old_universe <- merged_port_results_list$delisted_tickers_old_universe
  ipo_tickers <- merged_port_results_list$ipo_tickers
  ###########################

  #Calculate transactions needed
  ###########################
  ##Create a rebalancing dataframe to support calculations
  transactions_m_d_ref <- port_weights_m_d_ref %>%
    dplyr::left_join(liquidity_m_d_ref %>% dplyr::select(-id, -dates), by = "tickers") %>% #Join liquidity_m_d_ref
    dplyr::left_join(volatility_m_d_ref %>% dplyr::select(-id, -dates), by = "tickers") %>% #Join volatility_m_d_ref
    dplyr::full_join(updated_port_weights_m_lstd_ref %>% dplyr::select(tickers, bop_port_weights), by = "tickers") #Full join in order to consider delisted stocks

  ##Add details
  transactions_m_d_ref$obs <- "none"
  transactions_m_d_ref$obs[which(transactions_m_d_ref$tickers %in% delisted_tickers_old_universe)] <- "delisted"
  transactions_m_d_ref$obs[which(transactions_m_d_ref$tickers %in% ipo_tickers)] <- "IPO"

  ##Treat NAs
  ###Delisted Tickers
  ####remove NAs in weights with 0 (NAs are possible if stocks are delisted (were present in last portfolio and not in current)
  transactions_m_d_ref$eop_port_weights[which(is.na(transactions_m_d_ref$eop_port_weights))] <- 0
  ####replace NAs in liquidity with low quantile (more conservative for a deslisting stock)
  transactions_m_d_ref <- transactions_m_d_ref %>%
    dplyr::mutate(!!rlang::sym(main_liquidity_metric) := dplyr::if_else( #Change main_liquidity_metric
      is.na(!!rlang::sym(main_liquidity_metric)), #If it is NA
      quantile(!!rlang::sym(main_liquidity_metric), 0.25, na.rm = TRUE) %>% as.numeric(), #Replace it with the 25% quantile
      !!rlang::sym(main_liquidity_metric) #Else keep the metric
    ))
  ####replace NAs in volatility_m_d_ref with median (conservative, as, usually, when there is an OPA, there is a pre-defined price that limits stock vol)
  transactions_m_d_ref <- transactions_m_d_ref %>%
    dplyr::mutate(daily_vol = dplyr::if_else( #Change daily_vol
      is.na(daily_vol), #If it is NA
      median(daily_vol, na.rm = TRUE) %>% as.numeric(), #Replace it with the median
      daily_vol #Else keep the metric
    ))
  ####replace ids and dates for delisted tickers
  transactions_m_d_ref <- transactions_m_d_ref %>%
    dplyr::mutate(dates = dplyr::if_else(obs == "delisted",
                                         updated_port_weights_m_lstd_ref %>% dplyr::pull(dates) %>% unique(), #Replace dates for delisted stocks as last date
                                         dates #Else do nothing
    )) %>%
    dplyr::mutate(id = dplyr::if_else(obs == "delisted",
                                      paste0(tickers, "-", dates), #Replace id for delisted stocks as last id
                                            id #Else do nothing
          ))
         ####replace remaining NAs in all columns with 0
         transactions_m_d_ref <- transactions_m_d_ref %>%
           dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0)))


         ###IPO Tickers
         ####remove NAs in bop weights with 0 (NAs are possible if stocks are IPOs
         transactions_m_d_ref$bop_port_weights[which(is.na(transactions_m_d_ref$bop_port_weights))] <- 0



         ###########################

         #Add order data
         ###########################
         transactions_m_d_ref <- transactions_m_d_ref %>%
           dplyr::mutate(delta = eop_port_weights - bop_port_weights) %>%
           dplyr::mutate(order = delta*strategy_aum) %>%
           dplyr::mutate(relative_order_size = abs(order)/!!rlang::sym(main_liquidity_metric))


         ###########################

         return(transactions_m_d_ref)

}
