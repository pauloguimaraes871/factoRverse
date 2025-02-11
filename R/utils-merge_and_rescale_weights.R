#' Merge and Rescale Portfolio Weights
#'
#' This function updates an existing set of portfolio weights by merging it with new or updated weights.
#' It identifies delisted and newly listed stocks (IPOs) across two universes (old and current) and optionally
#' prints the results. The final set of weights is then rescaled to ensure the sum of weights equals 1.
#' If the sum of weights is zero or significantly different from 1, the function stops with an error.
#'
#' @param port_weights_placeholder_m_d_ref A data frame containing the current universe of stocks
#'   (typically with columns \code{tickers}, \code{id}, and any other relevant columns for the current period).
#' @param updated_port_weights_m_lstd_ref A data frame containing the updated or lagged portfolio weights,
#'   (typically with columns \code{tickers} and \code{bop_port_weights}) referring to weights carried over from the last period.
#' @param stock_universe_m_d_ref A data frame (default \code{NULL}) representing a rebalanced set of stocks
#'   (typically with columns \code{id}, \code{weights}), which, if provided, is used directly to assign \code{eop_port_weights}.
#'   If \code{NULL}, the function uses the \code{bop_port_weights} from \code{updated_port_weights_m_lstd_ref}
#'   and rescales them to sum to 1.
#' @param verbose A logical value indicating whether to print information about delisted tickers and IPO tickers.
#'   Defaults to \code{TRUE}.
#'
#' @details
#' \describe{
#'   \item{\strong{Delisted Tickers}}{Stocks present in the old universe but not in the new universe.
#'   If these delisted tickers were part of the portfolio (i.e., had a weight > 0), they are also reported.}
#'   \item{\strong{IPO Tickers}}{Stocks present in the new universe but absent from the old universe, hence considered as newly listed.}
#'   \item{\strong{Rescaling}}{If \code{stock_universe_m_d_ref} is not \code{NULL}, \code{eop_port_weights}
#'   is taken from the \code{weights} column of \code{stock_universe_m_d_ref}. Otherwise,
#'   the function uses \code{bop_port_weights} from \code{updated_port_weights_m_lstd_ref}
#'   and rescales them such that the total weight sums to 1.}
#' }
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{port_weights_m_d_ref}}{A data frame with the updated and rescaled \code{eop_port_weights} column.}
#'   \item{\code{tickers_both_universes}}{A character vector of tickers present in both the old and current universes.}
#'   \item{\code{delisted_tickers_old_universe}}{A character vector of tickers from the old universe that are no longer present in the current universe.}
#'   \item{\code{delisted_tickers_old_portfolio}}{A character vector of delisted tickers that had a positive weight in the old portfolio.}
#'   \item{\code{ipo_tickers}}{A character vector of newly introduced tickers in the current universe (i.e., IPOs).}
#' }
#'
merge_and_rescale_weights <- function(port_weights_placeholder_m_d_ref, updated_port_weights_m_lstd_ref,
                                      selected_benchmark_weights_m_d_ref,
                                      stock_universe_m_d_ref = NULL, verbose = TRUE){

  #Get portfolio compositions
  ###########################
  ##Lagged Universe (use portfolio with last composition but updated weights)
  tickers_old_universe <- updated_port_weights_m_lstd_ref %>% dplyr::pull(tickers)
  tickers_old_portfolio <- updated_port_weights_m_lstd_ref %>% dplyr::filter(bop_port_weights > 0) %>% dplyr::pull(tickers)

  ##Current Universe
  tickers_current_universe <- port_weights_placeholder_m_d_ref %>% dplyr::pull(tickers)

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
        "Delisted tickers: ", paste0(delisted_tickers_old_universe, collapse = ", "), ". Of those, the following were in the portfolio: ",
        if (length(delisted_tickers_old_portfolio) != 0) crayon::yellow(paste0(delisted_tickers_old_portfolio, collapse = ", ")))
      )
    }
    ###IPOs
    if (length(ipo_tickers) != 0){
      cat("\n")
      message(paste("IPOs:", paste0(ipo_tickers, collapse = ", ")))
    }
  }

  ###########################

  #Elaborate new portfolio
  ###########################
  ##If stock_universe_m_d_ref is not NULL, use new weights
  if (!is.null(stock_universe_m_d_ref)){
    port_weights_m_d_ref <- port_weights_placeholder_m_d_ref %>%
      dplyr::left_join(stock_universe_m_d_ref %>% dplyr::select(id, weights), by = "id") %>% #Get rebalanced weights from stock_universe
      dplyr::mutate(eop_port_weights = weights) %>% #Make the from -> to
      dplyr::select(-weights) #Unselect weights
  } else {
    ##Otherwise, get updated weights from last period
    port_weights_m_d_ref <- port_weights_placeholder_m_d_ref %>%
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

  ##Add benchmark weights if not NULL
  ###########################
  if (!is.null(selected_benchmark_weights_m_d_ref)){
    port_weights_m_d_ref <- port_weights_m_d_ref %>%
      dplyr::left_join(selected_benchmark_weights_m_d_ref %>%
                         dplyr::select(-tickers, -dates) %>% #Unselect tickers and dates
                         dplyr::rename_with(~ "bench_weights", .cols = 2), #Rename bench_weights
                       by = "id") #Join bench_weights
  } else {
    port_weights_m_d_ref <- port_weights_m_d_ref
  }

  ###########################

  #Get all outputs
  merged_port_results_list <- list(
    port_weights_m_d_ref = port_weights_m_d_ref,
    tickers_both_universes = tickers_both_universes,
    delisted_tickers_old_universe = delisted_tickers_old_universe,
    delisted_tickers_old_portfolio = delisted_tickers_old_portfolio,
    ipo_tickers = ipo_tickers
  )

  return(merged_port_results_list)
}
