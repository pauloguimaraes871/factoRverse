#' Select and Correct Signal Positions
#'
#' This function selects signals based on a given policy, adjusts their positions according to the policy, and validates the data against backtest returns.
#'
#' @param signals_m_df A (meta) data frame with columns including "id", "tickers", "dates", and the selected signals.
#' @param chosen_signals_and_positions A named vector indicating signals and their corresponding positions (long or short).
#' For example, chosen_signals_and_positions = c(book_yield = "long", vol_36m = "short").
#' @param signal_themes_m_df A (meta) data frame with "id", "tickers" ("signals"), and "dates" columns, including all signals in `signals_m_df`, and a "theme" column providing group membership for each signal.
#' @param backtest_returns_m_xts A xts containing historical backtested returns named according to signals in `signals_m_df`,
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Extracts the chosen signals from \code{signals_m_df} and subsets the data frame to include only these signals.
#'   \item Checks for consistency between the length of \code{chosen_signals} and \code{signal_positions} and ensures that all chosen signals have corresponding positions.
#'   \item Adjusts the signal positions based on whether they are "short" by multiplying their values by -1.
#'   \item Updates column names in the data frame to reflect the corrected positions of the signals.
#'   \item Validates that all adjusted signals have corresponding columns in \code{backtest_returns_df}.
#' }
#'
#' @return A list with two components:
#' \itemize{
#'   \item \code{selected_signals_corrected_positions_m_df}: The updated data frame from \code{signals_m_df} with corrected signal positions and adjusted column names.
#'   \item \code{selected_backtest_returns_corrected_positions_df}: The data frame from \code{backtest_returns_df} with columns matching the corrected signal positions.
#' }
#'
#' @seealso \code{\link{backtest_returns_df}}
#' @importFrom dplyr select
#' @importFrom stats setNames
#' @export
select_and_correct_signals <- function(signals_m_df, chosen_signals_and_positions, signal_themes_m_df = NULL, backtest_returns_m_xts = NULL){

  ###Get chosen signals
  #####################
  chosen_signals <- names(chosen_signals_and_positions) #Get chosen signals
  signal_positions <- unname(chosen_signals_and_positions) #Get signal positions

  ###selected_signals_m_df
    ###Check if all signals are in signals_m_df
    if(!any(chosen_signals %in% colnames(signals_m_df)[-c(1:3)])){
      stop("all chosen signals should have a matching position in signals_m_df")
    }

  selected_signals_m_df <- signals_m_df %>% dplyr::select(id, tickers, dates, dplyr::all_of(chosen_signals)) #subset cols present in signals_m_df
  #####################

  ###Inform short positions
  #####################
  ###Get positions
  chosen_short_signals <- chosen_signals[which(signal_positions == "short")] #Get who is short
  chosen_signals_corrected_positions <- chosen_signals #Init object
  chosen_signals_corrected_positions[which(chosen_signals %in% chosen_short_signals)] <- paste0("low_", chosen_signals[which(chosen_signals %in% chosen_short_signals)]) #Inform short positions


  ####Correct positions
  selected_signals_corrected_positions_m_df <- selected_signals_m_df
  ###Invert sign of short signals
  selected_signals_corrected_positions_m_df <- selected_signals_corrected_positions_m_df %>% dplyr::mutate(dplyr::across(dplyr::all_of(chosen_short_signals), ~ . * -1))
  ###Change colnames
  colnames(selected_signals_corrected_positions_m_df)[-c(1:3)] <- chosen_signals_corrected_positions


  #####################

  ###Subset signal_themes
  ########################
  if(!is.null(signal_themes_m_df)){
  ###Check if all signals have a theme
  if(!all(chosen_signals_corrected_positions %in% unique(signal_themes_m_df %>% dplyr::pull(tickers)))){
    stop("all chosen signals should have a matching position in signal_themes_m_df")
  }


  selected_signal_themes_m_df <- signal_themes_m_df %>% dplyr::filter(tickers %in% chosen_signals_corrected_positions)
  } else {
    selected_signal_themes_m_df <- NULL
  }

  ###Subset backtests
  #######################
  if(!is.null(backtest_returns_m_xts)){
    ###Check if all signals have a backtest
    if(!all(chosen_signals_corrected_positions %in% colnames(backtest_returns_m_xts))){
      stop("all chosen signals should have a matching position in backtest_returns_m_xts")
    }

    #signals_backtests
    selected_backtest_returns_corrected_positions_m_xts <- backtest_returns_m_xts[, chosen_signals_corrected_positions]
  } else {
    selected_backtest_returns_corrected_positions_m_xts <- NULL
  }

  #######################


  #Returns
  selected_signals_and_backtest_list <- list(
    selected_signals_corrected_positions_m_df = selected_signals_corrected_positions_m_df,
    selected_signal_themes_m_df = selected_signal_themes_m_df,
    selected_backtest_returns_corrected_positions_m_xts = selected_backtest_returns_corrected_positions_m_xts
  )

  return(selected_signals_and_backtest_list)

}
