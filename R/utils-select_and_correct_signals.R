#' Select and Correct Signal Positions
#'
#' This function selects signals based on a given policy, adjusts their positions according to the policy, and validates the data against backtest returns.
#'
#' @param signal_selection_policy A list containing the following elements:
#'   \itemize{
#'     \item \code{chosen_signals}: A character vector of selected signal names.
#'     \item \code{signal_positions}: A named character vector where names are signal names and values are their positions ("long" or "short").
#'   }
#' @param signals_m_upd_ref A data frame with columns including "id", "tickers", "dates", and the selected signals. This data frame contains the current signal data.
#' @param backtest_returns_upd_ref A data frame with columns "dates" and the signals, containing historical backtest return data.
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Extracts the chosen signals from \code{signals_m_upd_ref} and subsets the data frame to include only these signals.
#'   \item Checks for consistency between the length of \code{chosen_signals} and \code{signal_positions} and ensures that all chosen signals have corresponding positions.
#'   \item Adjusts the signal positions based on whether they are "short" by multiplying their values by -1.
#'   \item Updates column names in the data frame to reflect the corrected positions of the signals.
#'   \item Validates that all adjusted signals have corresponding columns in \code{backtest_returns_upd_ref}.
#' }
#'
#' @return A list with two components:
#' \itemize{
#'   \item \code{selected_signals_corrected_positions_m_upd_ref}: The updated data frame from \code{signals_m_upd_ref} with corrected signal positions and adjusted column names.
#'   \item \code{selected_signals_backtest_returns_upd_ref}: The data frame from \code{backtest_returns_upd_ref} with columns matching the corrected signal positions.
#' }
#'
#' @seealso \code{\link{backtest_returns_upd_ref}}
#' @importFrom dplyr select
#' @importFrom stats setNames
#' @export
select_and_correct_signals <- function(signal_selection_policy, signals_m_upd_ref, backtest_returns_upd_ref){

  ###Get chosen signals
  #####################
  chosen_signals <- signal_selection_policy$chosen_signals

  ###Check if all chosen_signals are present in signals_m_upd_ref
  if(any(!chosen_signals %in% colnames(signals_m_upd_ref))){
    stop("signal selection not avaiable in signals_m_df")
  }

  ###Check if there are repeated signals in chosen_signals
  if(!identical(chosen_signals, unique(chosen_signals))){
    stop("each signal must be chosen only once")
  }

  ###selected_signals_m_upd_ref
  selected_signals_m_upd_ref <- signals_m_upd_ref[, c("id", "tickers", "dates", chosen_signals)] #subset cols present in signals_m_upd_ref
  #####################

  ###Inform short positions
  #####################
  ###Get positions
  signal_positions <- signal_selection_policy$signal_positions #Get signals positions
  chosen_short_signals <- chosen_signals[which(signal_positions == "short")] #Get who is short
  chosen_signals_corrected_positions <- chosen_signals #Init object
  chosen_signals_corrected_positions[which(chosen_signals == chosen_short_signals)] <- paste0("low_", chosen_signals[which(chosen_signals == chosen_short_signals)]) #Inform short positions

  ###Check if all signals have a position
  if(!identical(chosen_signals, names(signal_positions))){
    stop("all chosen signals should have a matching position in signal_positions.")
  }

  ####Correct positions
  selected_signals_corrected_positions_m_upd_ref <- selected_signals_m_upd_ref
  ###Invert sign of short signals
  selected_signals_corrected_positions_m_upd_ref[, chosen_short_signals] <- selected_signals_corrected_positions_m_upd_ref[, chosen_short_signals]*-1
  ###Change colnames
  colnames(selected_signals_corrected_positions_m_upd_ref)[-c(1:3)] <- chosen_signals_corrected_positions


  #####################

  ###Subset backtests
  #######################

  ###Check if all signals have a backtest
  if(!is.null(backtest_returns_upd_ref) & !all(chosen_signals_corrected_positions %in% colnames(backtest_returns_upd_ref[-1]))){
    stop("all chosen signals should have a matching position in backtest_returns_df")
  }

  #signals_backtests
  selected_signals_backtest_returns_upd_ref <- backtest_returns_upd_ref[, c("dates", chosen_signals_corrected_positions)]
  #######################


  #Returns
  selected_signals_and_backtest_list <- list(
    selected_signals_corrected_positions_m_upd_ref = selected_signals_corrected_positions_m_upd_ref,
    selected_signals_backtest_returns_upd_ref = selected_signals_backtest_returns_upd_ref
  )

  return(selected_signals_and_backtest_list)

}
