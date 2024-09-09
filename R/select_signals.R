#' Select and Correct Signal Positions
#'
#' This function selects signals based on a given policy, adjusts their positions according to the policy, and validates the data against backtest returns.
#'
#' @param signal_selection_policy A list containing the following elements:
#'   \itemize{
#'     \item \code{chosen_signals}: A character vector of selected signal names.
#'     \item \code{signals_position}: A named character vector where names are signal names and values are their positions ("long" or "short").
#'   }
#' @param signals_m_upd_ref A data frame with columns including "id", "tickers", "dates", and the selected signals. This data frame contains the current signal data.
#' @param backtest_returns_upd_ref A data frame with columns "dates" and the signals, containing historical backtest return data.
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Extracts the chosen signals from \code{signals_m_upd_ref} and subsets the data frame to include only these signals.
#'   \item Checks for consistency between the length of \code{chosen_signals} and \code{signals_position} and ensures that all chosen signals have corresponding positions.
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
  chosen_signals <- signal_selection_policy$chosen_signals

  ###selected_signals_m_upd_ref
  selected_signals_m_upd_ref <- signals_m_upd_ref[, c("id", "tickers", "dates", chosen_signals)] #subset cols present in signals_m_upd_ref

  ###Inform short positions
  ###Check if signals and signals_position have the same length
  if(length(chosen_signals) != length(signals_positions)){
    stop("signals and signals_positions should have the same length.")
  }
  ###Check if all signals have a position
  if(!all(chosen_signals == names(signals_positions))){
    stop("all signals should have a matching position in signals_positions.")
  }

  ###Get positions
  signals_positions <- signal_selection_policy$signals_position #Get signals positions
  chosen_short_signals <- chosen_signals[which(signals_positions == "short")] #Get who is short
  chosen_signals_corrected_positions <- chosen_signals #Init object
  chosen_signals_corrected_positions[which(chosen_signals == chosen_short_signals)] <- paste0("low_", chosen_signals[which(chosen_signals == chosen_short_signals)]) #Inform short positions

  ####Correct positions
  selected_signals_corrected_positions_m_upd_ref <- selected_signals_m_upd_ref
  ###Invert sign of short signals
  selected_signals_corrected_positions_m_upd_ref[, chosen_short_signals] <- selected_signals_corrected_positions_m_upd_ref[, chosen_short_signals]*-1
  ###Change colnames
  colnames(selected_signals_corrected_positions_m_upd_ref)[-c(1:3)] <- chosen_signals_corrected_positions


  #signals_backtests
  selected_signals_backtest_returns_upd_ref <- backtest_returns_upd_ref[, c("dates", chosen_signals_corrected_positions)]

  if(!all(chosen_signals_corrected_positions == colnames(selected_signals_backtest_returns_upd_ref[-1]))){
    stop("all signals should have a matching position in backtests_df")
  }


selected_signals_and_backtest_list <- list(
  selected_signals_corrected_positions_m_upd_ref = selected_signals_corrected_positions_m_upd_ref,
  selected_signals_backtest_returns_upd_ref = selected_signals_backtest_returns_upd_ref
)



}
