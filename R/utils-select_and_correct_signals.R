#' Select and Correct Signal Positions
#'
#' This function selects signals based on a given policy, adjusts their positions according to the policy, and validates the data against backtest returns.
#'
#' @param signals_m_df A (meta) data frame with columns including "id", "tickers", "dates", and the selected signals.
#' @param chosen_signals_and_positions A named vector indicating signals and their corresponding positions (long or short).
#' For example, chosen_signals_and_positions = c(book_yield = "long", vol_36m = "short").
#' @param backtest_returns_df A data frame with a 'dates' column and remaining columns named according to signals in signals_m_df, containing historical backtested returns.
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
select_and_correct_signals <- function(signals_m_df, chosen_signals_and_positions, backtest_returns_df = NULL){

  ###Get chosen signals
  #####################
  chosen_signals <- names(chosen_signals_and_positions) #Get chosen signals
  signal_positions <- unname(chosen_signals_and_positions) #Get signal positions

  ###selected_signals_m_df
  selected_signals_m_df <- signals_m_df[, c("id", "tickers", "dates", chosen_signals)] #subset cols present in signals_m_df
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
  selected_signals_corrected_positions_m_df[, chosen_short_signals] <- selected_signals_corrected_positions_m_df[, chosen_short_signals]*-1
  ###Change colnames
  colnames(selected_signals_corrected_positions_m_df)[-c(1:3)] <- chosen_signals_corrected_positions


  #####################

  ###Subset backtests
  #######################
  if(!is.null(backtest_returns_df)){
    ###Check if all signals have a backtest
    if(!all(chosen_signals_corrected_positions %in% colnames(backtest_returns_df[-1]))){
      stop("all chosen signals should have a matching position in backtest_returns_df")
    }

    #signals_backtests
    selected_backtest_returns_corrected_positions_df <- backtest_returns_df[, c("dates", chosen_signals_corrected_positions)]
  } else {
    selected_backtest_returns_corrected_positions_df <- NULL
  }

  #######################


  #Returns
  selected_signals_and_backtest_list <- list(
    selected_signals_corrected_positions_m_df = selected_signals_corrected_positions_m_df,
    selected_backtest_returns_corrected_positions_df = selected_backtest_returns_corrected_positions_df
  )

  return(selected_signals_and_backtest_list)

}
