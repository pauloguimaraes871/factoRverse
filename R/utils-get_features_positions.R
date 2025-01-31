#' Retrieve Processed Feature Positions
#'
#' Gathers chosen signals and positions from a set of base backtest objects, verifies
#' consistency, and filters features according to \code{features_passthrough}.
#' Positions labeled \code{"force"} are mapped to \code{"long"} in the final output.
#'
#' @param base_sb_backtest_results_list A list of base backtest result objects, each of which
#'   may contain chosen signals and positions. Passed to the internal function
#'   \code{\link{get_and_check_chosen_signals_and_positions}} to retrieve a reference
#'   set of signals/positions.
#' @param features_passthrough A character vector dictating which features are retained
#'   in the final output (subsetting the reference set). If \code{"all"}, all features
#'   remain; if \code{"none"}, returns \code{"none"}; otherwise, must be a subset of
#'   features found in the reference set.
#' @param features_m_df A meta-data object (e.g., a \code{meta_dataframe}) containing
#'   at least three key columns (often \code{date}, \code{symbol}, \code{target}) plus
#'   feature columns. Used primarily for consistency checks and reconstructing any missing
#'   signals/positions in \code{base_sb_backtest_results_list}.
#'
#' @details
#' \enumerate{
#'   \item **Reference Signals**: The function first calls
#'         \code{\link{get_and_check_chosen_signals_and_positions}} to ensure all base backtest
#'         elements share identical named positions. Missing elements are reconstructed as
#'         all-"long".
#'   \item **Feature Filtering**: Depending on \code{features_passthrough},
#'         the reference set is either left intact (\code{"all"}), discarded (\code{"none"}),
#'         or subset to the specified feature names.
#'   \item **Force Handling**: Any positions labeled \code{"force"} are treated as
#'         \code{"long"} in the final output.
#' }
#'
#' @return A named character vector of positions (e.g., \code{"long"} or \code{"short"}),
#'   indexed by feature name. If \code{features_passthrough = "none"}, returns the string
#'   \code{"none"} directly.
#'
#' @examples
#' \dontrun{
#'   # Example backtest results list
#'   base_list <- list(...)
#'
#'   # meta_dataframe with columns date, symbol, target, and feature columns
#'   features_mdf <- create_meta_dataframe(...)
#'
#'   # Retain all features
#'   get_features_positions(
#'     base_sb_backtest_results_list = base_list,
#'     features_passthrough = "all",
#'     features_m_df = features_mdf
#'   )
#'
#'   # Retain only a subset of features
#'   get_features_positions(
#'     base_sb_backtest_results_list = base_list,
#'     features_passthrough = c("Feature1", "Feature2"),
#'     features_m_df = features_mdf
#'   )
#' }
#'
get_features_positions <- function(base_sb_backtest_results_list, features_passthrough, features_m_df) {

  #Get reference chosen_signals_and_positions
  ############################
  chosen_signals_and_positions <- get_and_check_chosen_signals_and_positions(
    base_sb_backtest_results_list = base_sb_backtest_results_list,
    base_sb_backtest_configs_list = NULL,
    features_passthrough = features_passthrough,
    features_m_df = features_m_df@data
  )

  ############################

  ##Get positions
  ############################
  ###Get positions according to features_passthrough
  if (length(features_passthrough) == 1 && features_passthrough == "all") {
    features_passthrough_and_positions <- chosen_signals_and_positions
  } else if (length(features_passthrough) == 1 && features_passthrough == "none") {
    features_passthrough_and_positions <- "none"
  } else {
    features_passthrough_and_positions <- chosen_signals_and_positions[features_passthrough]
  }

  ##treat "force" as "long"
  features_passthrough_and_positions[features_passthrough_and_positions == "force"] <- "long"


  return(features_passthrough_and_positions)
  ############################

}


#' Get chosen_signals_and_positions from base_sb_backtest_results_list or base_sb_backtest_configs_list and check for conformity
get_and_check_chosen_signals_and_positions <- function(base_sb_backtest_results_list = NULL, base_sb_backtest_configs_list = NULL, features_passthrough, features_m_df){

  ####Get raw chosen_signals_and_positions_list depending on whether ss_backtest_results or ss_backtest_configs are supplied
  #############################################
  if (!is.null(base_sb_backtest_results_list)){
    ####Base SB Backtest Results
    chosen_signals_and_positions_list <-
      lapply(base_sb_backtest_results_list, function(x){ #List is already available
          current_chosen_signals_and_positions_vec <- x@sb_backtest_workflow$chosen_signals_and_positions
          return(current_chosen_signals_and_positions_vec)
      })
  } else {
    ####Base SB Backtest Configs
    chosen_signals_and_positions_list <-
      lapply(base_sb_backtest_configs_list, function(x){
        #For SS Backtest Configs
        if (!is.null(x@ss_backtest_config)){
          current_chosen_signals_and_positions_vec <-x@ss_backtest_config@chosen_signals_and_positions
          names(current_chosen_signals_and_positions_vec) <- names(x@ss_backtest_config@chosen_signals_and_positions)
          return(current_chosen_signals_and_positions_vec)
        }
        #For SS Backtest Results
        if (!is.null(x@ss_backtest_results)){
          current_chosen_signals_and_positions_vec <- x@ss_backtest_results@ss_backtest_workflow$chosen_signals_and_positions
          names(current_chosen_signals_and_positions_vec) <- names(x@ss_backtest_results@ss_backtest_workflow$chosen_signals_and_positions)
          return(current_chosen_signals_and_positions_vec)
        }
        #For NULLs
        if (is.null(x@ss_backtest_config) && is.null(x@ss_backtest_results)){
          #If both SS Config and Results are missing, one can just use sb-level chosen_signals
          current_chosen_signals_and_positions <- x@chosen_signals_and_positions
          names(current_chosen_signals_and_positions) <- names(x@chosen_signals_and_positions)
          return(current_chosen_signals_and_positions)
        }
      })
  }
  #############################################

  #Checks
  #############################################

  ####Verify that objects are the same
  chosen_signals_and_positions_reference <- chosen_signals_and_positions_list[[1]]
  if (length(chosen_signals_and_positions_list) > 1) {
    for (i in seq_along(chosen_signals_and_positions_list)) { #For each object
      current_vec <- chosen_signals_and_positions_list[[i]]
      if (!identical(current_vec, chosen_signals_and_positions_reference)) { #Compare with first one as reference
        #If they are not identifical, warn
        stop("chosen_signals_and_positions of base objects differ at element index: ", i, ".")
      }
    }
  }

  ####Check if features_passthrough is contained
   ####If chosen_signals_and_positions is a consolidated 'all', what can happen if ss or sb haven't be run yet (during check_meta_inputs)
   ####reconstruct it base on features_m_df
  if (length(chosen_signals_and_positions_reference) == 1 && chosen_signals_and_positions_reference == "all") {
    chosen_signals_and_positions_reference <- rep("long", ncol(features_m_df[,-c(1:3)])) #Exclude date, ticker, and id
    names(chosen_signals_and_positions_reference) <- colnames(features_m_df)[-c(1:3)]
  }

  if (!(length(features_passthrough) == 1 && features_passthrough %in% c("all", "none"))) {
    if (!all(features_passthrough %in% names(chosen_signals_and_positions_reference))) {
      stop("features_passthrough should be contained in chosen_signals_and_positions of base objects")
    }
  }
  #############################################

  return(invisible(chosen_signals_and_positions_reference))

}
