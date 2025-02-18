#' Extract Backtest Returns from port_backtest_cohort Object
#'
#' This method extracts the `net_returns_m_xts` slot from the `port_backtest_cohort` object,
#' optionally simplifying the column names based on the configuration names of the cohort.
#'
#' @param object An object of class `port_backtest_cohort`.
#' @param signals_m_df An object of class `signals_m_df` containing the signals used in the backtest.
#' @param benchmark_returns_m_xts An object of class `meta_xts` containing the benchmark returns used in the backtest.
#' @param simplify_name Logical. If `TRUE`, the column names will be simplified using the configuration names.
#' @param verbose Logical. If `TRUE`, messages will be printed about the simplification process.
#'
#' @return An `xts` object containing the extracted backtest returns.
#'
#' @examples
#' # Assume `cohort` is an existing port_backtest_cohort object
#' returns_xts <- extract_backtest_returns_m_xts(cohort)
#'
extract_backtest_returns_m_xts <- function(port_backtest_cohort, signals_m_df, benchmark_returns_m_xts, simplify_name = TRUE, verbose = TRUE){

  #Check if port_backtest_cohort objects match arguments
  #####################
  backtest_workflow_common <- port_backtest_cohort@backtest_workflow_common

  ##signals_m_df
  if(backtest_workflow_common$signals_object_name != signals_m_df@meta_dataframe_name){
    stop("signals_m_df object name does not match port_backtest_cohort object name")
  }

  ##benchmark_returns_m_xts
  if (!is.null(benchmark_returns_m_xts)){
    if(backtest_workflow_common$benchmark_returns_object_name != benchmark_returns_m_xts@meta_xts_name){
      stop("benchmark_returns_m_xts object name does not match port_backtest_cohort object name")
    }
  }

  #####################

  #Get backtest_returns_m_xts
  #####################
  backtest_returns_m_xts <- port_backtest_cohort@port_returns_m_xts_list$net_returns_m_xts

  ##Simplify colnames
  if (simplify_name){
    ###Derive simple colnames
    simple_colnames <- sapply(port_backtest_cohort@port_backtest_results_list, function(x){

      ###For single signal, use config_name
      if (!is.null(x@port_backtest_config@chosen_score_metric_and_position)){
        return(x@port_backtest_config@config_name)
      } else {
        ###For signal blend, use backtest id
        return(x@backtest_identifier)
      }

    })

    ###Check if they are unique
    if (length(unique(simple_colnames)) != length(simple_colnames)){
      stop("Simplified cohort backtest names are not unique")
    }

    if (verbose) {
      message("Simplified cohort backtest names: ", simple_colnames)
    }

    ###Change colnames
    colnames(backtest_returns_m_xts@data) <- simple_colnames
  }

  #####################

  ##Return
  return(backtest_returns_m_xts)

}

