#' Extract Returns from port_backtest_cohort Object
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
extract_returns_m_xts <- function(port_backtest_cohort, signals_m_df, benchmark_returns_m_xts, simplify_name = TRUE, verbose = TRUE){

  #Check if port_backtest_cohort objects match arguments
  #####################
  backtest_workflow_common <- port_backtest_cohort@backtest_workflow_common
  selected_benchmark <- port_backtest_cohort@backtest_workflow_common$selected_benchmark

  ##signals_m_df
  if(backtest_workflow_common$signals_object_name != signals_m_df@meta_dataframe_name){
    stop("signals_m_df object name does not match port_backtest_cohort object name")
  }

  #####################

  #Get backtest_returns_m_xts
  #####################
  backtest_returns_m_xts <- port_backtest_cohort@port_returns_m_xts_list$net_returns_m_xts
  ##Remove selected_bench_return
  if (!is.null(selected_benchmark)){
    backtest_returns_m_xts@data <- backtest_returns_m_xts@data[, -which(colnames(backtest_returns_m_xts@data) == "selected_bench_return")]
  }

  ##Simplify colnames
  #(This step is performed inside this function and not in create_port_backtest_cohort because a typical cohort might contain backtests with same chosen score metric,
  #but a cohort for ss and sb purposes can't)
  if (simplify_name){
    ###Derive simple colnames
    simple_colnames <- sapply(port_backtest_cohort@port_backtest_results_list, function(x){

      port_type <- x@final_stock_port@type # Get portfolio type

      ###For single signal, use config_name
      if (!is.null(x@port_backtest_config@chosen_score_metric_and_position) && port_type == "single_signal"){
        ####Get position ('long' or 'short')
        position <- x@port_backtest_config@chosen_score_metric_and_position %>% unname()
        ####Get score metric
        score_metric <- x@port_backtest_config@chosen_score_metric_and_position %>% names()
        corrected_score_metric <- if(position == "short") paste0("low_", score_metric) else score_metric
        return(corrected_score_metric)
      } else if (!is.null(x@sb_backtest_results) && port_type == "signal_blend"){
      ###For signal blend, use backtest id
        return(x@sb_backtest_results@backtest_identifier)
      } else if (port_type == "custom_weights"){
      ####For custom weights, just use config_name
        return(x@port_backtest_config@config_name)
      } else {
        stop("Can't simplify column names for this port_type")
      }
    })

    ###Check if they are unique
    if (length(unique(simple_colnames)) != length(simple_colnames)){
      stop("Simplified cohort backtest names are not unique")
    }

    if (verbose) {
      message("Simplified cohort backtest names: ", paste(simple_colnames, collapse = ", "))
    }

    ###Change colnames
    colnames(backtest_returns_m_xts@data) <- simple_colnames
  }

  #####################

  #Subset benchmark_returns_m_xts
  if (!is.null(benchmark_returns_m_xts)){
    port_backtest_cohort_dates <- zoo::index(backtest_returns_m_xts@data)
    benchmark_returns_m_xts_dates <- zoo::index(benchmark_returns_m_xts@data)
    benchmark_returns_m_xts@data <- benchmark_returns_m_xts@data[benchmark_returns_m_xts_dates %in% port_backtest_cohort_dates,]
  }

  ##Return
  return(list(backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts))

}

