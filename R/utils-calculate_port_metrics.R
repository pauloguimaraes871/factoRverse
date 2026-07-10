#' Calculate Consolidated Portfolio and Benchmark Metrics
#'
#' This function joins custom stock metrics to portfolio allocation data and computes
#' consolidated portfolio metrics by calculating the weighted sum of each metric column.
#' Optionally, if benchmark weights are provided (i.e. if \code{selected_benchmark} is not \code{NULL}),
#' the function also computes benchmark metrics and merges them with the portfolio metrics.
#'
#' @param port_weights_m_d_ref A data frame containing portfolio weights. Must include an \code{id} column and an
#'   \code{eop_port_weights} column (end-of-period weights). May optionally include a \code{bench_weights} column,
#'   in which case benchmark-weighted metrics (prefixed \code{bench_}) are also computed and merged.
#' @param custom_stock_metrics_m_d_ref A data frame of custom per-stock metrics. Must include an \code{id} column
#'   (used for the join) plus one or more numeric metric columns; \code{tickers}/\code{dates}, if present, are ignored.
#'
#' @return A data frame with consolidated portfolio metrics. If benchmark metrics are calculated,
#'   the data frame will also include columns with a \code{"bench_"} prefix.
#'
#'
calculate_port_metrics <- function(port_weights_m_d_ref, #Base object with weight information
                                   custom_stock_metrics_m_d_ref #Metrics
){

  #Initial prep
  ################
  #Join custom_stock_metrics_m_d_ref to port_allocation_log_m_d_ref
  port_weights_and_metrics_m_d_ref <- port_weights_m_d_ref %>%
    dplyr::left_join(custom_stock_metrics_m_d_ref %>% dplyr::select(-tickers, -dates), #Remove tickers and dates
                     by = "id")  #Join user metrics, allowing NAs

  ################

  #Calculate portfolio and benchmark consolidated metrics
  ################
  ##Define the columns that should be excluded from metric calculations.
  exclude_cols <- c("id", "tickers", "dates", "eop_port_weights", "bench_weights")

  ##Portfolio metrics
  ###Calculate metrics
  port_metrics_d_ref <- port_weights_and_metrics_m_d_ref %>%
    dplyr::summarise(
      dplyr::across(
        -dplyr::any_of(exclude_cols), #Exclude these columns
        ~ sum(eop_port_weights * .x, na.rm = TRUE)  #Calculate weighted sum of each column
      )
    ) %>% as.data.frame()
  ###Reforce names
  colnames(port_metrics_d_ref) <- port_weights_and_metrics_m_d_ref %>% dplyr::select(-dplyr::any_of(exclude_cols)) %>% colnames()

  ##Benchmark metrics
  ###Check for bench_weights presence
  if ("bench_weights" %in% colnames(port_weights_and_metrics_m_d_ref)){

    ###Calculate benchmark metrics
    bench_metrics_d_ref <- port_weights_and_metrics_m_d_ref %>%
      dplyr::summarise(
        dplyr::across(
          -dplyr::all_of(exclude_cols), #Exclude these columns
          ~ sum(bench_weights * .x, na.rm = TRUE)  #Calculate weighted sum of each column
        )
      ) %>% as.data.frame()
    ###Reforce names
    colnames(bench_metrics_d_ref) <- port_weights_and_metrics_m_d_ref %>% dplyr::select(-dplyr::all_of(exclude_cols)) %>% colnames() %>% stringr::str_c("bench_", .)

    ###Merge with port_metrics_d_ref
    port_metrics_d_ref <- merge(port_metrics_d_ref, bench_metrics_d_ref)
  }

  ################

  ##Return
  return(port_metrics_d_ref)

}










