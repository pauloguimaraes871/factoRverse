#' Screen (filter) a meta_dataframe by row-wise conditions
#'
#' Filters the rows of a `meta_dataframe` using one or more logical conditions, which are passed straight to
#' `dplyr::filter()`. Object metadata is preserved and a screening step is appended to the `workflow` log.
#' Errors if the conditions filter out every row.
#'
#' @param meta_dataframe A `meta_dataframe` object.
#' @param ... One or more logical conditions passed to `dplyr::filter()` (e.g. `value > 15`, `tickers != "C"`).
#'
#' @return A new `meta_dataframe` containing only the rows that satisfy the conditions.
#'
#' @examples
#' \dontrun{
#' # Keep liquid, non-financial names
#' screen_by_conditions(features_m_df, mean_volfin_3m > 1e6, sector != "Financials")
#' }
#'
#' @export
setGeneric("screen_by_conditions", function(meta_dataframe, ...) {
  standardGeneric("screen_by_conditions")
})


#' Method for screening a meta_dataframe based on conditions
#'
#' @inheritParams screen_by_conditions
#' @return A new `meta_dataframe` object with screened data.
#' @export
setMethod("screen_by_conditions", "meta_dataframe", function(meta_dataframe, ...) {

  ##Apply filtering using dplyr
  pre_silver_features_m_df <- dplyr::filter(meta_dataframe@data, ...)

    ###Check if it is empty
    if(nrow(pre_silver_features_m_df) == 0){
      stop("All stocks were filtered out. Please check expression.")
    }

  #Return the preprocessed meta_dataframe
  #################
  ##Finalize with the workflow
  new_workflow <- list(
    list(current_date = meta_dataframe@current_date,  # Current date
         timestamp = Sys.time(), # Timestamp
         condition = match.call(expand.dots = FALSE)$...,
         screening_call = match.call()
    )
  )

  ##Recreate
  pre_silver_features_m_df <- create_meta_dataframe(pre_silver_features_m_df,
                                                    meta_dataframe_name = meta_dataframe@meta_dataframe_name,
                                                    workflow = c(meta_dataframe@workflow, new_workflow)
  )


  ##Rename
  names(pre_silver_features_m_df@workflow)[length(pre_silver_features_m_df@workflow)] <-
    paste0("screen_generic", "_", pre_silver_features_m_df@current_date)

  return(pre_silver_features_m_df)


})
