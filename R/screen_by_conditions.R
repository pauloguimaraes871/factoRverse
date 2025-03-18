#' Generic function for screening a meta_dataframe based on conditions
#'
#' This is a generic function for screening `meta_dataframe` objects based on conditions.
#'
#' @param meta_dataframe A `meta_dataframe` object.
#' @param ... A list of conditions to filter the data. Those will be passed to `dplyr::filter()`.
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
