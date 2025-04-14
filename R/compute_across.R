#' @title Compute Across: Apply a Calculation Between meta_dataframe and meta_xts
#'
#' @description
#' Applies a predefined mathematical operation between a signal column in a `meta_dataframe`
#' and a metric column in a `meta_xts`. The operation is performed for each row in the
#' `meta_dataframe` that matches the same date in the `meta_xts`.
#'
#' @param meta_dataframe A `meta_dataframe` object containing financial or factor data.
#' @param meta_xts A `meta_xts` object containing time series data (e.g., market metrics).
#' @param FUN A `character` string indicating the operation to apply. Must be one of:
#'   `"product"`, `"ratio"`, `"subtract"`, `"sum"`, `"just_append"`.
#' @param feature_name Optional `character` string giving the name for the new feature column.
#'   If `NULL`, a default name is constructed.
#' @param signal A `character` string specifying the column name in `meta_dataframe` to be used
#'   in the calculation (required unless `FUN = "just_append"`).
#' @param metric A `character` string specifying the column name in `meta_xts` to be used.
#' @param ... Additional arguments (not used currently).
#'
#' @return A modified `meta_dataframe` object with a new computed column.
#'
#' @details
#' The function checks consistency of column names and dates, performs the operation row-wise
#' for matching dates, and appends the result to the `meta_dataframe`.
#'
#' @export
setGeneric("compute_across",
           function(meta_dataframe, meta_xts, FUN, feature_name = NULL, signal = NULL, metric, ...) {
             standardGeneric("compute_across")
           }
)

# Method for meta_dataframe and meta_xts
setMethod("compute_across",
          signature(meta_dataframe = "meta_dataframe", meta_xts = "meta_xts", FUN = "character"),
          function(meta_dataframe, meta_xts, FUN, feature_name = NULL, signal = NULL, metric, ...) {

            #Extract objs
            ###############
            ##Name
            meta_dataframe_name <- meta_dataframe@meta_dataframe_name
            meta_xts_name <- meta_xts@meta_xts_name

            ##Current date
            meta_dataframe_current_date <- meta_dataframe@current_date
            meta_xts_current_date <- meta_xts@current_date

            ##Workflow
            meta_dataframe_workflow <- meta_dataframe@workflow

            ##data
            pre_silver_features_m_df <- meta_dataframe@data
            pre_silver_meta_xts <- meta_xts@data

            ###############

            #Initial checks
            ###############
            ##Check that current_date match
            if (meta_dataframe_current_date != meta_xts_current_date) {
              stop("Current dates do not match between meta_dataframe and meta_xts.")
            }
            ##Ensure signal exists in pre_silver_features_m_df
            if (!FUN == "just_append"){
              if (!(!is.null(signal) && signal %in% colnames(pre_silver_features_m_df))) {
                stop("The specified signal does not exist in the meta_dataframe.")
              }
            }
            ##Ensure metric exists in meta_xts
            if (!(metric %in% colnames(pre_silver_meta_xts))) {
              stop("The specified metric does not exist in the meta_xts.")
            }
            ##Ensure both objects share same dates
            if (!setequal(unique(pre_silver_features_m_df$dates), as.Date(zoo::index(pre_silver_meta_xts)))){
              stop("Dates in meta_dataframe and meta_xts do not match.")
            }
            ##Ensure FUN is one of the predefined functions
            valid_FUNs <- c("product", "ratio", "subtract", "sum", "just_append")
            if (!(FUN %in% valid_FUNs)) {
              stop("Invalid FUN specified. Must be one of: 'product', 'ratio', 'subtract', 'sum', 'just_append'.")
            }
            ###############

            #Apply FUN
            ###############

            ##Generate feature name if needed
            if (is.null(feature_name)) {
              new_col_name <- if (!FUN == "just_append") paste0(signal, "_across_", metric, "_", FUN) else paste0("append_", metric)
            } else {
              new_col_name <- feature_name
            }

            ##Apply function row-wise
            if (FUN == "just_append"){
              ###Directly append time-correspondent values from pre_silver_meta_xts
              ####Select signal column
              selected_metric_pre_silver_meta_xts <- pre_silver_meta_xts[, metric]
              ####Turn in df
              selected_metric_pre_silver_df <- data.frame(dates = zoo::index(selected_metric_pre_silver_meta_xts)) %>%
                dplyr::mutate(!!new_col_name := as.numeric(selected_metric_pre_silver_meta_xts)) ##Add metric
              ####Append
              pre_silver_features_m_df <- pre_silver_features_m_df %>%
                dplyr::left_join(selected_metric_pre_silver_df, by = "dates")
            } else {
              pre_silver_features_m_df <- pre_silver_features_m_df %>%
                dplyr::mutate(!!new_col_name := switch(
                  FUN,
                  "product" = .data[[signal]] * purrr::map_dbl(.data$dates, ~ as.numeric(pre_silver_meta_xts[as.character(.x), metric])),
                  "ratio" = .data[[signal]] / purrr::map_dbl(.data$dates, ~ as.numeric(pre_silver_meta_xts[as.character(.x), metric])),
                  "subtract" = .data[[signal]] - purrr::map_dbl(.data$dates, ~ as.numeric(pre_silver_meta_xts[as.character(.x), metric])),
                  "sum" = .data[[signal]] + purrr::map_dbl(.data$dates, ~ as.numeric(pre_silver_meta_xts[as.character(.x), metric])),
                  stop("Invalid FUN. Must be one of 'product', 'ratio', 'subtract', 'sum' or 'just_append'.")
                ))
            }

            ############
            ##Finalize with the workflow
            new_workflow <- list(
              list(current_date = meta_dataframe_current_date,  # Current date
                   timestamp = Sys.time(),        # Timestamp
                   signal = signal,
                   metric = metric,
                   feature_name = new_col_name,
                   meta_xts_name = meta_xts_name,
                   FUN = FUN,
                   call = match.call()
              )
            )

            ##Recreate
            pre_silver_features_m_df <- create_meta_dataframe(pre_silver_features_m_df,
                                                              meta_dataframe_name = meta_dataframe_name,
                                                              workflow = c(meta_dataframe_workflow, new_workflow),
                                                              type = "generic")
            ##Rename
            names(pre_silver_features_m_df@workflow)[length(pre_silver_features_m_df@workflow)] <-
              paste0("compute_", signal, "_across_", metric, "_", FUN, "_", meta_dataframe_current_date)
            ############

            return(pre_silver_features_m_df)


          })
