#' Compute Compound Annual Growth Rate (CAGR) for a given signal in a meta_dataframe
#'
#' This method computes the CAGR for each observation in a \code{meta_dataframe} object by comparing the value
#' in the specified signal column (final value) with the value for the same ticker at a date \code{period} months before (begin value).
#' If no corresponding previous observation is found, the result is set to \code{NA}.
#'
#' @param features_m_df A \code{meta_dataframe} object.
#' @param period A \code{numeric} value representing the number of months to look back.
#' @param signal A \code{character} specifying the column name on which the CAGR is computed.
#'
#' @return A \code{meta_dataframe} object with an added column named \code{<signal>_cagr} in its \code{data} slot containing the computed CAGR values.
#'
#' @details
#' The CAGR is calculated using the \code{cagr} function. For each row (final value), the method identifies the corresponding row
#' for the same ticker at a date that is exactly \code{period} months earlier (begin value) using \code{lubridate::`%m-%`} and
#' \code{lubridate::months}. If no matching observation is found, the resulting CAGR is \code{NA}. This method assumes that the
#' \code{dates} and \code{tickers} columns are present in the data slot and that the \code{dates} column is of class \code{Date}.
#'
#' @examples
#' \dontrun{
#'   # Suppose meta_df is a meta_dataframe object and "Alpha" is one of the signal columns:
#'   meta_df <- compute_cagr(meta_df, period = 3, signal = "Alpha")
#' }
#'
#' @export
setGeneric("compute_cagr", function(features_m_df, period, signal) standardGeneric("compute_cagr"))

setMethod("compute_cagr",
          signature(features_m_df = "meta_dataframe", period = "numeric", signal = "character"),
          function(features_m_df, period, signal) {

            #Pass features_m_df as pre_silver_features_m_df
            ############
            meta_dataframe_workflow <- features_m_df@workflow
            meta_dataframe_name <- features_m_df@meta_dataframe_name
            current_date <- features_m_df@current_date
            pre_silver_features_m_df <- features_m_df@data

            ############

            #Initial Checks
            ############
            ##Check if the specified signal column exists in the data frame
            if (!signal %in% names(pre_silver_features_m_df)) {
              stop("The signal column does not exist in the data frame.")
            }

            ############

            #Compute CAGR
            ############

            ##Use purrr::map_dbl to compute CAGR for each row
            cagr_values <- purrr::map_dbl(seq_len(nrow(pre_silver_features_m_df)), function(i){

              ###Get current
              current_row <- pre_silver_features_m_df[i,]
              ticker_i <- current_row$tickers
              current_date <- current_row$dates

              ###Compute the target date by subtracting 'period' months
              target_date <- lubridate::add_with_rollback(current_date, -months(period))

              ###Find rows with the same ticker and the computed target date
              matching_index <- which(pre_silver_features_m_df$tickers == ticker_i & pre_silver_features_m_df$dates == target_date)

              ###Apply CAGR
              if (length(matching_index) > 0) {
                begin_value <- pre_silver_features_m_df[matching_index[1], signal]
                final_value <- current_row[[signal]]
                cagr(begin_value, final_value, period)
              } else {
                NA_real_
              }
            })

            ##Add the computed CAGR values to the meta_dataframe
            ###Create col names
            new_col_name <- paste0(signal, "_cagr_", period)

            ###Add
            pre_silver_features_m_df <- pre_silver_features_m_df %>%
              dplyr::mutate(!!rlang::sym(new_col_name) := cagr_values) #Add cagr values to dynamically created column


            ############

            #Finalize with the workflow
            ############
              ##Create workflow
              new_workflow <- list(
                list(current_date = current_date, #Current date
                     timestamp = Sys.time(), #Timestamp
                     signal = signal,
                     period = period
                )
              )

              ##Create
              pre_silver_features_m_df <- create_meta_dataframe(pre_silver_features_m_df, meta_dataframe_name = meta_dataframe_name,
                                                                workflow = c(meta_dataframe_workflow, new_workflow),
                                                                type = "generic")
              ##Rename
              names(pre_silver_features_m_df@workflow)[length(pre_silver_features_m_df@workflow)] <- paste0("compute_cagr_",period, "_", signal, "_", current_date)

            return(pre_silver_features_m_df)


          })
