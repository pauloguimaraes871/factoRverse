# compute_sector_wise ----------------------------------------------------
#' Compute Sector-Wise Calculation for a Given Signal in a meta_dataframe
#'
#' This function computes a calculation for each observation in a \code{meta_dataframe} object by applying a
#' predefined function (specified by a character) to all observations within the same sector on the same date.
#'
#' @param features_m_df A \code{meta_dataframe} object.
#' @param sector_column A \code{character} specifying the column name representing sector classification in the dataset.
#' @param signal A \code{character} specifying the column name on which the function is computed.
#' @param FUN A \code{character} specifying the function to apply. Options are "median", "mean", "sd", "signal_to_noise".
#' @param na.rm A \code{logical} indicating whether to remove NA values (default TRUE).
#' @param feature_name A \code{character} specifying the name of the feature to be added to the meta_dataframe. If NULL,
#' the feature name will be set to "<signal>_sector_<FUN>". Default is NULL.
#' @param min_non_na A \code{numeric} value specifying the minimum number of non-NA values required to compute the metric. Default is 0.
#'
#' @return A \code{meta_dataframe} object with an added column named \code{<signal>_sector_<FUN>} in its
#' \code{data} slot containing the computed values.
#'
#' @details
#' For each row (current observation), the function groups observations by \code{sector_column} and \code{dates},
#' then applies the specified function to the \code{signal} column.
#' If no matching observation is found, the resulting value is \code{NA}. The available functions are:
#' \itemize{
#'   \item \strong{median}: \code{stats::median(x, na.rm = na.rm)}
#'   \item \strong{mean}: \code{stats::mean(x, na.rm = na.rm)}
#'   \item \strong{sd}: \code{stats::sd(x, na.rm = na.rm)}
#'   \item \strong{signal_to_noise}: \code{stats::mean(x, na.rm = na.rm) / stats::sd(x, na.rm = na.rm)}
#' }
#'
#' @export
setGeneric("compute_sector_wise", function(features_m_df, sector_column, signal, FUN, na.rm = TRUE, feature_name = NULL, min_non_na = 0) {
  standardGeneric("compute_sector_wise")
})

#' @rdname compute_sector_wise
#' @export
setMethod("compute_sector_wise",
          signature(features_m_df = "meta_dataframe", sector_column = "character", signal = "character", FUN = "character"),
          function(features_m_df, sector_column, signal, FUN, na.rm = TRUE, feature_name = NULL, min_non_na = 0) {

            # Extract Data
            meta_dataframe_workflow <- features_m_df@workflow
            meta_dataframe_name <- features_m_df@meta_dataframe_name
            current_date <- features_m_df@current_date
            pre_silver_features_m_df <- features_m_df@data

            # Initial Checks
            if (!sector_column %in% names(pre_silver_features_m_df)) {
              stop("The sector column does not exist in the data frame.")
            }
            if (any(is.na(pre_silver_features_m_df[[sector_column]]))) {
              stop("The sector column contains NAs.")
            }
            if (!signal %in% names(pre_silver_features_m_df)) {
              stop("The signal column does not exist in the data frame.")
            }
            if (!is.numeric(pre_silver_features_m_df[[signal]])) {
              stop("The signal column must be numeric.")
            }
            if (!is.character(pre_silver_features_m_df[[sector_column]])) {
              stop("The sector_column column must be character.")
            }


            # Compute Sector-Wise Calculation
            sector_values <- pre_silver_features_m_df %>%
              dplyr::group_by(!!rlang::sym(sector_column), dates) %>%
              dplyr::mutate(
                sector_stat = dplyr::if_else(
                  sum(!is.na(!!rlang::sym(signal))) >= min_non_na,
                  switch(FUN,
                         "median" = stats::median(!!rlang::sym(signal), na.rm = na.rm),
                         "mean" = mean(!!rlang::sym(signal), na.rm = na.rm),
                         "sd" = stats::sd(!!rlang::sym(signal), na.rm = na.rm),
                         "signal_to_noise" = signal_to_noise(!!rlang::sym(signal), na.rm = na.rm),
                         stop("Unsupported function type")
                  ),
                  NA_real_
                )
              ) %>%
              dplyr::ungroup()

            # Generate Column Name
            if (is.null(feature_name)) {
              new_col_name <- paste0(signal, "_sector_", FUN)
            } else {
              new_col_name <- feature_name
            }

            # Add Computed Column to Data
            pre_silver_features_m_df <- pre_silver_features_m_df %>%
              dplyr::mutate(!!rlang::sym(new_col_name) := sector_values$sector_stat)

            # Update Workflow
            new_workflow <- list(
              list(current_date = current_date,
                   timestamp = Sys.time(),
                   signal = signal,
                   sector_column = sector_column,
                   feature_name = new_col_name,
                   FUN = FUN,
                   call = match.call(),
                   na.rm = na.rm,
                   min_non_na = min_non_na
              )
            )

            # Recreate meta_dataframe
            pre_silver_features_m_df <- create_meta_dataframe(pre_silver_features_m_df,
                                                              meta_dataframe_name = meta_dataframe_name,
                                                              workflow = c(meta_dataframe_workflow, new_workflow),
                                                              type = "generic")
            names(pre_silver_features_m_df@workflow)[length(pre_silver_features_m_df@workflow)] <-
              paste0("compute_", signal, "_sector_", FUN, "_", current_date)

            return(pre_silver_features_m_df)
          })
