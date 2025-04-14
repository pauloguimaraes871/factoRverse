#' @title Compute Score Based on Conditions
#'
#' @description
#' Computes a custom score by evaluating multiple user-defined conditions on columns
#' of a `meta_dataframe`. Each condition corresponds to a variable and is applied
#' row-wise. For each row, the number of satisfied conditions is counted.
#'
#' If the number of non-NA inputs for that row is below `min_non_na`, the score is set to `NA`.
#'
#' @param features_m_df A `meta_dataframe` object.
#' @param conditions A named list of functions. Each name must correspond to a column
#'   in the data, and each function should return a logical vector indicating whether
#'   the condition is met.
#' @param feature_name A `character` string specifying the name of the new score column.
#' @param min_non_na A numeric value indicating the minimum number of non-NA values required
#'   to compute the score for a given row. Defaults to `0`.
#'
#' @return A `meta_dataframe` object with an additional column (named `feature_name`)
#'   containing the number of conditions met for each row (or `NA` if insufficient data).
#'
#' @details
#' This function is useful for constructing composite scores based on multiple thresholds
#' or logical criteria. Each condition is applied to its corresponding column using `purrr::map2()`.
#' The total score per row reflects how many of the defined conditions are satisfied.
#'
#' @export
setGeneric("compute_score", function(features_m_df, conditions, feature_name, ...) {
  standardGeneric("compute_score")
})

#' @rdname compute_score
#' @export
setMethod("compute_score",
          signature(features_m_df = "meta_dataframe", conditions = "list", feature_name = "character"),
          function(features_m_df, conditions, feature_name = "score", min_non_na = 0) {

            #Extract data
            ###############
            meta_dataframe_workflow <- features_m_df@workflow
            meta_dataframe_name <- features_m_df@meta_dataframe_name
            pre_silver_features_m_df <- features_m_df@data

            ###############

            #Initial checks
            ###############
              ##Check if conditions is a named list of functions
              if (!is.list(conditions) || is.null(names(conditions))) {
                stop("Conditions must be a named list.")
              }
              if (!all(sapply(conditions, is.function))) {
                stop("Each condition in the list must be a function.")
              }
              #Check if all names are present in features_m_df
              missing_vars <- setdiff(names(conditions), names(pre_silver_features_m_df))
              if (length(missing_vars) > 0) {
                stop("The following condition names do not exist in the data: ", paste(missing_vars, collapse = ", "))
              }

            ###############

            #Apply conditions
            ###############
              ##Apply conditions row-wise using purrr
              pre_silver_features_m_df <- pre_silver_features_m_df %>%
                dplyr::mutate(
                  !!feature_name := purrr::pmap_int(.[names(conditions)], function(...) { #Applies function to all subset rows (df[names(conditions)])
                                                      ##For each row, get the values of the columns
                                                      values <- list(...)

                                                      ##Count non-NA values
                                                      non_na_count <- sum(!is.na(values))

                                                      ##If the number of non-NA values is less than min_non_na, return NA
                                                      if (non_na_count < min_non_na) {
                                                        return(NA_integer_)
                                                      }

                                                      ##Otherwise, compute the sum of conditions met
                                                      sum(purrr::map2_lgl(values, conditions, #Maps values to respective conditions
                                                                          function(x, f) f(x)), na.rm = TRUE)
                                                    }
                  )
                )

            ###############

            #Finalize and return
            ###############

              ##Update workflow
              new_workflow <- list(
                list(
                  timestamp = Sys.time(),
                  conditions = conditions,
                  feature_name = feature_name,
                  min_non_na = min_non_na,
                  call = match.call()
                )
              )

              ##Recreate meta_dataframe
              pre_silver_features_m_df <- create_meta_dataframe(
                pre_silver_features_m_df,
                meta_dataframe_name = meta_dataframe_name,
                workflow = c(meta_dataframe_workflow, new_workflow),
                type = "generic"
              )

              ##Rename workflow entry
              names(pre_silver_features_m_df@workflow)[length(pre_silver_features_m_df@workflow)] <- paste0("compute_score_", feature_name)

              return(pre_silver_features_m_df)
          })
