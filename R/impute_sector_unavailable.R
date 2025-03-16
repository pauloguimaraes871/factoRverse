#' Impute Missing Values for Entirely Unavailable Features by Sector
#'
#' This function imputes missing values in a `meta_dataframe` for features that are completely missing
#' within specified sectors. If a feature has only NAs for all rows in the given sector, it will be
#' imputed using the specified method (mean, median, or mode) based on data from other sectors.
#'
#' @param meta_dataframe A `meta_dataframe` object containing the target data.
#' @param sector_column A `character` specifying the column in `meta_dataframe` that contains sector classifications.
#' @param sectors_to_adjust A `character vector` specifying which sectors should be imputed.
#' @param features_to_preserve A `character vector` specifying which features should not be imputed.
#' @param method A `character` specifying the imputation method. Options:
#'   - "mean": Global mean
#'   - "median": Global median
#'   - "mode": Most frequent value
#'
#' @return A modified `meta_dataframe` with imputed values and an informative summary of imputation.
#'
#' @export
setGeneric("impute_sector_unavailable", function(meta_dataframe, sector_column, sectors_to_adjust, features_to_preserve, method = "mean", ...) {
  standardGeneric("impute_sector_unavailable")
})

setMethod("impute_sector_unavailable",
          signature(meta_dataframe = "meta_dataframe", sector_column = "character", sectors_to_adjust = "character", features_to_preserve = "character", method = "character"),
          function(meta_dataframe, sector_column, sectors_to_adjust, features_to_preserve, method = "mean", ...) {

            # Extract data
            pre_silver_features_m_df <- meta_dataframe@data

            # Initial checks
            if (!(sector_column %in% colnames(pre_silver_features_m_df))) {
              stop("The specified sector column does not exist in the meta_dataframe.")
            }
            if (any(!sectors_to_adjust %in% pre_silver_features_m_df[[sector_column]])) {
              stop("Some specified sectors do not exist in the dataset.")
            }
            if (any(!features_to_preserve %in% colnames(pre_silver_features_m_df))) {
              stop("Some specified features to preserve do not exist in the dataset.")
            }
            if (!method %in% c("mean", "median", "mode")) {
              stop("Invalid imputation method specified.")
            }

            # Define imputation functions
            impute_func <- function(column, method) {
              if (method == "mean") return(mean(column, na.rm = TRUE))
              if (method == "median") return(median(column, na.rm = TRUE))
              if (method == "mode") return(as.numeric(names(sort(table(column), decreasing = TRUE)[1])))
              return(NA)
            }

            # Track imputation summary
            imputation_summary <- data.frame(
              Feature = character(),
              Imputed_Value = numeric(),
              Data_Used = integer(),
              stringsAsFactors = FALSE
            )

            # Apply imputation only for features entirely missing in a given sector
            for (feature in setdiff(colnames(pre_silver_features_m_df), c(features_to_preserve, sector_column))) {
              for (sector in sectors_to_adjust) {
                sector_rows <- pre_silver_features_m_df[[sector_column]] == sector
                if (all(is.na(pre_silver_features_m_df[sector_rows, feature]))) {
                  non_sector_rows <- !sector_rows
                  data_used <- sum(!is.na(pre_silver_features_m_df[non_sector_rows, feature]))
                  imputed_value <- impute_func(pre_silver_features_m_df[non_sector_rows, feature], method)
                  pre_silver_features_m_df[sector_rows, feature] <- imputed_value

                  # Store summary info
                  imputation_summary <- rbind(imputation_summary, data.frame(
                    Feature = feature,
                    Imputed_Value = imputed_value,
                    Data_Used = data_used
                  ))
                }
              }
            }

            # Print imputation summary
            print(imputation_summary)

            # Workflow
            new_workflow <- list(
              list(
                timestamp = Sys.time(),
                sector_column = sector_column,
                sectors_to_adjust = sectors_to_adjust,
                features_to_preserve = features_to_preserve,
                method = method,
                call = match.call()
              )
            )

            # Recreate meta_dataframe
            pre_silver_features_m_df <- create_meta_dataframe(pre_silver_features_m_df,
                                                              meta_dataframe_name = meta_dataframe@meta_dataframe_name,
                                                              workflow = c(meta_dataframe@workflow, new_workflow),
                                                              type = "generic")

            names(pre_silver_features_m_df@workflow)[length(pre_silver_features_m_df@workflow)] <-
              paste0("impute_sector_unavailable_", Sys.Date())

            return(pre_silver_features_m_df)
          })
