#' Compute Sector-Based Column Transformations
#'
#' This function creates a new column in a `meta_dataframe` based on sector classification
#' and applies transformations specified in a mapping list of formulas.
#'
#' @param meta_dataframe A `meta_dataframe` object containing the target data.
#' @param sector_column A `character` specifying the column in `meta_dataframe` that contains sector classifications.
#' @param mapper A named `list` in which names represent sector names and elements are formulas defining how to compute values (e.g., `~-Alpha`, `~ Beta + Alpha`).
#' @param feature_name A `character` specifying the name of the new computed column.
#'
#' @return A modified `meta_dataframe` with the new column containing computed values.
#'
#' @details
#' The function:
#' - Maps each row in `meta_dataframe` to the corresponding transformation based on `sector_column`
#' - Evaluates the formulas in `mapper` within the context of the `meta_dataframe`
#'
#' @export
setGeneric("compute_sector_map", function(meta_dataframe, sector_column, mapper, ...) {
  standardGeneric("compute_sector_map")
})

setMethod("compute_sector_map",
          signature(meta_dataframe = "meta_dataframe", sector_column = "character", mapper = "list"),
          function(meta_dataframe, sector_column, mapper, feature_name = NULL, ...) {

            #Initial prep
            ###############
            meta_dataframe_name <- meta_dataframe@meta_dataframe_name
            meta_dataframe_current_date <- meta_dataframe@current_date
            meta_dataframe_workflow <- meta_dataframe@workflow
            pre_silver_features_m_df <- meta_dataframe@data

            ###############

            #Initial checks
            ###############
            if (!(sector_column %in% colnames(pre_silver_features_m_df))) {
              stop("The specified sector column does not exist in the meta_dataframe.")
            }
            if (any(is.na(pre_silver_features_m_df[[sector_column]]))) {
              stop("The sector column contains NAs.")
            }
            if (any(!sapply(mapper, function(x) inherits(x, "formula")))) {
              stop("The mapper object must be a list of formulas.")
            }
            unique_sectors <- unique(pre_silver_features_m_df[[sector_column]])
            missing_sectors <- setdiff(unique_sectors, names(mapper))
            if (length(missing_sectors) > 0) {
              stop("The following sectors in meta_dataframe are missing in the mapper: ", paste(missing_sectors, collapse = ", "))
            }

            ###############

            #Compute new column
            ###############
             ##Create feature_name
            if (is.null(feature_name)) {
              new_col_name <- paste0(sector_column, "_mapped")
            } else {
              new_col_name <- feature_name
            }


            pre_silver_features_m_df[[new_col_name]] <- purrr::map2_dbl(
              .x = pre_silver_features_m_df[[sector_column]],
              .y = seq_len(nrow(pre_silver_features_m_df)),
              ~ {
                sector_name <- .x
                row_index <- .y
                formula_expr <- mapper[[sector_name]]

                ##Extract formula right-hand side
                rhs_expr <- rlang::f_rhs(formula_expr)

                ##Evaluate formula within the row's context
                row_data <- as.list(pre_silver_features_m_df[row_index, , drop = FALSE])

                ###Ensure variables exist in row_data
                if (!all(all.vars(formula_expr) %in% names(row_data))) {
                  stop(paste0("One or more variables in formula '", deparse(formula_expr), "' are missing in meta_dataframe at date ", row_data$dates))
                }

                #Eval FUN
                result <- rlang::eval_tidy(rhs_expr, data = row_data)

                return(as.numeric(result))
              }
            )
            ###############

            #Finalize
            ###############
              ##Create workflow entry
              new_workflow <- list(
                list(current_date = meta_dataframe_current_date,
                     timestamp = Sys.time(),
                     sector_column = sector_column,
                     new_col_name = new_col_name,
                     mapper = mapper,
                     call = match.call())
              )

              ##Recreate meta_dataframe
              pre_silver_features_m_df <- create_meta_dataframe(
                pre_silver_features_m_df,
                meta_dataframe_name = meta_dataframe_name,
                workflow = c(meta_dataframe_workflow, new_workflow),
                type = "generic"
              )

              ##Rename workflow entry
              names(pre_silver_features_m_df@workflow)[length(pre_silver_features_m_df@workflow)] <-
                paste0("compute_sector_map", meta_dataframe_current_date)

              return(pre_silver_features_m_df)
          })
