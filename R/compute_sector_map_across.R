#' Compute Sector-Based Mapped Values Across a Meta XTS
#'
#' This function assigns values from a `meta_xts` object to a new column in a `meta_dataframe`
#' based on a sector mapping rule. The mapping determines which column from `meta_xts` should be used
#' based on the sector classification in `meta_dataframe`, and an optional transformation is applied.
#'
#' @param meta_dataframe A `meta_dataframe` object containing the target data.
#' @param meta_xts A `meta_xts` object containing the reference values.
#' @param sector_columnumn A `character` specifying the column in `meta_dataframe` that contains sector classifications.
#' @param mapper A named `list` in which names represent sector names and elements are formulas defining how to compute values (e.g., `~-A`, `~A + B`).
#' @param feature_name A `character` specifying the name of the new computed column. Default is `<sector_column>_sector_value`.
#'
#' @return A modified `meta_dataframe` with the new column containing computed values.
#'
#' @details
#' The function:
#' - Maps each row in `meta_dataframe` to the corresponding column in `meta_xts` using `mapper`
#' - Extracts values from `meta_xts` based on `dates`
#' - Applies optional transformations per sector
#'
#' @return A modified `meta_dataframe` with the new column containing computed values.
#'
#' @export
setGeneric("compute_sector_map_across", function(meta_dataframe, meta_xts, sector_column, mapper, ...) {
  standardGeneric("compute_sector_map_across")
})

#' @rdname compute_sector_map_across
#' @export
setMethod("compute_sector_map_across",
          signature(meta_dataframe = "meta_dataframe", meta_xts = "meta_xts", sector_column = "character", mapper = "list"),
          function(meta_dataframe, meta_xts, sector_column, mapper, feature_name = NULL, ...) {

            #Initial prep
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
              ##Ensure sector column exists
              if (!(sector_column %in% colnames(pre_silver_features_m_df))) {
                stop("The specified sector column does not exist in the meta_dataframe.")
              }
              ##Ensure that there are no NAs in sector column
              if (any(is.na(pre_silver_features_m_df[[sector_column]]))) {
                stop("The sector column contains NAs.")
              }
              ##Ensure that are are no NAs in pre_silver_meta_xts
              if (any(is.na(pre_silver_meta_xts))) {
                stop("The meta_xts contains NAs.")
              }
              ##Ensure 'formula' is of class formula
              if (any(!sapply(mapper, function(x) inherits(x, "formula")))) {
                stop("The mapper object must be a list of formulas.")
              }
              ##Ensure all sectors in pre_silver_features_m_df exist in mapper
              unique_sectors <- unique(pre_silver_features_m_df[[sector_column]])
              missing_sectors <- setdiff(unique_sectors, names(mapper))
              if (length(missing_sectors) > 0) {
                stop("The following sectors in meta_dataframe are missing in the mapper: ", paste(missing_sectors, collapse = ", "))
              }
              ##Ensure both objects share same dates
              if (!setequal(unique(pre_silver_features_m_df$dates), as.Date(zoo::index(pre_silver_meta_xts)))){
                stop("Dates in meta_dataframe and meta_xts do not match.")
              }
              ##Check that current_date match
              if (meta_dataframe_current_date != meta_xts_current_date) {
                stop("Current dates do not match between meta_dataframe and meta_xts.")
              }

            ###############

            #Apply FUN
            ###############

              ##Default new column name
              if (is.null(feature_name)) {
                new_col_name <- "sector_mapped"
              } else {
                new_col_name <- feature_name
              }

              ##Compute mapped values
              pre_silver_features_m_df[[new_col_name]] <- purrr::map2_dbl(
                .x = pre_silver_features_m_df[[sector_column]], #Maps x and y to FUN
                .y = pre_silver_features_m_df$dates,
                ~ {
                  sector_name <- .x
                  date <- .y
                  formula_expr <- mapper[[sector_name]]

                  ###Ensure formula exists
                  if (is.null(formula_expr)) {
                    stop(paste0("No formula found in mapper for sector '", sector_name, "'"))
                  }

                  ###Extract values from xts using the matching date and convert to numeric value
                  xts_row <- as.list(pre_silver_meta_xts[as.character(date), , drop = FALSE]) %>%
                    purrr::map(as.numeric)

                  ###Ensure variables exist in xts_row
                  if (!all(all.vars(formula_expr) %in% names(xts_row))) {
                    stop(paste0("One or more variables in formula '", deparse(formula_expr), "' are missing in metrics_xts at date ", date))
                  }

                  ###Extract only the right-hand side of the formula
                  rhs_expr <- rlang::f_rhs(formula_expr)
                  result <- rlang::eval_tidy(rhs_expr, data = xts_row) #Apply the extracted expression using rlang::eval_tidy()

                  return(as.numeric(result))
                }
              )

            ###############
            ##Finalize with the workflow
            new_workflow <- list(
              list(current_date = meta_dataframe_current_date,  # Current date
                   timestamp = Sys.time(),        # Timestamp
                   sector_column = sector_column,
                   feature_name = new_col_name,
                   meta_xts_name = meta_xts_name,
                   mapper = mapper,
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
              paste0("compute_sector_map_across", meta_dataframe_current_date)
            ############

            return(pre_silver_features_m_df)

          })
