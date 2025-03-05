#' Apply tickers_catalog transformations to raw_features_m_df
#'
#' This function takes a `raw_features_m_df` object and a `tickers_catalog` object,
#' applies transformations by mapping tickers in the `tickers` column of `raw_features_m_df`
#' to their corresponding `perm_id` from `tickers_catalog`, and removes rows where the date
#' is outside the valid trading range or the stock is classified as "untraded".
#'
#' @param raw_features_m_df A `raw_features_m_df` object.
#' @param tickers_catalog A `tickers_catalog` object.
#' @param verbose A logical value indicating whether to print a summary of the removed rows.
#'
#' @return A modified `raw_features_m_df` object with tickers mapped to `perm_id` and invalid rows removed.
#'
#' @export
setGeneric("read_tickers_catalog", function(raw_features_m_df, tickers_catalog, ...) {
  standardGeneric("read_tickers_catalog")
})

setMethod("read_tickers_catalog",
          signature(raw_features_m_df = "raw_features_m_df", tickers_catalog = "tickers_catalog"),
          function(raw_features_m_df, tickers_catalog, verbose = TRUE) {

            #Initial checks
            #################
              ##Check that meta_dataframe names match
              if (!stringr::str_detect(raw_features_m_df@meta_dataframe_name, tickers_catalog@meta_dataframe_name)) {
                stop("The meta_dataframe_name of raw_features_m_df does not match the one in tickers_catalog")
              }
              ##Check that versions match
              if (raw_features_m_df@current_date != tickers_catalog@current_date) {
                stop("The current_date of raw_features_m_df does not match the one in tickers_catalog")
              }
              ##Check if raw_features_m_df contains tickers not present in tickers_catalog
              if (any(!raw_features_m_df@data$tickers %in% tickers_catalog@catalog$tickers)) {
                stop("Some tickers in raw_features_m_df are not present in tickers_catalog")
              }
              ##Check that no 'old' tickers are present
              if (nrow(raw_features_m_df@data %>% dplyr::filter(tickers %in% tickers_catalog@old)) > 0) {
                stop("raw_features_m_df should not have 'old' tickers.")
              }

            #################

            # Extract and adjust relevant data
            #################
              ##meta_dataframe
                ###Extraction
                meta_dataframe_name <- raw_features_m_df@meta_dataframe_name
                meta_dataframe_workflow <- raw_features_m_df@workflow
                current_date <- raw_features_m_df@current_date
                raw_features_m_df <- raw_features_m_df@data

                ###Create a flag indicating if row has any NA
                features_cols <- setdiff(names(raw_features_m_df), c("id", "tickers", "dates"))
                raw_features_m_df$has_not_na <- apply(raw_features_m_df[, features_cols, drop = FALSE], 1, function(x) any(!is.na(x)))


              ##catalog
              catalog <- tickers_catalog@catalog
              n_days_tolerance <- tickers_catalog@n_days_tolerance
            #################

            #Remove untraded and delisted (at time) and change perm_id
            #################
              ##Join raw_features_m_df with catalog
              raw_features_m_df <- raw_features_m_df %>%
                dplyr::left_join(catalog %>% dplyr::select(tickers, perm_id, tickers_first_quote, tickers_last_quote, untraded), by = "tickers")

              ##Remove rows where date is untraded or outside the trading range
                ###Count untraded
                removed_untraded_summary <- raw_features_m_df %>%
                  dplyr::filter(untraded) %>%
                  dplyr::group_by(tickers, has_not_na) %>%
                  dplyr::summarize(count = dplyr::n(), .groups = "drop") %>% #Count untraded stocks
                  tidyr::complete(tickers, has_not_na = c(TRUE, FALSE), fill = list(count = 0)) %>% #Complete with 0s to avoid breakdowns
                  tidyr::pivot_wider(names_from = has_not_na, values_from = count, names_prefix = "untrd_not_only_NA_")

                ###Remove untraded
                raw_features_m_df <- raw_features_m_df %>%
                  dplyr::filter(!untraded) #Remove untraded stocks

                ###Count outside trading range
                outside_range_summary <- raw_features_m_df %>%
                  dplyr::filter(dates < tickers_first_quote |
                                dates > tickers_last_quote + lubridate::days(n_days_tolerance)) %>%
                  dplyr::group_by(tickers, has_not_na) %>%
                  dplyr::summarize(count = dplyr::n(), .groups = "drop") %>% #Count rows outside trading range
                  tidyr::complete(tickers, has_not_na = c(TRUE, FALSE), fill = list(count = 0)) %>%
                  tidyr::pivot_wider(names_from = has_not_na, values_from = count, names_prefix = "out_trd_rg_not_only_NA_")

                ###Remove outside trading range
                raw_features_m_df <- raw_features_m_df %>%
                  dplyr::filter(dates >= tickers_first_quote & #Remove rows for which date happens before a stock being listed
                                dates <= tickers_last_quote + lubridate::days(n_days_tolerance))#Remove rows for which date happens after a stock being delisted

              ##Replace tickers column with perm_id and remove unnecessary columns
              raw_features_m_df <- raw_features_m_df %>%
                dplyr::select(-tickers, -tickers_first_quote, -tickers_last_quote, -untraded, -has_not_na) %>%
                dplyr::mutate(perm_id = unname(perm_id)) %>% #Remove name attribute from perm_id
                dplyr::rename(tickers = perm_id) %>% #Rename perm_id to tickers
                dplyr::relocate("tickers", .before = "dates") %>% #Move tickers column to the left
                dplyr::mutate(id = paste0(tickers, "-", dates), .before = "tickers") %>% #Rereate id column
                dplyr::arrange(id) #Sort by id
            #################

            #Combine summaries
              ##Consider 4 cases
                ###If there are no untraded or delisted
                if (nrow(removed_untraded_summary) == 0 && nrow(outside_range_summary) == 0) {
                  row_removal_summary <- data.frame(
                    tickers = character(0),
                    untrd_not_only_NA_TRUE = integer(0),
                    untrd_not_only_NA_FALSE = integer(0),
                    out_trd_rg_not_only_NA_TRUE = integer(0),
                    out_trd_rg_not_only_NA_FALSE = integer(0),
                    stringsAsFactors = FALSE
                  )
                }
                ###If there are no untraded
                if (nrow(removed_untraded_summary) == 0 && nrow(outside_range_summary) > 0) {
                  row_removal_summary <- outside_range_summary %>%
                    dplyr::mutate(
                      untrd_not_only_NA_TRUE = 0,
                      untrd_not_only_NA_FALSE = 0
                    ) %>%
                    dplyr::arrange(tickers) %>%
                    as.data.frame()
                }
                ###If there are no delisted
                if (nrow(removed_untraded_summary) > 0 && nrow(outside_range_summary) == 0) {
                  row_removal_summary <- removed_untraded_summary %>%
                    dplyr::mutate(
                      out_trd_rg_not_only_NA_TRUE = 0,
                      out_trd_rg_not_only_NA_FALSE = 0
                    ) %>%
                    dplyr::arrange(tickers) %>%
                    as.data.frame()
                }
                ###If there are both untraded and delisted
                if (nrow(removed_untraded_summary) > 0 && nrow(outside_range_summary) > 0) {
                  row_removal_summary <- dplyr::full_join(removed_untraded_summary, outside_range_summary, by = "tickers") %>%
                    dplyr::mutate(
                      untrd_not_only_NA_TRUE = ifelse(is.na(untrd_not_only_NA_TRUE), 0, untrd_not_only_NA_TRUE),
                      untrd_not_only_NA_FALSE = ifelse(is.na(untrd_not_only_NA_FALSE), 0, untrd_not_only_NA_FALSE),
                      out_trd_rg_not_only_NA_TRUE = ifelse(is.na(out_trd_rg_not_only_NA_TRUE), 0, out_trd_rg_not_only_NA_TRUE),
                      out_trd_rg_not_only_NA_FALSE = ifelse(is.na(out_trd_rg_not_only_NA_FALSE), 0, out_trd_rg_not_only_NA_FALSE)
                    ) %>%
                    dplyr::arrange(tickers) %>%
                    as.data.frame()
                }

              ##Rearrange columns
              row_removal_summary <- row_removal_summary %>%
                dplyr::select(tickers, untrd_not_only_NA_TRUE, untrd_not_only_NA_FALSE, out_trd_rg_not_only_NA_TRUE, out_trd_rg_not_only_NA_FALSE)

              ##Change names for better interpretability
              names(row_removal_summary) <- c("tickers", "untrd_not_only_NA", "untrd_only_NA", "out_trd_rg_not_only_NA", "out_trd_rg_only_NA")

              ##Inform the user by printing the summary
              if (verbose) {
                message("Summary of rows removed by ticker:")
                print(row_removal_summary)
              }

            #Update meta_dataframe object
            #################
            new_workflow <-
                list(
                  list(n_days_tolerance = n_days_tolerance, #Inform n_days_tolerance used
                       current_date = current_date, #Current date
                       timestamp = Sys.time(),
                       row_removal_summary = row_removal_summary) #Summary of rows removed
                )

            pre_silver_features_m_df <- create_meta_dataframe(raw_features_m_df, meta_dataframe_name = meta_dataframe_name,
                                                              workflow = c(meta_dataframe_workflow, new_workflow),
                                                              type = "generic")

            names(pre_silver_features_m_df@workflow)[length(pre_silver_features_m_df@workflow)] <- paste0("read_tickers_catalog_", current_date)


            return(pre_silver_features_m_df)
          })
