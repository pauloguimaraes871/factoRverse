#' Apply tickers_catalog transformations to raw_features_m_df
#'
#' This function takes a `raw_features_m_df` object and a `apply_tickers_catalog` object,
#' applies transformations by mapping tickers in the `tickers` column of `raw_features_m_df`
#' to their corresponding `perm_id` from `apply_tickers_catalog`, and removes rows where the date
#' is outside the valid trading range.
#'
#' @param raw_features_m_df A `raw_features_m_df` object.
#' @param tickers_catalog A `tickers_catalog` object.
#' @param remove_untraded A logical value. If TRUE, rows containing stocks classified as "untraded" in `apply_tickers_catalog` will also be eliminated.
#'
#' @return A modified `raw_features_m_df` object with tickers mapped to `perm_id` and invalid rows removed.
#'
#' @export
setGeneric("read_tickers_catalog", function(raw_features_m_df, tickers_catalog, remove_untraded = FALSE) {
  standardGeneric("read_tickers_catalog")
})

setMethod("read_tickers_catalog",
          signature(raw_features_m_df = "raw_features_m_df", tickers_catalog = "tickers_catalog"),
          function(raw_features_m_df, tickers_catalog, remove_untraded = FALSE) {


            # Check that meta_dataframe names match
            if (raw_features_m_df@meta_dataframe_name != tickers_catalog@meta_dataframe_name) {
              stop("The meta_dataframe_name of raw_features_m_df does not match the one in tickers_catalog")
            }
            # Check that versions match
            if (raw_features_m_df@current_date != tickers_catalog@current_date) {
              stop("The current_date of raw_features_m_df does not match the one in tickers_catalog")
            }
            #Check if raw_features_m_df contains tickers not present in tickers_catalog
            if (any(!raw_features_m_df@data$tickers %in% tickers_catalog@catalog$tickers)) {
              stop("Some tickers in raw_features_m_df are not present in tickers_catalog")
            }


            # Extract relevant data
            meta_dataframe_name <- raw_features_m_df@meta_dataframe_name
            raw_features_m_df <- raw_features_m_df@data
            catalog <- tickers_catalog@catalog
            n_days_tolerance <- tickers_catalog@n_days_tolerance

            # Merge to replace tickers with perm_id
            raw_features_m_df <- raw_features_m_df %>%
              dplyr::left_join(catalog %>% dplyr::select(tickers, perm_id, date_first_quote, date_last_quote, untraded), by = "tickers")

            # Remove rows where date is outside the trading range, but keep untraded stocks (both dates NA)
            raw_features_m_df <- raw_features_m_df %>%
              dplyr::filter(untraded | #Keep untraded stocks at this point
                           ((!is.na(date_first_quote) & dates >= date_first_quote) & #Remove rows for which date happens before a stock being listed
                            (!is.na(date_last_quote) & dates <= date_last_quote + lubridate::days(n_days_tolerance)))) #Remove rows for which date happens

            # Optionally remove rows with untraded stocks
            if (remove_untraded) {
              raw_features_m_df <- raw_features_m_df %>% dplyr::filter(!untraded)
            }

            # Replace tickers column with perm_id and remove unnecessary columns
            raw_features_m_df <- raw_features_m_df %>%
              dplyr::select(-tickers, -date_first_quote, -date_last_quote, -untraded) %>%
              dplyr::mutate(perm_id = unname(perm_id)) %>% #Remove name attribute from perm_id
              dplyr::rename(tickers = perm_id) %>% #Rename perm_id to tickers
              dplyr::relocate("tickers", .before = "dates") %>% #Move tickers column to the left
              dplyr::mutate(id = paste0(tickers, "-", dates), .before = "tickers") %>% #Rereate id column
              dplyr::arrange(id) #Sort by id

            # Update meta_dataframe object
            pre_silver_features_m_df <- create_meta_dataframe(raw_features_m_df, meta_dataframe_name = meta_dataframe_name,
                                                              workflow = list(apply_tickers_catalog =
                                                                                c(n_days_tolerance = tickers_catalog@n_days_tolerance, #Inform n_days_tolerance used
                                                                                  remove_untraded = remove_untraded, #Inform if remove_untraded
                                                                                  current_date = raw_features_m_df$dates[which.max(raw_features_m_df$dates)], #Current date
                                                                                  timestamp = Sys.time())
                                                                              ),
                                                              type = "generic")


            return(pre_silver_features_m_df)
          })
