#' @title Apply tickers_catalog Transformations
#'
#' @description
#' Applies ticker ID mapping and filtering based on a `tickers_catalog` object. Can handle either
#' a `raw_features_m_df` or a `returns_meta_xts` object. This ensures consistency in ticker
#' identifiers and filters out data outside trading ranges or for untraded tickers.
#'
#' @param data An object of class `raw_features_m_df` or `returns_meta_xts`.
#' @param tickers_catalog A `tickers_catalog` object containing reference data including
#' ticker status, trading range, and permanent IDs.
#' @param verbose A logical indicating whether to print progress messages. Defaults to `TRUE`.
#' @param ... Additional arguments passed to methods. Supports `verbose = TRUE/FALSE`.
#'
#' @details
#' The catalog's stable `perm_id` values replace the (possibly changing) `tickers` labels, so a company keeps a
#' single identity across ticker renames. Each series is then restricted to its valid trading window: observations
#' before `tickers_first_quote` or after `tickers_last_quote` (plus the catalog's `n_days_tolerance`) are set to NA
#' or dropped, and tickers classified as `untraded` are removed. This yields a clean, survivorship-aware panel
#' ready for the silver/gold preprocessing stages.
#'
#' @return An object of the same class as `data`, modified according to the catalog rules.
#'
#' @seealso \code{\link{create_tickers_catalog}}, \code{\link{update_tickers_catalog}}, \code{\link{tickers_catalog-class}}
#'
#' @export
setGeneric("read_tickers_catalog", function(data, tickers_catalog, ...) {
  standardGeneric("read_tickers_catalog")
})

#' @rdname read_tickers_catalog
setMethod("read_tickers_catalog",
          signature(data = "raw_features_m_df", tickers_catalog = "tickers_catalog"),
          function(data, tickers_catalog, verbose = TRUE) {

            #Pass data as raw_features_m_df
            raw_features_m_df <- data

            #Initial checks
            #################
              ##Check that meta_dataframe names match
              if (!stringr::str_detect(raw_features_m_df@meta_dataframe_name, tickers_catalog@meta_dataframe_name) && verbose) {
                message(paste0("Applying ", tickers_catalog@meta_dataframe_name, " tickers_catalog to ", raw_features_m_df@meta_dataframe_name))
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

                  ###In cases of ticker changes, two distinct tickers have a same perm_id
                  ###This way, the new ticker first quote will be mapped with ticker change date, not with old ticker first quote
                  ###In a daily freq meta_dataframe, this might lead to incorrect exclusions
                  ###Group 'catalog' by 'perm_id' to handle tickers sharing the same perm_id
                  catalog_summary <- dplyr::group_by(catalog, perm_id) %>%
                    dplyr::summarize( #Summarize to get min and max dates for each perm_id
                      min_first = if (all(is.na(tickers_first_quote))) as.Date(NA) else min(tickers_first_quote, na.rm = TRUE),
                      max_last  = if (all(is.na(tickers_last_quote)))  as.Date(NA) else max(tickers_last_quote,  na.rm = TRUE),
                      .groups   = "drop"
                    )

                  ###Join those summarized columns back into main data 'raw_features_m_df'
                  raw_features_m_df <- dplyr::left_join(raw_features_m_df, catalog_summary, by = "perm_id") %>%
                    ####Replace the original quotes with the summarized min/max, converting ±Inf to NA when all were NA
                    dplyr::mutate(
                      tickers_first_quote = dplyr::if_else(is.infinite(min_first), as.Date(NA), min_first),
                      tickers_last_quote  = dplyr::if_else(is.infinite(max_last),  as.Date(NA), max_last)
                    ) %>% dplyr::select(-min_first, -max_last) #Remove unnecessary columns


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

#' @rdname read_tickers_catalog
setMethod(
  "read_tickers_catalog",
  signature(data = "returns_meta_xts", tickers_catalog = "tickers_catalog"),
  function(data, tickers_catalog, verbose = TRUE) {

    #Pass data as returns_meta_xts
    returns_meta_xts <- data

    #Initial checks
    ################
      ##Check that year and month of versions match
      if (lubridate::year(returns_meta_xts@current_date) != lubridate::year(tickers_catalog@current_date) ||
          lubridate::month(returns_meta_xts@current_date) != lubridate::month(tickers_catalog@current_date)) {
        stop("The year and month of returns_meta_xts do not match the ones in tickers_catalog")
      }
      ##Check that day match and just warn if it does not
      if (lubridate::day(returns_meta_xts@current_date) != lubridate::day(tickers_catalog@current_date)) {
        warning("The day of returns_meta_xts does not match the one in tickers_catalog")
      }
      ##Check that day of returns_meta_xts is not higher than the one in tickers_catalog
      if (lubridate::day(returns_meta_xts@current_date) > lubridate::day(tickers_catalog@current_date)) {
        stop("The day of returns_meta_xts is higher than the one in tickers_catalog")
      }
      ##Check if returns_meta_xts contains tickers not present in tickers_catalog
      if (any(!colnames(returns_meta_xts@data) %in% tickers_catalog@catalog$tickers)) {
        stop("Some tickers in returns_meta_xts are not present in tickers_catalog")
      }
      ##Check that no 'old' tickers are present
      if (any(tickers_catalog@old %in% colnames(returns_meta_xts@data))) {
        stop("returns_meta_xts should not have 'old' tickers.")
      }
      ##Check that all listed + delisted (with last_quote > first date in meta_xts) tickers are present
      required_tickers <- tickers_catalog@catalog %>%
        dplyr::filter(!untraded & !old) %>% #First remove untraded and old
        dplyr::filter(tickers_last_quote > zoo::index(returns_meta_xts@data)[1]) %>% #Then filter by last_quote > first date in meta_xts
        dplyr::pull(tickers)

      if (any(!required_tickers %in% colnames(returns_meta_xts@data))) {
        stop("returns_meta_xts must contain all tickers with last_quote > minimum date of the time series.")
      }

    ##############

    #Extract and adjust relevant data
    ###############
      ##Meta xts
      meta_xts_name <- returns_meta_xts@meta_xts_name
      current_date <- returns_meta_xts@current_date
      asset_type <- returns_meta_xts@asset_type
      metric_name <- returns_meta_xts@metric_name
      meta_xts_workflow <- returns_meta_xts@workflow
      source <- returns_meta_xts@source

      returns_meta_xts <- returns_meta_xts@data

      ##Tickers and dates
      current_tickers <- colnames(returns_meta_xts)
      names(source) <- current_tickers
      dates_xts <- zoo::index(returns_meta_xts)

      ##Catalog
      catalog <- tickers_catalog@catalog

    ###############

    #Map current tickers to catalog rows using match
    ###############
      ##Get mapping
      mapping <- match(current_tickers, catalog$tickers)

      ##Identify tickers to keep: those not marked as untraded
      valid <- !catalog$untraded[mapping]
      if (any(!valid)) {
        ###Get removed tickers
        removed_untraded_tickers <- current_tickers[!valid]

        ###Print
        if (verbose) message("Removed tickers classified as untraded: ", paste(removed_untraded_tickers, collapse = ", "))

        ###Subset the xts data to keep only valid tickers
        returns_meta_xts <- returns_meta_xts[, valid, drop = FALSE]

        ###Update current_tickers, source and mapping accordingly
        current_tickers <- current_tickers[valid]
        source <- source[current_tickers]
        mapping <- mapping[valid]
     }

      ##Rename columns using the perm_id from the catalog
      new_names <- catalog$perm_id[mapping]
      colnames(returns_meta_xts) <- new_names
    ###############

    #For each remaining ticker column, set observations to NA when outside the valid trading range.
    ###############
      ##Initialize a summary vector for NA transformations per ticker
      na_imputation_summary <- setNames(integer(length(new_names)), new_names)

      ##Loop through cols
      for (i in seq_along(new_names)) {
        ###Get valid trading dates from the catalog for the current ticker
        first_quote <- as.Date(catalog$tickers_first_quote[mapping[i]])
        last_quote  <- as.Date(catalog$tickers_last_quote[mapping[i]])

        ###Identify rows where the observation date is before first_quote or after last_quote.
        ###If first_quote or last_quote are NA, the corresponding condition is not applied.
        out_before <- if (!is.na(first_quote)) which(dates_xts < first_quote) else integer(0)
        out_after  <- if (!is.na(last_quote))  which(dates_xts > last_quote)  else integer(0)
        out_idx <- union(out_before, out_after)

        ###Update the summary with the number of rows transformed to NA
        na_imputation_summary[new_names[i]] <- length(out_idx)

        ###Set the observations to NA
        if (length(out_idx) > 0) {
          returns_meta_xts[out_idx, i] <- NA
        }
      }
   ###############

  #Update the meta_xts object's data slot and return the modified object
  ###############
  names(na_imputation_summary) <- lookup_catalog(tickers_catalog, perm_id_to_lookup = names(na_imputation_summary)) %>% unname()
  new_workflow <-
        list(
          list(current_date = current_date, #Current date
               timestamp = Sys.time(), #Timestamp
               removed_untraded_tickers = removed_untraded_tickers, #Tickers removed
               na_imputation_summary = na_imputation_summary
               )
        )

  pre_silver_returns_meta_xts <- create_meta_xts(data = returns_meta_xts, type = "returns", asset_type = asset_type,
                                                 metric_name = metric_name, meta_xts_name = meta_xts_name,
                                                 workflow = c(meta_xts_workflow, new_workflow), source = source
  )

  names(pre_silver_returns_meta_xts@workflow)[length(pre_silver_returns_meta_xts@workflow)] <- paste0("read_tickers_catalog_", current_date)

  return(pre_silver_returns_meta_xts)

  }
)
