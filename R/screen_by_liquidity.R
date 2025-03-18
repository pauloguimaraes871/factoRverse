#' Generic function for screening a meta_dataframe based on liquidity classification, given a liquidity_floor_rule.
#'
#' This function filters a `meta_dataframe` using `classify_stock_liquidity()` to remove illiquid stocks.
#'
#' @param meta_dataframe A `meta_dataframe` object.
#' @param liquidity_m_df A `liquidity_m_df` meta_dataframe containing one or more market liquidity measures (e.g., inflation-adjusted mean financial volume).
#' All ids in meta_dataframe must have a unique correspondence to this object.
#' @param liquidity_floor_cutoffs A data.frame containing cutoff values for liquidity metrics specified in `liquidity_m_df`.
#' The names should match the metrics and values should be the minimum acceptable values (adjust for inflation)
#' Stocks that have all metrics higher than defined in a `liquidity_floor_cutoffs` element will receive a liquidity classification at least equal to it.
#' Elements should be: "micro_caps", "small_caps", "mid_caps", "large_caps" and "mega_caps"
#' Classification should be in ascending order (from lest liquid to most liquid) for all metrics.
#' If set in decimals, values will be interpreted as quantiles and classification will be set according to quantiles
#' The first column is named "liquidity_classification"
#' It has at most 5 rows and no duplicates or NAs.
#' @param liquidity_floor_rule Optional. Character string specifying the liquidity classification to apply the liquidity floor rule (eg. "nano_caps", "micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps").
#' @param verbose Logical. If TRUE, prints processing messages.
#' @export
setGeneric("screen_by_liquidity", function(meta_dataframe, liquidity_m_df, liquidity_floor_cutoffs, liquidity_floor_rule, ...) {
  standardGeneric("screen_by_liquidity")
})

#' Method for screening a meta_dataframe based on liquidity
#'
#' @inheritParams screen_by_liquidity
#' @return A new `meta_dataframe` object with screened data.
#' @export
setMethod("screen_by_liquidity",
          signature(meta_dataframe = "meta_dataframe",
                    liquidity_m_df = "meta_dataframe",
                    liquidity_floor_cutoffs = "data.frame",
                    liquidity_floor_rule = "character"),
          function(meta_dataframe, liquidity_m_df, liquidity_floor_cutoffs, liquidity_floor_rule, verbose = TRUE) {

  #Initial checks
  ###################
    ##liquidity_m_df
      ###Check for non NAs
      if(!all(sapply(liquidity_m_df@data[,-c(1:3)], function(x) is.numeric(x) && all(!is.na(x))))){
         stop("liquidity_m_df should contain only numeric columns with non-NAs.")
      }
      ###Check if all stocks of signals_m_df are covered in liquidity_m_df
      if(any(!unique(meta_dataframe@data$id) %in% (liquidity_m_df@data %>% dplyr::pull(id)))){
        stop("all ids from meta_dataframe should be present in liquidity_m_df")
      }
      ###Check normalization
      if(any(apply(as.data.frame(liquidity_m_df@data[,-c(1:3)]), 2, function(x) all(x >= -1 & x <= 1)))){
         stop("values in liquidity_m_df should not be normalized")
      }
      ###Current date must match
      if(meta_dataframe@current_date != liquidity_m_df@current_date){
        stop("current_date of meta_dataframe and liquidity_m_df must match")
      }

    ##liquidity_floor_cutoffs
      ###Call validate_liquidity_floor
      validate_liquidity_floor_cutoffs(liquidity_floor_cutoffs)
      ###Check if its present in liquidity_floor_cutoffs
      if (!liquidity_floor_rule %in% dplyr::pull(liquidity_floor_cutoffs, liquidity_classification)){
          stop("liquidity_floor_rule not present in liquidity_floor_cutoffs")
      }
  ###################

  #Apply classify_stock_liquidity
  ###################
    ###Classify liquidity_m_df stocks
    classified_liquidity_m_df <- classify_stock_liquidity(
      liquidity_floor_cutoffs = liquidity_floor_cutoffs,
      liquidity_m_df = liquidity_m_df@data,
      liquidity_floor_rule = liquidity_floor_rule,
      apply_liquidity_floor_rule = TRUE,
      filter_out_liquidity_floor_rule = TRUE,
      verbose = verbose
    )
    ###Get the ids to filter (ids of meta_dataframe not in classified_liquidity_m_df)
    ids_to_filter <- setdiff(meta_dataframe@data$id, classified_liquidity_m_df$id)

    ###Filter out ids
    pre_silver_features_m_df <- meta_dataframe@data %>% dplyr::filter(!id %in% ids_to_filter)

      ###Check if it is empty
      if(nrow(pre_silver_features_m_df) == 0){
        stop("All stocks were filtered out. Please check liquidity_floor_cutoffs and liquidity_floor_rule.")
      }


    ###################

    #Finalize obj
    ###################

    ##Create a new workflow entry
    new_workflow_entry <- list(
      current_date = meta_dataframe@current_date,
      timestamp = Sys.time(),
      call = match.call(),
      liquidity_floor_rule = liquidity_floor_rule,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs,
      liquidity_m_df_name = liquidity_m_df@meta_dataframe_name
    )

    ##Recreate the meta_dataframe with updated workflow
    pre_silver_features_m_df <- create_meta_dataframe(
      pre_silver_features_m_df,
      meta_dataframe_name = meta_dataframe@meta_dataframe_name,
      workflow = c(meta_dataframe@workflow, list(new_workflow_entry))
    )

    ##Rename last workflow step for clarity
    names(pre_silver_features_m_df@workflow)[length(pre_silver_features_m_df@workflow)] <-
      paste0("screen_liquidity", "_", liquidity_floor_rule, "_", pre_silver_features_m_df@current_date)

    return(pre_silver_features_m_df)

    })
