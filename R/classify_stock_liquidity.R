#' Classify stocks based on their liquidity
#'
#' @param liquidity_m_df A data frame (similar to `features_m_df`) containing columns for id, tickers, dates, and one or more market liquidity measures (e.g., inflation-adjusted mean financial volume).
#' All tickers in current stock universe must have a unique correspondence in this data frame.
#' @param liquidity_floor_cutoffs_list Optional. A list of named vectors containing cutoff values for liquidity metrics specified in `liquidity_m_df`.
#' The names should match the metrics and values should be the minimum acceptable values (adjust for inflation)
#' Stocks that have all metrics higher than defined in a `liquidity_floor_cutoffs_list` element will receive a liquidity classification at least equal to it.
#' Elements should be: "micro_caps", "small_caps", "mid_caps", "large_caps" and "mega_caps"
#' Classification should be in ascending order (from lest liquid to most liquid) for all metrics.
#' If set in decimals, values will be interpreted as quantiles and classification will be set according to quantiles
#' @param apply_liquidity_floor_rule If TRUE, stocks that fall below the classification in liquidity_floor_rule will be assigned a value of 0
#' @param liquidity_floor_rule Optional. Character string specifying the liquidity classification to apply the liquidity floor rule (eg. "nano_caps", "micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps").
#' @param filter_out_liquidity_floor_rule If TRUE, stocks that that fall below the classification in liquidity_floor_rule_policy will be filtered out
#'
#' @return
#' @export
#'
#' @examples
classify_stock_liquidity <- function(liquidity_floor_cutoffs_list, liquidity_m_df,
                                     liquidity_floor_rule = NULL, apply_liquidity_floor_rule = FALSE,
                                     filter_out_liquidity_floor_rule = FALSE, verbose = TRUE){

  ###Get objects
  ################
  ##Liquidity Floor Rule DF
  liquidity_floor_rule_df <- liquidity_m_df #Init dataframe
  ##Get liquidity metrics names
  liquidity_metrics <- names(liquidity_floor_cutoffs_list[[1]]) #Get name of metrics
  #################

  #####Check objects
  #####################################

  ###Checks
  ###Transpose df to facilitate manipulation
  transposed_liquidity_floor_cutoffs_df <- t(as.data.frame(liquidity_floor_cutoffs_list))
  ###Make sure liquidity_floor_cutoffs_list is correctly ordered
  if(!all(transposed_liquidity_floor_cutoffs_df == transposed_liquidity_floor_cutoffs_df[order(transposed_liquidity_floor_cutoffs_df[,1]),])){
    stop("liquidity_floor_cutoffs_list is not in ascending order")
  }
  ###Make sure orders of metrics in liquidity_floor_cutoffs_list match
  if(!all(transposed_liquidity_floor_cutoffs_df %>% #Transform to data frame and invert
          apply(2, function(x) order(x)) %>% #Get the order of each col
          apply(1, function(x) length(unique(x)) == 1))){ #Check if each row is equal
    stop("liquidity metrics orders in liquidity_floor_cutoffs_list are conflicting")
  }
  ###Check if liquidity_floor_rule_policy is in liquidity_floor_cutoffs_list
  if(all(!is.null(liquidity_floor_rule), !liquidity_floor_rule %in% names(liquidity_floor_cutoffs_list))){
    stop("liquidity_floor_rule not included in liquidity_floor_cutoffs_list")
  }

  ###Check if liquidity_floor_rule_policy is set if apply_liquidity_floor_rule = TRUE
  if(apply_liquidity_floor_rule && (is.null(liquidity_floor_rule))) {
    stop("liquidity_floor_rule can't be missing if apply_liquidity_floor_rule is TRUE")
  }

  ###Check if apply_liquidity_floor_rule is FALSE when filter_out_liquidity_floor_rule = TRUE
  if(filter_out_liquidity_floor_rule & !apply_liquidity_floor_rule) {
    stop("apply_liquidity_floor_rule can't be FALSE if filter_out_liquidity_floor_rule is TRUE")
  }

  ###Set liquidity_floor_cutoffs_list according to quantiles if needed
  if(all(transposed_liquidity_floor_cutoffs_df <= 1 & transposed_liquidity_floor_cutoffs_df >= 0)){ #Check if quantiles were set
    if(verbose){
      warning("liquidity_cutoffs provided as decimals. Overwritting with quantiles")
    }
    #For each classification
    for(l in 1:length(liquidity_floor_cutoffs_list)){
      #For each columns

      for(j in 1:length(liquidity_floor_cutoffs_list[[l]])){
        liquidity_floor_cutoffs_list[[l]][j] <- quantile(liquidity_floor_rule_df[,liquidity_metrics][,j], #Calculate j-th metric quantile
                                                         liquidity_floor_cutoffs_list[[l]][j]) #according to j-th probs
      }
    }
  }
  #####################################


  ###Classify stocks
  ###################
  liquidity_floor_rule_df$liquidity_classification <- "nano_caps" #First assign all to smallest classification
  for(i in 1:nrow(liquidity_floor_rule_df)){
    #For each row
    ith_stock_row <- liquidity_floor_rule_df[i, liquidity_metrics] #Get liquidity
    for(l in 1:length(liquidity_floor_cutoffs_list)){
      #For each list element
      if(all(ith_stock_row >= liquidity_floor_cutoffs_list[[l]])){ #Condition is stock liquidity being greater than all metrics
        liquidity_floor_rule_df$liquidity_classification[i] <- names(liquidity_floor_cutoffs_list)[l] #Rename
      }
    }
  }
  ###################

  ###Apply liquidity_floor rule
  ###############################
  if(apply_liquidity_floor_rule){
    liquidity_floor_rule_df$liquidity_floor <- 1
    for(i in 1:nrow(liquidity_floor_rule_df)){
      #For each row
      ith_stock_row <- liquidity_floor_rule_df[i, liquidity_metrics] #Get liquidity
      if(any(ith_stock_row < liquidity_floor_cutoffs_list[[liquidity_floor_rule]])){ #Condition is stock liquidity being less than at least one metric
        liquidity_floor_rule_df$liquidity_floor[i] <- 0 #Apply
      }
    }
  }

  ###Eliminate if told so
  if(filter_out_liquidity_floor_rule){
    if(verbose){
      cat("\n")
      cat(paste0("Stock with a liquidity classification worst than ", liquidity_floor_rule, " will be excluded."))
      cat("\n")
    }
    liquidity_floor_rule_df <- liquidity_floor_rule_df %>% dplyr::filter(liquidity_floor == 1)
  }
  #############################

  #Return result
  return(liquidity_floor_rule_df)

}
