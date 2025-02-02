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
classify_stock_liquidity <- function(liquidity_floor_cutoffs, liquidity_m_df,
                                     liquidity_floor_rule = NULL, apply_liquidity_floor_rule = FALSE,
                                     filter_out_liquidity_floor_rule = FALSE, verbose = TRUE){

  ###Get objects
  ################
  ##Liquidity Floor Rule
  liquidity_floor_rule_m_df <- liquidity_m_df #Init dataframe
  ##Get liquidity metrics names
  liquidity_metrics <- colnames(liquidity_floor_cutoffs)[-1] #Get name of metrics
  #################

  #####Check objects
  #########################

  ###Check if liquidity_floor_rule is set if apply_liquidity_floor_rule = TRUE
  if (apply_liquidity_floor_rule && (is.null(liquidity_floor_rule))) {
    stop("liquidity_floor_rule can't be missing if apply_liquidity_floor_rule is TRUE")
  }
  ###Check if apply_liquidity_floor_rule is FALSE when filter_out_liquidity_floor_rule = TRUE
  if (filter_out_liquidity_floor_rule & !apply_liquidity_floor_rule) {
    stop("apply_liquidity_floor_rule can't be FALSE if filter_out_liquidity_floor_rule is TRUE")
  }
  ###Check if liquidity_floor_cutoffs is a data.frame
  if (!is.data.frame(liquidity_floor_cutoffs)){
    stop("liquidity_floor_cutoffs must be a data.frame")
  }
  ###Make sure liquidity_floor_cutoffs is correctly ordered
  if (!all(liquidity_floor_cutoffs == dplyr::arrange(liquidity_floor_cutoffs, !!rlang::sym(liquidity_metrics[1])))){
    stop("liquidity_floor_cutoffs is not in ascending order")
  }
  ###Make sure orders of metrics in liquidity_floor_cutoffs_list match
  if (!all(liquidity_floor_cutoffs %>%
           dplyr::select(-liquidity_classification) %>%
           apply(2, function(x) order(x)) %>% #Get the order of each col
           apply(1, function(x) length(unique(x)) == 1))){ #Check if each row is equal
    stop("liquidity metrics orders in liquidity_floor_cutoffs are conflicting")
  }
  ###Check if names match expectations
  if (!all(dplyr::pull(liquidity_floor_cutoffs, liquidity_classification) %in% c("nano_caps", "micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"))){
    stop("liquidity_floor_cutoffs must contemplate only the following names: 'nano_caps', 'micro_caps', 'small_caps', 'mid_caps', 'large_caps' and 'mega_caps'")
  }
  ###Check if all but first column is numeric
  if (!all(sapply(liquidity_floor_cutoffs[, -1], is.numeric))){
    stop("liquidity_floor_cutoffs elements must be numeric")
  }
  ###Check if there are NAs
  if (any(is.na(liquidity_floor_cutoffs))){
    stop("liquidity_floor_cutoffs elements must not have NAs")
  }
  ###Check if liquidity_floor_rule is contemplated
  if (!is.null(liquidity_floor_rule) && !liquidity_floor_rule %in% dplyr::pull(liquidity_floor_cutoffs, liquidity_classification)){
    stop("liquidity_floor_rule must be contemplated in liquidity_floor_cutoffs")
  }

  #########################

  ###Set liquidity_floor_cutoffs according to quantiles if needed
  ###########################
  if (all(dplyr::select(liquidity_floor_cutoffs, -liquidity_classification) <= 1 &
          dplyr::select(liquidity_floor_cutoffs, -liquidity_classification) >= 0)){ #Check if quantiles were set
    if (verbose){
      message("liquidity_cutoffs provided as decimals. Overwritting with quantiles")
    }
    n_dates <- liquidity_m_df %>% dplyr::pull(dates) %>% unique() %>% length()
    if (n_dates > 1){
      stop("For working with decimals, there should be onl one date in liquidity_m_df.")
    }
    #For each classification, replace quantiles with corresponding values
    liquidity_floor_cutoffs <- liquidity_floor_cutoffs %>%
      dplyr::mutate(
        dplyr::across( #Across all columns, but the first
          .cols = -liquidity_classification,
          .fns = function(probs) { #Apply the function
            col_name <- dplyr::cur_column() #Get column name
            sapply(probs, function(p) as.numeric(stats::quantile(liquidity_m_df[[col_name]],
                                                                 probs = p,
                                                                 na.rm = TRUE)))
          }
        )
      ) %>% as.data.frame() #Convert to dataframe

  }
  #########################


  ###Classify stocks
  #########################
  liquidity_floor_rule_m_df <- liquidity_floor_rule_m_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(liquidity_classification = {
      # For each row in liquidity_floor_cutoffs, check if all liquidity metrics
      # in the current stock row are greater than or equal to the corresponding cutoff.
      satisfied <- purrr::map_lgl(
        .x = seq_len(nrow(liquidity_floor_cutoffs)),
        .f = function(i) {
          all(dplyr::c_across(dplyr::all_of(liquidity_metrics)) >=
                as.numeric(liquidity_floor_cutoffs[i, liquidity_metrics]))
        }
      )
      # If at least one threshold is satisfied, assign the classification corresponding
      # to the highest threshold (i.e., the one with the largest index); otherwise "nano_caps".
      if (any(satisfied)) {
        liquidity_floor_cutoffs$liquidity_classification[max(which(satisfied))]
      } else {
        "nano_caps"
      }
    }) %>%
    dplyr::ungroup() %>%
    as.data.frame()

  #########################

  ###Apply liquidity_floor rule
  #########################
  if (apply_liquidity_floor_rule) {
    # Extract the thresholds for the given liquidity_floor_rule from liquidity_floor_cutoffs.
    # This yields a named numeric vector with one cutoff per liquidity metric.
    thresholds <- liquidity_floor_cutoffs %>%
      dplyr::filter(liquidity_classification == liquidity_floor_rule) %>%
      dplyr::select(dplyr::all_of(liquidity_metrics)) %>%
      unlist()  # unlist to get a named numeric vector

    # For each row in liquidity_floor_rule_m_df (which is initialized as liquidity_m_df),
    # create a new column 'liquidity_floor' that is set to liquidity_floor_rule (e.g., "micro_caps")
    # if all liquidity metrics in that row are at or above the corresponding thresholds;
    # otherwise, assign "0" (indicating exclusion due to low liquidity).

    liquidity_floor_rule_m_df <- liquidity_floor_rule_m_df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        liquidity_floor = dplyr::if_else(
          # Compare each liquidity metric with its corresponding threshold:
          any(dplyr::c_across(dplyr::all_of(liquidity_metrics)) < thresholds),
          0L,  # If any metric does not meet all threshold, assign for exclusion (e.g., "micro_caps")
          1L   # Otherwise, assign "1" (keep the stock)
        )
      ) %>%
      dplyr::ungroup() %>%
      as.data.frame()
  }


  ###Eliminate if told so
  if (filter_out_liquidity_floor_rule){
    if (verbose){
      cat("\n")
      cat(paste0("Stock with a liquidity classification worst than ", liquidity_floor_rule, " will be excluded."))
      cat("\n")
    }
    liquidity_floor_rule_m_df <- liquidity_floor_rule_m_df %>% dplyr::filter(liquidity_floor == 1)
  }
  #############################

  #Return result
  return(liquidity_floor_rule_m_df)

}
