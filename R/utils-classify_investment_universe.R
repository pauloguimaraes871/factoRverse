#' Classify the universe based on signals and other custom and user-defined rules.
#'
#' The eligibility of a stock/signal portfolio depends on a series of criteria, as explained in Details. Default behavior is to apply only
#' the **Only Top Assets Rule**, in which case assets are promoted based on their signal being above a given quantile.
#'
#' The function provides additional custom rules and also accepts user-defined rules.
#'
#' ## Eligibility Criteria
#' To be promoted as eligible, assets must meet one of the following criteria:
#'
#' 1. **Regular Eligibility**
#'    - **Only Top Assets Rule**: Asset must be in the top quantile as specified by `top_quantile`.
#'      - To ignore this behavior, set `top_quantile` to 0.
#'    - **Liquidity Floor Rule** (exclusive for stocks): must meet minimum liquidity requirements as defined by the liquidity floor rule.
#'
#' 2. OR **Active Weights Constraint Policy Eligibility:**
#'    - **Maximum Absolute Individual Active Weight Rule** (exlusive for stocks): Benchmark weight must exceed the maximum absolute individual active weight threshold.
#'
#' 3. OR **Turnover Policy Eligibility:** (exclusive for stocks)
#'    - Stock must be in one of the buffer zones. For this to happen:
#'      - Stock must be in the top quantile buffer (`signal >= top_quantile_buffer`).
#'      - Stock must be in the pre-rebalancing portfolio.
#'      - Stock must meet the liquidity classification of the buffer zone.
#'
#'
#' 4. OR **user_defined_OR_rules Eligibility** (currently only implemented for stocks)
#'
#' 5. OR **Group Representativeness Eligibility:**
#'    - If there are no stocks or signal portfolios in one of the groups specified in `concentration_constraint_policy`, a representative will be included according to the best quantile.
#'
#' 6. AND **user_defined_AND_rules**
#'
#' ## Dominance of Rules
#' - The **Active Weights Constraint Policy Eligibility** is dominant; assets meeting this rule will always be eligible.
#' - The **Turnover Policy Eligibility** takes precedence over the **Liquidity Floor Rule**; thus, a stock in the buffer zone will be included even if the liquidity floor rule suggests otherwise.
#' - Assets that meet **user_defined_OR_rules** will always be promoted.
#' - Assets that fail to meet **user_defined_AND_rules** will always be excluded.
#'
#' @param signals_m_d_ref A data frame of stocks with signals columns.
#' @param top_assets_quantile Optional. Numeric value to apply the **Only Top Assets Rule**, indicating the top quantile for filtering assets based on signal.
#'   Only assets in this quantile will be considered for the `filtered_universe`.
#' @param liquidity_m_d_ref A data frame  containing columns for id, tickers, dates, and one or more market liquidity measures (e.g., inflation-adjusted mean financial volume).
#'  All tickers in the current universe must have a unique correspondence in this data frame.
#' @param liquidity_constraint_policy Optional. A named list containing objects used to apply liquidity constraints. Possible elements of the list are:
#' - `liquidity_floor_rule`: A character indicating the liquidity classification (e.g., micro_caps, small_caps) used to filter stocks. Stocks with less liquidity than specified in `liquidity_floor_rule` will be considered ineligible.
#'   In the case of the `generate_box_constraints` function, `liquidity_constraint_policy` can also contain:
#' - `liquidity_cap_rule` lists: One or many lists used to create upper bounds for weights based on a liquidity classification. Each list must contain:
#'   - `liquidity_classification`: A character indicating the classification for the cap.
#'   - `liquidity_cap`: A numeric value indicating the cap (upper bound) for stocks with that liquidity classification.
#'   Many liquidity caps might be created, and in this case, each `liquidity_cap_rule` must be identified with a number (e.g., liquidity_cap_rule_1, liquidity_cap_rule_2, and so on).
#' @param portfolio_weights_m_lstd_ref  A data frame containing columns for id, tickers, dates, and weights from the old portfolio (pre-rebalancing).
#' All tickers in the current stock universe must have a unique correspondence in this data frame.
#' @param turnover_constraint_policy A named list containing objects used to build buffer zones and apply turnover constraints.
#' - Each element will constitute a `buffer_zone`, being a list with three elements:
#'   - `liquidity_classification` element: A liquidity classification (e.g., "micro_caps", "small_caps") for that buffer zone.
#'   - `top_quantile_buffer`: A numeric value indicating a buffer value that relaxes `top_assets` for stocks with the specified liquidity classification.
#'   - `turnover_cap`: A numeric value specifying the turnover cap.
#'   Stocks that are less liquid than specified for a buffer zone and have a signal higher than the respective buffer quantile will be considered eligible, even if they do not meet the `liquidity_floor_rule`.
#' @param benchmark_weights_m_d_ref A data frame containing columns for id, tickers, dates, and current benchmark weights columns.
#'  All tickers in the current universe must have a unique correspondence in this data frame.
#' @param groups_m_d_ref A data frame containing columns for id, tickers, dates, and group classification columns following a given classification method.
#' All tickers in the current universe must have a unique correspondence in the data frame.
#' @param concentration_constraints_policy A named list containing up to four elements:
#' - `benchmark`: A character vector describing the benchmark to be used to apply constraint.
#' Must have a correspondence in `benchmark_weights_m_d_ref`
#' - `max_abs_active_individual_weight`: The maximum absolute individual active weights.
#' - `group_classification`: A character vector describing the group classification to be used to apply group constraints.
#' Must have a correspondence in `group_m_d_ref`
#' - `max_abs_active_group_weight`: The maximum absolute group active weight used for creating group constraints in `generate_group_constraints`.
#' If a given group has no eligible asset, the one with the greatest signal will be automatically promoted.
#' Note that, in the context of `generate_group_constraints`, a `benchmark_weights_m_d_ref` data frame must also be supplied.
#' @param liquidity_floor_cutoffs_list Mandatory if `turnover_constraint_policy` and/or `liquidity_constraint_policy` are provided.
#' A list of named vectors containing cutoff values to classify stocks according to liquidity.
#' Each element must be named according to the 5 following liquidity classifications: ("micro_caps", "small_caps", "mid_caps", "large_caps" and "mega_caps)
#' and the vector must provide named numeric values that indicate the minimum acceptable values (adjusted for inflation) for stocks to have that classification.
#' Classification should be in ascending order (from least liquid to most liquid) for all metrics.
#' If set in decimals, values will be interpreted as quantiles and classification will be set accordingly.
#' Stocks with liquidity lower than micro_caps will receive nano_caps classification.
#' @param user_defined_AND_rules_list Optional. A named list of named data frames containing a column with tickers, columns with metrics to be passed to the final data frame, and a column that describes the filter with the same name as the list element.
#' For example, to apply a filter with stocks that begin with A, `user_defined_AND_rules_list` can contain a data frame element named "starts_with_A_rule".
#' This data frame can contain a metric column (e.g., the name of the stock), and the descriptive filter column must be named "starts_with_A_rule". In this case, the "starts_with_A_rule" column should be an integer, and its values must be either 1L (stock passes rule) or 0L (stock fails to pass rule).
#' The rule will be appended to the filter as a regular promotion rule, accumulating with other rules.
#' @param user_defined_OR_rules_list Optional. A named list of named data frames containing a column with tickers, columns with metrics to be passed to the final data frame, and a column that describes the filter with the same name as the list element.
#'All tickers in the current stock universe must have a unique correspondence in this data frame.
#' @param asset_object A character indicating whether the analysis is being applied to "stocks" or "signal_portfolios"
#' @return
#' @export
classify_investment_universe <- function(signals_m_d_ref, #Signals d_ref
                                         top_assets_quantile = NULL, signal_significance_threshold = NULL, #Signal classification for only_top_assets_rule
                                         liquidity_floor_cutoffs_list = NULL, liquidity_m_d_ref = NULL, liquidity_constraint_policy= NULL, #Liquidity policy
                                         portfolio_weights_m_lstd_ref = NULL, turnover_constraint_policy = NULL, #Turnover policy
                                         benchmark_weights_m_d_ref = NULL, groups_m_d_ref = NULL, concentration_constraint_policy = NULL, #Concentration policy
                                         user_defined_AND_rules_list = NULL, user_defined_OR_rules_list = NULL, #User defined rules
                                         asset_object = "stocks", verbose = TRUE

){
  ###Check objects
  #################
  ##Check if last col is exp_ret_score
  if(!"exp_ret_score" == colnames(signals_m_d_ref)[length(colnames(signals_m_d_ref))]){
    stop("last column of signals_m_d_ref must be exp_ret_score")
  }

  ##Check if liquidity_m_d_ref is for only one date
  if(!is.null(liquidity_m_d_ref) & !length(unique(liquidity_m_d_ref$dates)) == 1){
    stop("liquidity_m_d_ref should have only one date")
  }

  ##Check if portfolio_weights_m_lstd_ref is for only one date
  if(!is.null(portfolio_weights_m_lstd_ref) & !length(unique(portfolio_weights_m_lstd_ref$dates)) == 1){
    stop("portfolio_weights_m_lstd_ref should have only one date")
  }

  ##Check if benchmark_weights_m_d_ref is for only one date
  if(!is.null(benchmark_weights_m_d_ref) & !length(unique(benchmark_weights_m_d_ref$dates)) == 1){
    stop("benchmark_weights_m_d_ref should have only one date")
  }

  ##Check possibilities for liquidity_floor_rule_policy
  if(!is.null(liquidity_constraint_policy$liquidity_floor_rule) &&
     !liquidity_constraint_policy$liquidity_floor_rule %in% c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")){
    stop("liquidity_floor_rule not supported")
  }

  ##Check additional args needed for liquidity_floor_rule
  if(!is.null(liquidity_constraint_policy$liquidity_floor_rule) &&
     (is.null(liquidity_m_d_ref) || is.null(liquidity_floor_cutoffs_list))){
    stop("liquidity_m_d_ref and liquidity_floor_cutoffs_list can't be missing if liquidity_floor_rule is set")
  }

  ##Check additional args needed for turnover_constraint_policy
  if(!is.null(turnover_constraint_policy) &&
     (is.null(portfolio_weights_m_lstd_ref) || is.null(liquidity_m_d_ref) || is.null(liquidity_floor_cutoffs_list))){
    stop("portfolio_weights_m_lstd_ref, liquidity_floor_cutoffs_list and liquidity_m_d_ref can't be missing if turnover_constraint_policy is set")
  }

  ##Check additional args needed for max_abs_active_weight_individual_rule
  if(!is.null(concentration_constraint_policy) && (asset_object == "stocks") &&
     (is.null(benchmark_weights_m_d_ref))){
    stop("benchmark_weights_m_d_ref can't be missing if concentration_constraint_policy is set")
  }

  ##Check if benchmark_weights sum to 1
  if(!is.null(benchmark_weights_m_d_ref) && any(colSums(select(benchmark_weights_m_d_ref, -tickers, -id, -dates)) != 1)){
    stop("benchmark weights must sum to 1")
  }

  ##Check user_defined_OR_list
  if(!is.null(user_defined_OR_rules_list)){
    if(!all(names(user_defined_OR_rules_list) == as.vector(sapply(user_defined_OR_rules_list, function(x) colnames(x)[ncol(x)])))){
      stop("colnames in last element of user_defined_OR_rules_list does not match object's name")
    }
  }

  ##Check user_defined_AND_list
  if(!is.null(user_defined_AND_rules_list)){
    if(!all(names(user_defined_AND_rules_list) == as.vector(sapply(user_defined_AND_rules_list, function(x) colnames(x)[ncol(x)])))){
      stop("colnames in last element of user_defined_AND_rules_list does not match object's name")
    }
  }

  ##################

  ###Apply Rules

  ###Current Universe
  universe_m_d_ref <- signals_m_d_ref #Init dataframe

  ###Only Top Assets Rule for signals
  if(asset_object == "signals"){
    try(pd_alpha <- universe_m_d_ref[,"pd_alpha"], silent = TRUE) #Get bayesian p-value

    #Bayesian choice
    ###################
    if(exists("pd_alpha")){
      converted_pd_alpha <- 1 - pd_alpha #Convert to one-sided p-value (P = 1 - pd))
        ###Define top assets
        universe_m_d_ref$top_assets <- ifelse(converted_pd_alpha <= signal_significance_threshold, 1L, 0L)  #If PD > threshold, can asset alpha is positive
    }
    #Frequentist choice
    ###################
    else {
      adjusted_p_value <- universe_m_d_ref[,"adjusted_p_value"] #Get frequentist adjusted p-value
      try(alpha <- universe_m_d_ref[, "alpha"], silent = TRUE) #Get no-pooled alpha
      if(!exists("alpha")) alpha <-  universe_m_d_ref[, "individual_alpha"] #Get pooled alpha
        ###Define top assets
        universe_m_d_ref$top_assets <- ifelse(adjusted_p_value <= signal_significance_threshold & alpha > 0, 1L, 0L) #If p-value < threshold, can assert alpha is positive
    }

    #Check if there are eligible signals
    if(all(na.omit(universe_m_d_ref$top_assets) == 0)) stop("No signal was deemed significant.")

    #Print
    if(verbose){
      cat("The following signals showed statistical significant alphas:",
          paste(universe_m_d_ref$tickers[which(universe_m_d_ref$top_assets == 1)], collapse = ", "), "\n")
    }
    #Create benchmark_weights
    benchmark_weights_m_d_ref <- create_se_benchmarks(signal_universe_m_d_ref = universe_m_d_ref, selected_signal_themes_m_d_ref = groups_m_d_ref)

  }
  ###Only Top Assets Rule for Stocks
  else {
    universe_m_d_ref$top_assets <- ifelse(signals_m_d_ref[,"exp_ret_score"] >= quantile(signals_m_d_ref[,"exp_ret_score"], top_assets_quantile), 1L,0L) #Apply rule
    #Print
    if(verbose){
      cat("The following stocks have a exp_ret_score above the specified quantile for top_assets:",
          paste(universe_m_d_ref$tickers[universe_m_d_ref$top_assets == 1], collapse = ", "), "\n")
    }
  }

  ###Liquidity Floor Rule
  #########################
  if(!is.null(liquidity_constraint_policy)){ #If liquidity_floor_cutoffs_list is NULL, do not apply rule

    #Apply liquidity_floor_rule
    liquidity_floor_rule_m_d_ref <- classify_stock_liquidity(
      liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, #How to classify
      liquidity_m_df = liquidity_m_d_ref, #Liq data
      liquidity_floor_rule = liquidity_constraint_policy$liquidity_floor_rule, #Floor
      apply_liquidity_floor_rule = !is.null(liquidity_constraint_policy$liquidity_floor_rule), #Checks if floor is provided
      filter_out_liquidity_floor_rule = FALSE, verbose = FALSE)

    #Remove id and dates (this is necessary bco classify_stock_liquidity can be used outside the function)
    liquidity_floor_rule_m_d_ref <- liquidity_floor_rule_m_d_ref %>% select(-id, -dates)

    #Include in universe_m_d_ref
    universe_m_d_ref <- dplyr::left_join(universe_m_d_ref, liquidity_floor_rule_m_d_ref, by = "tickers")

  }
  #########################

  ###Active Weights Constraint Policy
  ########################
  if(!is.null(concentration_constraint_policy$benchmark)){ #If max_abs_active_individual_weight is NULL, do not apply rule
    ##Maximum Absolute Active Individual Weight Rule DF
    #Select benchmark
    selected_benchmark <- concentration_constraint_policy$benchmark

    #Select only weights of that benchmark
    max_abs_active_weight_individual_rule_m_d_ref <- benchmark_weights_m_d_ref %>%
      dplyr::select(-id, -dates) %>%#Init dataframe
      dplyr::select(tickers, dplyr::all_of(selected_benchmark)) #Select onl tickers and weights

    ##Apply Maximum Absolute Active Individual Weight Rule if present in policy
    if(!is.null(concentration_constraint_policy$max_abs_active_individual_weight)){
      max_abs_active_weight_individual_rule_m_d_ref$max_abs_aw_ind <- #Apply Maximum Absolute Active Individual Weight Rule
        ifelse(max_abs_active_weight_individual_rule_m_d_ref[,2] >= concentration_constraint_policy$max_abs_active_individual_weight, 1L, 0L) #Is stock relevant in benchmark?
      }

      #Include in universe_m_d_ref
      universe_m_d_ref <- dplyr::left_join(universe_m_d_ref, max_abs_active_weight_individual_rule_m_d_ref, by = "tickers")
      #Rename
      colnames <- colnames(universe_m_d_ref)
      colnames(universe_m_d_ref)[which(colnames %in% selected_benchmark)] <- paste0(selected_benchmark, "_bench_weights")

    }


  #######################

  ###Turnover Policy
  ######################
  if(!is.null(turnover_constraint_policy)){

    #Checks for existence of liquidity_floor_cutoffs_list
    if(is.null(liquidity_floor_cutoffs_list)){
      stop("Application of turnover policy depends on existence of liquidity_floor_cutoffs_list")
    }

    #Apply Buffer Rules Iteratively
    for(l in 1:length(turnover_constraint_policy)){

      buffer_rule_m_d_ref <- apply_buffer_rule(
        signals_m_d_ref = signals_m_d_ref, #Signal DF
        top_assets_quantile_buffer = turnover_constraint_policy[[l]]$top_stock_quantile_buffer, #Quatile for BUffer
        portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref, #Old weights
        liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, #Liquidity floor
        liquidity_m_d_ref= liquidity_m_d_ref, #Liquidit data
        buffer_rule = turnover_constraint_policy[[l]]$liquidity_classification #Buffer rule policy
        )

      #Include in universe_m_d_ref
        ##Exclude old portfolio weights col to avoid repetition
        if(l != 1){
          buffer_rule_m_d_ref$old_portfolio_weights <- NULL
        }
        ##Merge
        universe_m_d_ref <- dplyr::full_join(universe_m_d_ref, buffer_rule_m_d_ref, by = "tickers") %>%
          dplyr::select(-is_in_top_quantile_buffer, -was_in_old_portfolio, -does_liquidity_meets_buffer_rule) #Drop those columns

      #Rename
      colnames <- colnames(universe_m_d_ref)
      colnames(universe_m_d_ref)[which(colnames == "buffer_rule")] <- names(turnover_constraint_policy)[l]
    }


  }
  ######################

  ###Classify groups
  if(!is.null(groups_m_d_ref)){
      #Merge group classification
      universe_m_d_ref <- dplyr::left_join(universe_m_d_ref, dplyr::select(groups_m_d_ref, -id, -dates), #Avoid duplication
                                           by = "tickers")
  }

  ###Promote stocks to filtered_stock_universe

  #Consolidated Promotion Criteria:

  ##1. Regular Eligibility


  #1.1. Stock must be in the top quantile of stocks AND
  universe_m_d_ref$is_eligible <- universe_m_d_ref$top_assets

  #1.2. Stock must meet minimum liquidity requirements as defined by the liquidity floor rule.
  if(!is.null(liquidity_constraint_policy$liquidity_floor_rule)){
    universe_m_d_ref$is_eligible <- universe_m_d_ref$is_eligible *
      universe_m_d_ref$liquidity_floor #Exclude based on floor rule
  }

  ##2. Active Individual Weights Constraint Policy Eligibility
  if(!is.null(concentration_constraint_policy$max_abs_active_individual_weight)){
    universe_m_d_ref$is_eligible <- universe_m_d_ref$is_eligible +
      #Select cols that start with "max_abs_aw"
      universe_m_d_ref %>%
      dplyr::select(dplyr::starts_with("max_abs_aw_ind")) %>% #Select cols
      dplyr::rowwise() %>%
      dplyr::mutate(row_sum = sum(dplyr::c_across(dplyr::everything()))) %>% #Sum
      dplyr::pull()
  }

  ##3. Turnover Policy Eligibility:
  if(!is.null(turnover_constraint_policy)){
    universe_m_d_ref$is_eligible <- universe_m_d_ref$is_eligible +
      #Select cols that start with "buffer_zone_"
      universe_m_d_ref %>%
      dplyr::select(dplyr::starts_with("buffer_zone")) %>% #Select cols
      dplyr::rowwise() %>%
      dplyr::mutate(row_sum = sum(dplyr::c_across(dplyr::everything()))) %>% #Sum
      dplyr::pull()

  }

  ##4. User defined OR rules
  if(!is.null(user_defined_OR_rules_list)){
    for(l in 1:length(user_defined_OR_rules_list)){
      #For each list
      universe_m_d_ref <-  merge(universe_m_d_ref, user_defined_OR_rules_list[[l]], all = TRUE, by = "tickers")
      #Apply rule
      universe_m_d_ref$is_eligible <- universe_m_d_ref$is_eligible + universe_m_d_ref[, names(user_defined_OR_rules_list)[l]]
    }
  }

  ##Group Representativeness Eligibility
  if(!is.null(concentration_constraint_policy$max_abs_active_group_weight) & !is.null(groups_m_d_ref)){
    groups <- groups_m_d_ref %>% dplyr::select(-id, -tickers, -dates) %>% colnames()
    #For each group classification
    for(i in 1:length(groups)){
      group_classification_m_d_ref <- universe_m_d_ref[, c("is_eligible", groups[i])]

      #Get eligible and ineligible groups
      eligible_groups <- unique(group_classification_m_d_ref[which(group_classification_m_d_ref$is_eligible == 1), 2]) #group with eligible stocks
      ineligible_groups <- setdiff(group_classification_m_d_ref[,groups[i]], eligible_groups) #groups with ineligible stocks

      #If there are ineligible groups
      if(length(ineligible_groups > 0)){ #Check if there are ineglibile groups
        for(j in 1:length(ineligible_groups)){
          ##For each ineligible group
          assets_in_ineligible_groups <- universe_m_d_ref[ #Get stocks that belong to the ineligible group
            which(universe_m_d_ref[,groups[i]] == ineligible_groups[j]),]

          ##Replace is_eligible for 1 for that asset high highest signal
          best_ineligible_asset <- assets_in_ineligible_groups$tickers[which.max(assets_in_ineligible_groups$exp_ret_score)]
          universe_m_d_ref[which(universe_m_d_ref$tickers == best_ineligible_asset),"is_eligible"] <- 1

          ##Print
          if(verbose && length(best_ineligible_asset) > 0){
            if(asset_object == "signals"){
              cat(crayon::yellow("Because of theme representativeness,", best_ineligible_asset, "was promoted as representative of theme", ineligible_groups[j]))
            } else {
              cat(crayon::yellow("Because of group representativeness,", best_ineligible_asset, "was promoted as representative of group", ineligible_groups[j]))
            }
          }
        }
      }
    }
  }


  ##6. User defined AND rules
  if(!is.null(user_defined_AND_rules_list)){
    for(l in 1:length(user_defined_AND_rules_list)){
      #For each list
      universe_m_d_ref <-  merge(universe_m_d_ref, user_defined_AND_rules_list[[l]], all = TRUE, by = "tickers")
      #Apply rule
      universe_m_d_ref$is_eligible <- universe_m_d_ref$is_eligible * universe_m_d_ref[, names(user_defined_AND_rules_list)[l]]
    }
  }



  #Rearrange
  universe_m_d_ref$is_eligible[universe_m_d_ref$is_eligible > 1] <- 1
  universe_m_d_ref <- universe_m_d_ref[, c(setdiff(colnames(universe_m_d_ref), "is_eligible"), "is_eligible")]

  ##Return results
  return(universe_m_d_ref)

}

