check_metabacktest_inputs <- function(signals_m_df, liquidity_m_df, volatility_m_df, benchmark_weights_m_df,
                                      stock_groups_m_df, signal_groups_m_df){

  #######signals_m_df
  ###################

  #Check for correct format in signals_m_df
  if(!is.data.frame(signals_m_df)){
    stop("signals_m_df should be a data_frame.")
  }

  if(!all(c("id", "tickers", "dates") %in% colnames(signals_m_df))){
    stop("signals_m_df should have id, tickers and dates columns.")
  } else {}

  if(any(sapply(signals_m_df[,-c(1:3)], function(x) any(is.na(as.numeric(as.character(x))))))
  ){
    stop("signals_m_df should contain only numeric columns with non-NAs.")
  }

  suppressWarnings(
    if(any(!is.na(as.numeric(signals_m_df$tickers)))){
      stop("tickers in signals_m_df must be character.")
    })

  if(all(any(!lubridate::is.Date(signals_m_df$dates)) ||
         any(is.na(as.Date(signals_m_df$dates, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
    stop("dates in signals_m_df must be a date object with format %Y-%m-%d.")
  }

  #Check structure of dates_m_vector and signals_m_df$dates
  if(!all(as.character(dates_m_vector) %in% unique(as.character(signals_m_df$dates))) ||
     !all(unique(as.character(signals_m_df$dates)) %in% as.character(dates_m_vector))){
    stop("all dates in dates_m_vector must have a correspondence in signals_m_df")
  } else {}


  ###################

  #######liquidity_m_df
  ###################

  #Check for correct format in liquidity_m_df
  if(!is.data.frame(liquidity_m_df)){
    stop("liquidity_m_df should be a data_frame.")
  }

  if(!all(c("id", "tickers", "dates") %in% colnames(liquidity_m_df))){
    stop("liquidity_m_df should have id, tickers and dates columns.")
  } else {}

  if(any(sapply(as.data.frame(liquidity_m_df[,-c(1:3)]), function(x) any(is.na(as.numeric(as.character(x))))))
  ){
    stop("liquidity_m_df should contain only numeric columns with non-NAs.")
  }

  suppressWarnings(
    if(any(!is.na(as.numeric(liquidity_m_df$tickers)))){
      stop("tickers in liquidity_m_df must be character.")
    })


  if(all(any(!lubridate::is.Date(liquidity_m_df$dates)) ||
         any(is.na(as.Date(liquidity_m_df$dates, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
    stop("dates in liquidity_m_df must be a date object with format %Y-%m-%d.")
  }

  #Check structure of dates_m_vector and liquidity_m_df$dates
  if(!all(as.character(dates_m_vector) %in% unique(as.character(liquidity_m_df$dates))) ||
     !all(unique(as.character(liquidity_m_df$dates)) %in% as.character(dates_m_vector))){
    stop("all dates in dates_m_vector must have a correspondence in liquidity_m_df")
  } else {}


  if(any(apply(as.data.frame(liquidity_m_df[,-c(1:3)]), 2, function(x) all(x >= -1 & x <= 1)))){
    stop("values in liquidity_m_df should not be normalized")
  }



  ###################

  #######volatility_m_df
  ###################

  #Check for correct format in volatility_m_df
  if(!is.data.frame(volatility_m_df)){
    stop("volatility_m_df should be a data_frame.")
  }

  if(!all(c("id", "tickers", "dates") %in% colnames(volatility_m_df))){
    stop("volatility_m_df should have id, tickers and dates columns.")
  } else {}

  if(any(sapply(as.data.frame(volatility_m_df[,-c(1:3)]), function(x) any(is.na(as.numeric(as.character(x))))))
  ){
    stop("volatility_m_df should contain only numeric columns with non-NAs.")
  }

  suppressWarnings(
    if(any(!is.na(as.numeric(volatility_m_df$tickers)))){
      stop("tickers in volatility_m_df must be character.")
    })


  if(all(any(!lubridate::is.Date(volatility_m_df$dates)) ||
         any(is.na(as.Date(volatility_m_df$dates, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
    stop("dates in volatility_m_df must be a date object with format %Y-%m-%d.")
  }

  #Check structure of dates_m_vector and volatility_m_df$dates
  if(!all(as.character(dates_m_vector) %in% unique(as.character(volatility_m_df$dates))) ||
     !all(unique(as.character(volatility_m_df$dates)) %in% as.character(dates_m_vector))){
    stop("all dates in dates_m_vector must have a correspondence in volatility_m_df")
  } else {}


  if(any(apply(as.data.frame(volatility_m_df[,-c(1:3)]), 2, function(x) all(x >= -1 & x <= 1)))){
    stop("values in volatility_m_df should not be normalized")
  }

  ###################

  #######stock_groups_m_df
  ###################

  #Check for correct format in stock_groups_m_df
  if(!is.data.frame(stock_groups_m_df)){
    stop("stock_groups_m_df should be a data_frame.")
  }

  if(!all(c("id", "tickers", "dates") %in% colnames(stock_groups_m_df))){
    stop("stock_groups_m_df should have id, tickers and dates columns.")
  } else {}

  if(!all(apply(as.data.frame(stock_groups_m_df[,-c(1:3)]), 2, function(x) is.character(x)))){
    stop("stock_groups_m_df should contain only character columns.")
  }

  suppressWarnings(
    if(any(!is.na(as.numeric(stock_groups_m_df$tickers)))){
      stop("tickers in stock_groups_m_df must be character.")
    })


  if(all(any(!lubridate::is.Date(stock_groups_m_df$dates)) ||
         any(is.na(as.Date(stock_groups_m_df$dates, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
    stop("dates in stock_groups_m_df must be a date object with format %Y-%m-%d.")
  }

  #Check structure of dates_m_vector and stock_groups_m_df$dates
  if(!all(as.character(dates_m_vector) %in% unique(as.character(stock_groups_m_df$dates))) ||
     !all(unique(as.character(stock_groups_m_df$dates)) %in% as.character(dates_m_vector))){
    stop("all dates in dates_m_vector must have a correspondence in stock_groups_m_df")
  } else {}



  ###################

  #######signal_groups_m_df
  ###################

  #Check for correct format in signal_groups_m_df
  if(!is.data.frame(signal_groups_m_df)){
    stop("signal_groups_m_df should be a data_frame.")
  }

  if(!all(c("id", "tickers", "dates") %in% colnames(signal_groups_m_df))){
    stop("signal_groups_m_df should have id, tickers and dates columns.")
  } else {}

  if(!all(apply(as.data.frame(signal_groups_m_df[,-c(1:3)]), 2, function(x) is.character(x)))){
    stop("signal_groups_m_df should contain only character columns.")
  }

  suppressWarnings(
    if(any(!is.na(as.numeric(signal_groups_m_df$tickers)))){
      stop("tickers in signal_groups_m_df must be character.")
    })


  if(all(any(!lubridate::is.Date(signal_groups_m_df$dates)) ||
         any(is.na(as.Date(signal_groups_m_df$dates, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
    stop("dates in signal_groups_m_df must be a date object with format %Y-%m-%d.")
  }

  #Check structure of dates_m_vector and signal_groups_m_df$dates
  if(!all(as.character(dates_m_vector) %in% unique(as.character(signal_groups_m_df$dates))) ||
     !all(unique(as.character(signal_groups_m_df$dates)) %in% as.character(dates_m_vector))){
    stop("all dates in dates_m_vector must have a correspondence in signal_groups_m_df")
  } else {}

  #check for theme
  if(!all(colnames(signal_groups_m_df) == c("id", "tickers", "dates", "theme"))){
    stop("columns in signals_groups_m_df should be id, tickers, dates and theme")
  }




  ###################

  #######benchmark_weights_m_df
  ###################

  #Check for correct format in benchmark_weights_m_df
  if(!is.data.frame(benchmark_weights_m_df)){
    stop("benchmark_weights_m_df should be a data_frame.")
  }

  if(!all(c("id", "tickers", "dates") %in% colnames(benchmark_weights_m_df))){
    stop("benchmark_weights_m_df should have id, tickers and dates columns.")
  } else {}

  if(any(sapply(as.data.frame(benchmark_weights_m_df[,-c(1:3)]), function(x) any(is.na(as.numeric(as.character(x))))))
  ){
    stop("benchmark_weights_m_df should contain only numeric columns with non-NAs.")
  }

  suppressWarnings(
    if(any(!is.na(as.numeric(benchmark_weights_m_df$tickers)))){
      stop("tickers in benchmark_weights_m_df must be character.")
    })


  if(all(any(!lubridate::is.Date(benchmark_weights_m_df$dates)) ||
         any(is.na(as.Date(benchmark_weights_m_df$dates, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
    stop("dates in benchmark_weights_m_df must be a date object with format %Y-%m-%d.")
  }

  #Check structure of dates_m_vector and benchmark_weights_m_df$dates
  if(!all(as.character(dates_m_vector) %in% unique(as.character(benchmark_weights_m_df$dates))) ||
     !all(unique(as.character(benchmark_weights_m_df$dates)) %in% as.character(dates_m_vector))){
    stop("all dates in dates_m_vector must have a correspondence in benchmark_weights_m_df")
  } else {}


  if(any(apply(as.data.frame(benchmark_weights_m_df[,-c(1:3)]), 2, function(x) all(x >= 0 & x <= 1)))){
    stop("values in benchmark_weights_m_df should be between 0 and 1")
  }

  #Get sum of benchmark weights by date
  benchmark_weights_sum <- benchmark_weights_m_df %>%
    dplyr::group_by(dates) %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(., na.rm = TRUE), .names = "sum_{col}"))

  if(!all(apply(as.data.frame(benchmark_weights_sum[,-1]), 2, function(x) sum(x) != 1))){
    stop("weights in benchmark_weights_m_df should sum to 1 in every date.")
  }

  ###################


  #######fwd_returns_m_df
  #####################
  #Check for correct format in fwd_returns_m_df
  if(!is.data.frame(fwd_returns_m_df)){
    stop("fwd_returns_m_df should be a data_frame.")
  }

  if(!all(c("id", "tickers", "dates") %in% colnames(fwd_returns_m_df))){
    stop("fwd_returns_m_df should have id, tickers and dates columns.")
  } else {}


  suppressWarnings(
    if(any(!is.na(as.numeric(fwd_returns_m_df$tickers)))){
      stop("tickers in fwd_returns_m_df must be character.")
    })

  if(all(any(!lubridate::is.Date(fwd_returns_m_df$dates)) ||
         any(is.na(as.Date(fwd_returns_m_df$dates, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
    stop("dates in fwd_returns_m_df must be a date object with format %Y-%m-%d.")
  }

  #Check structure of dates_m_vector and fwd_returns_m_df$dates
  if(!all(as.character(dates_m_vector) %in% unique(as.character(fwd_returns_m_df$dates))) ||
     !all(unique(as.character(fwd_returns_m_df$dates)) %in% as.character(dates_m_vector))){
    stop("all dates in dates_m_vector must have a correspondence in fwd_returns_m_df")
  } else {}

  dates_allowed_to_be_NA_in_fwd_returns_m_df <- unique(fwd_returns_m_df$dates)[(length(unique(fwd_returns_m_df$dates)) - 1 + 1):length(unique(fwd_returns_m_df$dates))]
  if(length(dates_allowed_to_be_NA_in_fwd_returns_m_df) > 1){
    stop("number of dates in fwd_returns_m_df with NAs should be at most equal to 1")
  }

  if(any(is.na(fwd_returns_m_df[-which(fwd_returns_m_df$dates %in% dates_allowed_to_be_NA_in_fwd_returns_m_df),"fwd_return_1m"]))){
    stop("fwd_returns_m_df before last period should contain only numeric columns with non-NAs.")
  }

  if(any(apply(as.data.frame(fwd_returns_m_df[,-c(1:3)]), 2, function(x) all(x >= -1 & x <= 1)))){
    stop("values in fwd_returns_m_df should not be normalized")
  }

  if(is.null(fwd_returns_m_df$fwd_return_1m)){
    stop("fwd_returns_m_df should contain a column named fwd_return_1m")
  }

  #####################

  ####target_m_df
  #######################
  if(signal_selection_policy$signal_blending_method == "ML"){
  #Check for correct format in target_m_df
  if(!(is.data.frame(target_m_df))){
    stop("target_m_df should be a data_frame.")
  }

  if(!all(c("id", "tickers", "dates") %in% colnames(target_m_df))){
    stop("target_m_df should have id, tickers and dates columns.")
  } else {}

  suppressWarnings(if(any(!is.na(as.numeric(target_m_df$tickers)))){
    stop("tickers in target_m_df must be character.")
  })

  if(all(any(!lubridate::is.Date(target_m_df$dates)) ||
         any(is.na(as.Date(target_m_df$dates, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
    stop("dates in target_m_df must be a date object with format %Y-%m-%d.")
  }

  dates_allowed_to_be_NA_in_target_m_df <- unique(target_m_df$dates)[(length(unique(target_m_df$dates)) - signal_selection_policy$ml_parameters$target_fwd + 1):length(unique(target_m_df$dates))]
  if(length(dates_allowed_to_be_NA_in_target_m_df) > signal_selection_policy$ml_parameters$target_fwd){
    stop("number of dates in target_m_df with NAs should be at most equal to target_fwd")
  }

  if(any(is.na(target_m_df[-which(target_m_df$dates %in% dates_allowed_to_be_NA_in_target_m_df),target_fwd_name]))){
    stop("target_m_df before target_fwd periods should contain only numeric columns with non-NAs.")
  }

  #Check structure of dates_m_vector and target_m_df$dates
  if(!all(as.character(dates_m_vector) %in% unique(as.character(target_m_df$dates))) ||
     !all(unique(as.character(target_m_df$dates)) %in% as.character(dates_m_vector))){
    stop("all dates in dates_m_vector must have a correspondence in target_m_df")
  } else {}

  }

  #######################


  #######dates_m_vector
  #####################

  #Check for correct format in dates_m_vector
  if(!inherits(dates_m_vector, "Date")){
    stop("dates_m_vector must be a date object with format %Y-%m-%d")
  }

  #Check for correct format in dates_m_vector
  if(any(!lubridate::is.Date(dates_m_vector)) ||
     any(is.na(as.Date(dates_m_vector, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d"))))){
    stop("dates_m_vector must be a date object with format %Y-%m-%d")
  } else {}

  #Check structure of dates_m_vector
  if(!all(dates_m_vector == dates_m_vector[order(dates_m_vector)])){
    stop("dates_m_vector should be in ascending chronological order")
  } else {}

  #Check structure of dates_m_vector and signals_m_df$dates
  if(!all(as.character(dates_m_vector) %in% unique(as.character(signals_m_df$dates))) ||
     !all(unique(as.character(signals_m_df$dates)) %in% as.character(dates_m_vector))){
    stop("all dates in dates_m_vector must have a correspondence in signals_m_df")
  } else {}

  if(any(as.Date(dates_m_vector, format = "%Y-%m-%d") != as.Date(unique(signals_m_df$dates), format = "%Y-%m-%d"))){
    stop("dates_m_vector and signals_m_df$dates should have same order")
  }

  #####################

  ##Cross-checks
  ##################

  #Check structure between liquidity_m_df and signals_m_df
  if(nrow(liquidity_m_df) != nrow(signals_m_df)){
    stop("signals_m_df and liquidity_m_df must possess same number of rows.")
  }

  if(any(liquidity_m_df$id != signals_m_df$id)){
    stop("id in signals_m_df and in liquidity_m_df must match.")
  }

  if(any(liquidity_m_df$tickers != signals_m_df$tickers)){
    stop("tickers in signals_m_df and in liquidity_m_df must match.")
  }

  if(any(liquidity_m_df$dates != signals_m_df$dates)){
    stop("dates in signals_m_df and in liquidity_m_df must match.")
  }


  #Check structure between volatility_m_df and signals_m_df
  if(nrow(volatility_m_df) != nrow(signals_m_df)){
    stop("signals_m_df and volatility_m_df must possess same number of rows.")
  }

  if(any(volatility_m_df$id != signals_m_df$id)){
    stop("id in signals_m_df and in volatility_m_df must match.")
  }

  if(any(volatility_m_df$tickers != signals_m_df$tickers)){
    stop("tickers in signals_m_df and in volatility_m_df must match.")
  }

  if(any(volatility_m_df$dates != signals_m_df$dates)){
    stop("dates in signals_m_df and in volatility_m_df must match.")
  }


  #Check structure between benchmark_weights_m_df and signals_m_df
  if(nrow(benchmark_weights_m_df) != nrow(signals_m_df)){
    stop("signals_m_df and benchmark_weights_m_df must possess same number of rows.")
  }

  if(any(benchmark_weights_m_df$id != signals_m_df$id)){
    stop("id in signals_m_df and in benchmark_weights_m_df must match.")
  }

  if(any(benchmark_weights_m_df$tickers != signals_m_df$tickers)){
    stop("tickers in signals_m_df and in benchmark_weights_m_df must match.")
  }

  if(any(benchmark_weights_m_df$dates != signals_m_df$dates)){
    stop("dates in signals_m_df and in benchmark_weights_m_df must match.")
  }


  #Check structure between stock_groups_m_df and signals_m_df
  if(nrow(stock_groups_m_df) != nrow(signals_m_df)){
    stop("signals_m_df and stock_groups_m_df must possess same number of rows.")
  }

  if(any(stock_groups_m_df$id != signals_m_df$id)){
    stop("id in signals_m_df and in stock_groups_m_df must match.")
  }

  if(any(stock_groups_m_df$tickers != signals_m_df$tickers)){
    stop("tickers in signals_m_df and in stock_groups_m_df must match.")
  }

  if(any(stock_groups_m_df$dates != signals_m_df$dates)){
    stop("dates in signals_m_df and in stock_groups_m_df must match.")
  }

  #Check structure between target_m_df and signals_m_df
  if(nrow(target_m_df) != nrow(signals_m_df)){
    stop("signals_m_df and target_m_df must possess same number of rows.")
  }

  if(any(target_m_df$id != signals_m_df$id)){
    stop("id in signals_m_df and in target_m_df must match.")
  }

  if(any(target_m_df$tickers != signals_m_df$tickers)){
    stop("tickers in signals_m_df and in target_m_df must match.")
  }

  if(any(target_m_df$dates != signals_m_df$dates)){
    stop("dates in signals_m_df and in target_m_df must match.")
  }


  #Check structure between fwd_returns_m_df and signals_m_df
  if(nrow(fwd_returns_m_df) != nrow(signals_m_df)){
    stop("signals_m_df and fwd_returns_m_df must possess same number of rows.")
  }

  if(any(fwd_returns_m_df$id != signals_m_df$id)){
    stop("id in signals_m_df and in fwd_returns_m_df must match.")
  }

  if(any(fwd_returns_m_df$tickers != signals_m_df$tickers)){
    stop("tickers in signals_m_df and in fwd_returns_m_df must match.")
  }

  if(any(fwd_returns_m_df$dates != signals_m_df$dates)){
    stop("dates in signals_m_df and in fwd_returns_m_df must match.")
  }

  ##################

  #Concentration constraint policy
  ##############################
  if(!is.null(concentration_constraint_policy)){

      ##Check if benchmark_weights_m_d_ref are present if constraint is set
      if(is.null(benchmark_weights_m_d_ref)){
        stop("Error in concentration_constraint_policy: benchmark_weights_m_d_ref can't be missing if concentration_constraint_policy is set")
      }

      ##Check if chosen benchmark is present in benchmark_weights_m_d_ref
      if(!concentration_constraint_policy$benchmark %in% colnames(benchmark_weights_m_df)){
        stop("Error in concentration_constraint_policy: chosen_benchmark is not present in benchmark_weights_m_df")
      }

      ##Check if max_abs_active_individual_weight is numeric
      if(!is.null(concentration_constraint_policy$max_abs_active_individual_weight) &
         !is.numeric(concentration_constraint_policy$max_abs_active_individual_weight)){
       stop("Error in concentration_constraint_policy: max_abs_active_individual_weight must be numeric")
      }

     ##Check if max_abs_active_group_weight is numeric
     if(!is.null(concentration_constraint_policy$max_abs_active_group_weight) &
        !is.numeric(concentration_constraint_policy$max_abs_active_group_weight)){
      stop("Error in concentration_constraint_policy: max_abs_active_group_weight must be numeric")
    }

     ##Check if stock_groups_m_df are present if group constraint is set
     if(!is.null(concentration_constraint_policy$max_abs_active_group_weight) && (is.null(stock_groups_m_df))){
        stop("Error in concentration_constraint_policy: stock_groups_m_df can't be missing if max_abs_active_group_weight of concentration_constraint_policy is set")
     }

     ##Check if groups in stock_groups_m_df match group constraints
     if(!is.null(concentration_constraint_policy$max_abs_active_group_weight) &&
        names(concentration_constraint_policy$max_abs_active_group_weight) != colnames(stock_groups_m_df[,-c(1:3)])){
        stop("Error in concentration_constraint_policy: names of group constraints must match groups in stock_groups_m_df")
     }

     ##Check if names in concentration_constraint_policy match possible options
     if(any(!names(concentration_constraint_policy) %in%
           c("benchmark", "max_abs_active_individual_weight", "max_abs_active_group_weight"))){
      stop("Error in concentration_constraint_policy: elements of concentration_constraint_policy should be one of benchmark, max_abs_active_individual_weight or max_abs_active_group_weight.")
     }
  }

  ##############################

  #liquidity_constraint_policy
  ##############################
  if(!is.null(liquidity_constraint_policy)){
    ##Check possibilities for liquidity_floor_rule
    if(!is.null(liquidity_constraint_policy$liquidity_floor_rule) &&
       !liquidity_constraint_policy$liquidity_floor_rule %in% c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")){
      stop("Error in liquidity_constraint_policy: liquidity_floor_rule not supported")
    }

    ##Check possibilies for liquidity_classification in liquidity_cap_rule
    if(any(unlist(sapply(liquidity_constraint_policy, function(x) {
      if(is.list(x)) {
        # Check if liquidity_classification is NOT in the specified categories
        !x$liquidity_classification %in% c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")
      }
    })))){
      stop("Error in liquidity_constraint_policy: liquidity_classification in liquidity_cap_rule not supported")
    }


    ##Check if liquidity caps are numeric
      if(any(unlist(sapply(liquidity_constraint_policy, function(x) {
        if(is.list(x)) {
          # Check if liquidity_classification is NOT in the specified categories
          !is.numeric(x$liquidity_cap)
        }
      })))){
        stop("Error in liquidity_constraint_policy: liquidity_cap is not numeric")
      }


  ##Check if elements of liquidity_constraint_policy are correct
    if(!any(substr(names(liquidity_constraint_policy), 1, nchar(names(liquidity_constraint_policy)) - 1) %in%
        c("liquidity_floor_rul", "liquidity_cap_rule_"))){
     stop("Error in liquidity_constraint_policy: elements of liquidity_constraint_policy should be one of liquidity_floor_rule or liquidity_cap_rule")
    }

  }

  ##########################

  #liquidity_floor_cutoffs_list
  ###############################
  if(!is.null(liquidity_floor_cutoffs_list)){
    #check if all needed classifications are present
    if(!all(names(liquidity_floor_cutoffs_list) == c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"))){
      stop("liquidity_floor_cutoffs_list must contain lower bound classifications for micro_caps, small_caps, mid_caps, large_caps and mega_caps")
    }

    #check if metrics are present in liquidity_m_df
    if(any(sapply(liquidity_floor_cutoffs_list, function(x){
        !all(names(x) %in% colnames(liquidity_m_df))
    }))){
      stop("liquidity_metrics of liquidity_floor_cutoffs_list must be present in liquidity_m_df.")
    }

  }

  ##############################

  #Check turnover constraint policy
  ##################################
    ##Check liquidity classification
    if(!is.null(turnover_constraint_policy)){
      if(any(sapply(turnover_constraint_policy, function(x){
        !x$liquidity_classification %in% c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")
      }))){
        stop("liquidity_classification in buffer_zone not supported")
      }
    }

    ##Check for presence of top_stock_quantile_buffer and liquidity_classification
    if(!is.null(turnover_constraint_policy)){
      if(!all(sapply(turnover_constraint_policy, function(x){
          all(c("liquidity_classification", "top_stock_quantile_buffer") %in% names(x))
      }))){
        stop("liquidity_classification and top_stock_quantile_buffer elements are mandatory if turnover_constraint_policy is set")
      }
    }

    ##Check if elements of turnover_constraint_policy are correct
    if(!is.null(turnover_constraint_policy)){
      if(!any(substr(names(turnover_constraint_policy), 1, nchar(names(turnover_constraint_policy)) - 1) %in%
              c("buffer_zone_"))){
        stop("elements of turnover_constraint_policy should be buffer_zone")
      }
    }

    ##Check if is possible to classify liquidity in case turnover_constraint_policy is set
    if(!is.null(turnover_constraint_policy)){
      if(is.null(liquidity_floor_cutoffs_list) || is.null(liquidity_m_df)){
        stop("liquidity_floor_cutoffs_list and liquidity_m_df are needed if turnover_constraint_policy is set")
      }
    }

  ##################################

  #signal_selection_policy
  ##################################
  #signal selection policy must always exist
  if(is.null(signal_selection_policy)){
    stop("signal_selection_policy can be NULL")
  }
  #chosen_signals too
  if(is.null(signal_selection_policy$chosen_signals)){
    stop("Error in signal_selection_policy: chosen_signals can't be missing")
  }
  #signals_positions too
  if(is.null(signal_selection_policy$signal_positions)){
    stop("Error in signal_selection_policy: signal_positions can't be missing")
  }


  if(any(!names(signal_selection_policy) %in% c("signal_blending_method", "sb_benchmark_weighting",
                                                "max_abs_active_individual_weight", "max_abs_active_group_weight",
                                                "p_correction_method", "data_availability_cutoff", "chosen_signals",
                                                "signal_positions", "signal_significance_threshold", "chosen_informative_data",
                                                "chosen_sb_metric", "priors_type"))){

    stop("Error in signal_selection_policy: option not supported.")
  }

    #signal_blending_method
      ##check if null
      if(is.null(signal_selection_policy$signal_blending_method)){
        stop("Error in signal_selection_policy: signal_blending_method can't be missing")
      }

      ##check if among viable options
      if(!signal_selection_policy$signal_blending_method %in% c("EW", "SW", "RP", "MTO", "ML")){
       stop("Error in signal_selection_policy: signal_blending_method should be one of EW, SW, RP, MTO or ML")
      }

      ##check if backtest_return_df is provided in case signal_blending_method != EW
      if(signal_blending_method != "EW" & is.null(backtest_return_df)){
       stop("backtest_return_df can't be NULL if signal_blending_method is not EW")
      }

      ##check if chosen_signals are present in signals_m_df
      if(any(!signal_selection_policy$chosen_signals %in% colnames(signals_m_df))){
        stop("Error in signal_selection_policy: one of chosen_signals not found in signals_m_df")
      }

      ##check if signals_positions are present in signals_m_df
      if(any(!signal_selection_policy$chosen_signals %in% names(signal_selection_policy$signal_positions))){
        stop("Error in signal_selection_policy: All chosen_signals should have a signal position.")
      }

      ##check if sb_benchmark_weighting is provided in case restrictions on signal concentration are set
      if((!is.null(signal_selection_policy$max_abs_active_individual_weight) ||
         !is.null(signal_selection_policy$max_abs_active_group_weight)) &
         is.null(signal_selection_policy$sb_benchmark_weighting)
      ){
        stop("Error in signal_selection_policy: sb_benchmark_weighting can't be missing if signal concentration constraints are set.")
      }


    #backtest_returns_df
      #In case backtest_returns_df is not NULL, define_signal_elibility is triggered
      if(!is.null(backtest_returns_df)){
        ##check for dates
        if(all(any(!lubridate::is.Date(backtest_returns_df$dates)) ||
               any(is.na(as.Date(backtest_returns_df$dates, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
          stop("dates in backtest_returns_df must be a date object with format %Y-%m-%d.")
        }

        ##check if chosen_sb_metric is one of viable options
        if(is.null(signal_selection_policy$chosen_sb_metric)){
         stop("Error in signal_selection_policy: chosen_sb_metric can't be NULL is backtest_returns_df is set")
        }
        if(!signal_selection_policy$chosen_sb_metric %in% c("mean_active_return", "IR", "alpha", "AP", "beta", "treynor")){
         stop("Error in signal_selection_policy: chosen_sb_metric must be one of mean_active_return, IR, alpha, AP, beta or treynor")
        }
        ##check if dates_backtest are all contemplated in backtest_returns_df
        if(any(!dates_backtest %in% backtest_returns_df$dates)){
         stop("all backtest dates should be present in backtest_returns_df dates")
        }
        ##check if data_availability_cutoff is set
        if(!is.null(signal_selection_policy$data_avaialability_cutoff) || !is.numeric(signal_selection_policy$data_avaialability_cutoff)){
         stop("Error in signal_selection_policy: data_availability_cutoff must be numeric")
        }

       ##signal significance
         ###threshold
          if(is.null(signal_selection_policy$signal_significance_threshold) ||
            !is.numeric(signal_selection_policy$signal_significance_threshold)){
           stop("Error in signal_selection_policy: signal_significance_threshold should be numeric in case backtest_returns_df is provided. If one wants to,
                consider all signals, set signal_significance_threshold to 1")
         }
          ###p_correction_method
          if(is.null(signal_selection_policy$p_correction_method)){
            stop("p_correction_method can't be missing if backtest_returns_df is provided")
          }

          if(!signal_selection_policy$p_correction_method %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
                                                                 "fdr", "none", "bayesian")){
            stop("Error in signal_selection_policy: p_correction_method not supported.")
          }

          ##sb_benchmark_weighting
          if(!signal_selection_policy$sb_benchmark_weighting %in% c("individual", "theme")){
            stop("Error in signal_selection_policy: should be one of individual_sb or theme_sb")
          }
        }
  ##################################

  #User constraints
  ######################
  #user defined constraints
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
  ######################

  #Portfolio Construction Method
  ###################################
  if(is.null(portfolio_construction_method)){
    stop("portfolio_construction_method can't be missing")
  }

  if(!portfolio_construction_method %in% c("EW", "SW", "CW", "CS", "RP", "MTO")){
    stop("portfolio_construction_method should be one of EW, SW, CW, CS, RP or MTO")
  }

  #RP or MTO
  if(portfolio_construction_method %in% c("RP", "MTO")){
    #Covariance estimation
    if(is.null(daily_active_returns_df)){
      stop("daily_active_returns_df can't be missing if portfolio_construction_method is RP or MTO")
    }

    if(if(any(!dates_backtest %in% daily_active_returns_df$dates)){
        stop("all backtest dates should be present in daily_active_returns_df dates")
    })

    ##check for dates
    if(all(any(!lubridate::is.Date(daily_active_returns_df$dates)) ||
           any(is.na(as.Date(daily_active_returns_df$dates, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
      stop("dates in backtest_returns_df must be a date object with format %Y-%m-%d.")
    }

    if(is.null(covariance_estimation_method)){
      stop("covariance_estimation_method can't be missing if portfolio_construction_method is RP or MTO")
    }
    if(is.null(covariance_matrix_sample_size)){
      stop("covariance_matrix_sample_size can't be missing if portfolio_construction_method is RP or MTO")
    }
  }

  ###################################



  #Check structure of rebalancing_months
  if(!is.numeric(rebalancing_months)){
    stop("rebalancing_months should be numeric.")
  }


  #Check structure of training_sample_size and validation_sample_size
  if(!(is.numeric(training_sample_size))){
    stop("training_sample_size should be numeric.")
  }

  if(!(is.numeric(validation_sample_size))){
    stop("validation_sample_size should be numeric.")
  }



  }























