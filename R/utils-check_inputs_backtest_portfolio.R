check_inputs_metabacktest <- function(signals_m_df, liquidity_m_df, volatility_m_df, benchmark_weights_m_df,
                                      stock_groups_m_df, signal_groups_m_df, ){

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

  if(any(sapply(as.data.frame(fwd_returns_m_df[,-c(1:3)]), function(x) any(is.na(as.numeric(as.character(x))))))
  ){
    stop("fwd_returns_m_df should contain only numeric columns with non-NAs.")
  }

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


  if(any(apply(as.data.frame(fwd_returns_m_df[,-c(1:3)]), 2, function(x) all(x >= -1 & x <= 1)))){
    stop("values in fwd_returns_m_df should not be normalized")
  }

  if(is.null(fwd_returns_m_df$fwd_return_1m)){
    stop("fwd_returns_m_df should contain a column named fwd_return_1m")
  }

  #####################

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

  ####target_m_df
  #######################

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

  dates_allowed_to_be_NA_in_target_m_df <- unique(target_m_df$dates)[(length(unique(target_m_df$dates)) - target_fwd + 1):length(unique(target_m_df$dates))]
  if(length(dates_allowed_to_be_NA_in_target_m_df) > target_fwd){
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

  #######################



  ##Cross-checks
  ##################

  #Check structure between target_m_df and signals_m_df
  if(nrow(target_m_df) != nrow(signals_m_df)){
    stop("signals_m_df and target_m_df must possess same number of rows.")
  }

  #Check structure between liquidity_m_df and signals_m_df
  if(nrow(liquidity_m_df) != nrow(signals_m_df)){
    stop("signals_m_df and liquidity_m_df must possess same number of rows.")
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

  ##################

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
