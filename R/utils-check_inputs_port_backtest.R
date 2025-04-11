#' Check Inputs for Portfolio Backtest
#'
#' This function validates all input parameters required for running a portfolio backtest. It ensures that data frames are coercible to a meta_dataframe, that required columns are present and correctly formatted, and that the values fall within acceptable ranges. In addition, it performs cross-checks between inputs (e.g., matching dates, tickers, and IDs) and issues warnings for minor issues and errors for critical mismatches.
#'
#' @param signals_m_df A data frame containing signal data that must be coercible to a meta_dataframe. It must have at least the columns for IDs, tickers, and dates (columns 1:3) and subsequent columns should be numeric and free of NAs. Dates must be of class \code{Date} and follow the \%Y-\%m-\%d format.
#' @param oos_predictions_m_df (Optional) A data frame with out-of-sample predictions that must be coercible to a meta_dataframe. It is expected to contain the columns: \code{"id"}, \code{"tickers"}, \code{"dates"}, \code{"target"}, \code{"pred"}, and \code{"error"}. The IDs in this data frame must correspond to the IDs in \code{signals_m_df} after the initial buffer period.
#' @param chosen_score_metric_and_position (Optional) A single-element list specifying the chosen score metric and position. Its name(s) must not include the substring \code{"low_"} and the chosen metric must be present as a column name in \code{signals_m_df}.
#' @param rebalancing_months A numeric value indicating the rebalancing frequency in months. Must be numeric and between 1 and 12.
#' @param initial_buffer_period A numeric value specifying the number of initial periods to exclude from the backtest.
#' @param port_construction_method A character string indicating the portfolio construction method. Allowed values are \code{"ew"}, \code{"sw"}, \code{"cw"}, \code{"cs"}, \code{"rp"}, \code{"mvo"}, or \code{"custom_weights"}.
#' @param eligibility_quantile_range A numeric vector specifying the eligibility quantile range used in portfolio construction.
#' @param rp_method A parameter specifying the risk parity method to be applied (when applicable).
#' @param n_random_ports A numeric value indicating the number of random portfolios to generate.
#' @param random_ports_method A parameter specifying the method for generating random portfolios.
#' @param opt_objective A parameter defining the optimization objective.
#' @param opt_method A parameter specifying the optimization method.
#' @param cov_estimation_method A character string specifying the covariance estimation method. This parameter is required when \code{port_construction_method} is \code{"rp"} or \code{"mvo"}.
#' @param cov_matrix_sample_size A numeric value indicating the sample size used for covariance matrix estimation. This is required when \code{port_construction_method} is \code{"rp"} or \code{"mvo"}.
#' @param active_returns A logical value indicating whether active returns are used. If \code{TRUE}, \code{daily_bench_returns_m_xts} must be provided.
#' @param cov_matrix_benchmark A character string specifying the benchmark for covariance matrix estimation. The specified benchmark must be present as a column in \code{daily_bench_returns_m_xts}.
#' @param daily_stock_returns_m_xts An \code{xts} object containing daily stock returns. The dates (i.e., the index) must be of class \code{Date}, consecutive, and should cover all tickers present in \code{signals_m_df}.
#' @param daily_bench_returns_m_xts An \code{xts} object containing daily benchmark returns. The dates must match those in \code{daily_stock_returns_m_xts} and the data must not contain NA values.
#' @param benchmark_returns_m_xts An \code{xts} object with benchmark returns. Dates must be of class \code{Date} and arranged as sequential monthly dates without NAs.
#' @param liquidity_constraint_policy (Optional) An object representing the liquidity constraint policy. It is validated by \code{validate_liquidity_constraint_policy} and requires that the corresponding liquidity data is provided.
#' @param turnover_constraint_policy (Optional) An object representing the turnover constraint policy. It is validated by \code{validate_turnover_constraint_policy} and requires that liquidity information is available.
#' @param concentration_constraint_policy (Optional) An object representing the concentration constraint policy. It is validated by \code{validate_concentration_constraint_policy} and requires corresponding benchmark weights and stock group data.
#' @param liquidity_m_df A data frame containing liquidity information. It must be coercible to a meta_dataframe, with non-normalized numeric columns free of NAs, and must cover all stocks from \code{signals_m_df}.
#' @param liquidity_floor_cutoffs A data frame providing liquidity floor cutoff values. It is validated by \code{validate_liquidity_floor_cutoffs} and must include the \code{main_liquidity_metric}.
#' @param main_liquidity_metric A character string specifying the primary liquidity metric. It must be present in \code{liquidity_m_df} and include the substring \code{"mean_volfin"}.
#' @param stock_groups_m_df (Optional) A data frame containing stock group data, coercible to a meta_dataframe. All stocks in \code{signals_m_df} should be represented, and group columns must be of type character.
#' @param benchmark_weights_m_df (Optional) A data frame with benchmark weights. It must be coercible to a meta_dataframe, have numeric columns with values between 0 and 1 (summing to 1 per date), and cover all stocks in \code{signals_m_df}.
#' @param volatility_m_df A data frame containing volatility data, coercible to a meta_dataframe. Numeric columns must have no NAs, include a \code{"daily_vol"} column, and cover all stocks in \code{signals_m_df}.
#' @param fwd_return_m_df A data frame containing forward returns, coercible to a meta_dataframe. It must contain a column named \code{"fwd_return_1m"}; the data should be numeric (with NAs allowed only at the final dates) and must match the structure of \code{signals_m_df} (IDs, tickers, dates).
#' @param transaction_costs_parameters An object (or list) containing transaction cost parameters. It must have the names \code{"direct_transaction_cost"}, \code{"strategy_aum"}, \code{"alpha"}, and \code{"lambda"}, and is validated via \code{validate_transaction_cost_parameters}.
#' @param custom_stock_weights_m_df (Optional) A data frame containing custom stock universe weights that is coercible to a meta_dataframe.
#' @param custom_stock_metrics_m_df (Optional) A data frame containing custom stock metrics that is coercible to a meta_dataframe.
#' @param lower_quantile_winsorization A numeric value specifying the lower winsorization quantile.
#' @param upper_quantile_winsorization A numeric value specifying the upper winsorization quantile.
#' @return \code{NULL}. This function is used for its side effects; it stops execution if any input validation fails.
#'
#' @details
#' This function performs comprehensive validation of multiple inputs for a portfolio backtest. It checks:
#'
#' - That data frames (e.g., \code{signals_m_df}, \code{oos_predictions_m_df}, \code{liquidity_m_df}, etc.) are coercible to a \code{meta_dataframe} and meet required formats.
#' - That numeric columns do not contain NAs (except where allowed) and fall within expected ranges.
#' - That date columns are of class \code{Date} and follow daily or sequential monthly formats as required.
#' - That data frames are consistent across IDs, tickers, and dates.
#' - That all constraints (liquidity, turnover, concentration) and custom weights/metrics satisfy required conditions.
#' - That transaction cost parameters are valid, and that \code{strategy_aum} and \code{main_liquidity_metric} are in comparable units.
#' The function uses additional helper functions such as \code{is_coercible_to_meta_dataframe}, \code{validate_turnover_constraint_policy}, \code{validate_liquidity_constraint_policy}, \code{validate_liquidity_floor_cutoffs}, etc., to perform these checks.
#'
#' }
check_inputs_port_backtest <- function(
  # Base Objects
  signals_m_df, oos_predictions_m_df, chosen_score_metric_and_position,
  # Backtest Scheme
  rebalancing_months, initial_buffer_period,
  # Portfolio Construction Method
  port_construction_method, eligibility_quantile_range, min_eligible_assets_fallback ,selected_benchmark,
  # RP/MVO Parameters
  rp_method, n_random_ports, random_ports_method, opt_objective, opt_method,
  # Covariance Estimation
  cov_estimation_method, cov_matrix_sample_size, active_returns, cov_matrix_benchmark,
  daily_stock_returns_m_xts, daily_bench_returns_m_xts, benchmark_returns_m_xts,
  # Constraints
  liquidity_constraint_policy, turnover_constraint_policy, concentration_constraint_policy,
  # Liquidity Information (Constraints and Active Returns Calculation)
  liquidity_m_df, liquidity_floor_cutoffs, main_liquidity_metric,
  # Group and benchmark constraints (stock groups also used to fill covariance data)
  stock_groups_m_df, benchmark_weights_m_df,
  # Return calculation (needs also liquidity and vol for net returns)
  volatility_m_df, fwd_return_m_df, transaction_costs_parameters,
  # Custom Stock Weights and Metrics
  custom_stock_weights_m_df, custom_stock_metrics_m_df,
  #User Defined
  user_defined_OR_rules_m_df, user_defined_AND_rules_m_df,
  # Misc
  lower_quantile_winsorization, upper_quantile_winsorization,
  verbose
){
  #######signals_m_df
  ###################

  #Check for correct format in signals_m_df
  if(!(is_coercible_to_meta_dataframe(signals_m_df))){
    stop("signals_m_df should be coercible to meta_dataframe object")
  }

  if(!all(sapply(signals_m_df[,-c(1:3)], function(x) is.numeric(x) && all(!is.na(x))))){
    stop("signals_m_df should contain only numeric columns with non-NAs.")
  }

  #Check for presence of low
  if(any(grepl("low_", colnames(signals_m_df)))){
    stop("signals_m_df column names should not contain 'low_'.")
  }

  #######eligibility_quantile_range
  ##################
  #Check if length is different from 2
  if(length(eligibility_quantile_range) != 2){
    stop("eligibility_quantile_range should have length 2.")
  }

  #Check if it is positive between 0 and 1
  if(any(eligibility_quantile_range < 0) || any(eligibility_quantile_range > 1)){
    stop("eligibility_quantile_range should be between 0 and 1.")
  }

  #Check if elements are in incresing order
  if(any(diff(eligibility_quantile_range) < 0)){
    stop("eligibility_quantile_range should be in increasing order.")
  }

  #######min_eligible_assets_fallback
  ##################
  if (!is.null(min_eligible_assets_fallback)){
    #Check if it is positive and %% 1 == 0
    if(min_eligible_assets_fallback <= 0 || min_eligible_assets_fallback %% 1 != 0){
      stop("min_eligible_assets_fallback should be a positive integer.")
    }

    #Check if it is less than the number of assets
    mean_n_assets <- signals_m_df %>% dplyr::group_by(dates) %>% dplyr::summarize(n_assets = dplyr::n()) %>% dplyr::pull(n_assets) %>% mean()
    if(min_eligible_assets_fallback >= mean_n_assets){
      stop("min_eligible_assets_fallback should be less than the average number of assets.")
    }
  }

  #######exp_ret_score_metric
  ###################
  if (!is.null(chosen_score_metric_and_position)){

    ###Check if chosen_score_metric_and_position is a single element
    if(length(chosen_score_metric_and_position) > 1){
      stop("chosen_score_metric_and_position should be a single element.")
    }

    #Check for presence of low_
    if(any(grepl("low_", names(chosen_score_metric_and_position)))){
      stop("chosen_score_metric_and_position should not contain 'low_'.")
    }

    ###Check if exp_ret_score_metric is present in signals_m_df
    if(any(!names(chosen_score_metric_and_position) %in% colnames(signals_m_df))){
      stop("chosen score metric selection not avaiable in signals_m_df")
    }
  }

  #######dates objs
  ###################
  dates_m_vector <- signals_m_df %>% dplyr::pull(dates) %>% unique() %>% sort()

  #Check structure of rebalancing_months
  if(!is.numeric(rebalancing_months)){
    stop("rebalancing_months should be numeric.")
  }

  if(rebalancing_months < 0 || rebalancing_months > 12){
    stop("rebalancing_months should be between 1 and 12.")
  }

  #Check that initial_buffer_period is positive and higher than 1
  if(!is.numeric(initial_buffer_period)){
    stop("initial_buffer_period must be numeric")
  }
  if(initial_buffer_period <= 0){
    stop("initial_buffer_period must be higher than 0")
  }


  ######oos_predictions_m_df
  if (!is.null(oos_predictions_m_df)){

    #Check if chosen_score_metric_and_position is not NULL
    if (!is.null(chosen_score_metric_and_position) ||
        (!is.null(custom_stock_weights_m_df) && port_construction_method == "custom_weights")){
      stop("either chosen_score_metric_and_position, oos_predictions_m_df or custom_stock_weights_m_df should be provided.")
    }

    #Check for correct format in oos_predictions_m_df
    if (!is_coercible_to_meta_dataframe(oos_predictions_m_df)){
      stop("oos_predictions_m_df should be coercible to meta_dataframe object")
    }
    if (!all(colnames(oos_predictions_m_df) == c("id", "tickers", "dates", "pred"))){
      stop("oos_predictions_m_df should contain columns 'id', 'tickers', 'dates', 'pred'")
    }
    #Check if id's match after initial buffer period
    if (any(!(signals_m_df %>% dplyr::filter(dates >= dates_m_vector[initial_buffer_period]) %>% dplyr::pull(id)) %in%
            (oos_predictions_m_df %>% dplyr::pull(id)))){
      stop("all id's from signals_m_df after initial_buffer_period must have a correspondence in oos_predictions_m_df")
    }
  }

  #Check that at least one of chosen_score_metric_and_position or oos_predictions_m_df is provided
  if (is.null(chosen_score_metric_and_position) && is.null(oos_predictions_m_df)){
    stop("either chosen_score_metric_and_position or oos_predictions_m_df should be provided.")
  }

  #######daily stock returns
  ###################
  #daily_stock_returns_m_xts
  if(!is.null(daily_stock_returns_m_xts)){

    if(!xts::is.xts(daily_stock_returns_m_xts)){
      stop("daily_stock_returns_m_xts must be a xts object")
    }

    if(is.null(cov_matrix_sample_size)){
      stop("cov_matrix_sample_size must be provided together with daily_stock_returns_m_xts")
    }

    #get dates
    stock_returns_dates <- zoo::index(daily_stock_returns_m_xts)

    if(class(stock_returns_dates) != "Date"){
      stop("dates in daily_stock_returns_m_xts must be of class Date")
    }

    if(nrow(daily_stock_returns_m_xts) < cov_matrix_sample_size){
      stop("daily_stock_returns_m_xts must have at least cov_matrix_sample_size rows")
    }

    if(any(!unique(dplyr::pull(signals_m_df, tickers)) %in% colnames(daily_stock_returns_m_xts))){
      stop("all tickers derived from signals_m_df must be present in daily_stock_returns_m_xts")
    }

    #Check if all backtest dates have enough data for cov estimation
    for (i in dates_m_vector[initial_buffer_period:length(dates_m_vector)]){
      stock_returns_dates_before_i <- stock_returns_dates[which(stock_returns_dates <= i)]
      if (length(stock_returns_dates_before_i) < cov_matrix_sample_size) {
        stop("There is not enought cov_matrix_sample_size dates in daily_stock_returns_m_xts at backtesting date ", as.Date(i))
      }
    }

    #Check if there are missing or non-consecutive days or months
    ##Days
    year_month <- format(stock_returns_dates, "%Y-%m")
    day_count <- table(year_month)
    inner_dates_day_count <- day_count[-c(1, length(day_count))] #Exclude first and last month
    first_last_dates_day_count <- day_count[c(1, length(day_count))] #Last month

    ##Months
    ##Generate the full sequence of months from the first to the last date
    start_date <- as.Date(paste0(format(min(stock_returns_dates), "%Y-%m"), "-01"))
    end_date <- as.Date(paste0(format(max(stock_returns_dates), "%Y-%m"), "-01"))

    ##Create a sequence of all months between start and end date
    expected_months <- seq(from = start_date, to = end_date, by = "months")
    expected_year_months <- format(expected_months, "%Y-%m")

    ##Find missing months
    actual_year_months <- unique(year_month)
    missing_months <- setdiff(expected_year_months, actual_year_months)

    ##Check
    if(any(inner_dates_day_count <= 15) || any(first_last_dates_day_count >= 30) || ##Missing
       !all(diff(stock_returns_dates) > 0) || !all(diff(stock_returns_dates) < 15) ||
       length(missing_months) > 0 || ##Non-consecutive
       any(duplicated(stock_returns_dates))){
      stop("daily_stock_returns_m_xts structure is wrong. It should have unique consecutive days and there should not be less than 15 trading days in any month.")
    }

  }


  #######daily bench returns
  ###################
  #daily_bench_returns_m_xts
  if(!is.null(daily_bench_returns_m_xts)){

    if(is.null(daily_stock_returns_m_xts)){
      stop("daily_stock_returns_m_xts must be provided together with daily_bench_returns_m_xts")
    }

    if(!xts::is.xts(daily_bench_returns_m_xts)){
      stop("daily_bench_returns_m_xts must be a xts object")
    }
    #get dates
    bench_returns_dates <- zoo::index(daily_bench_returns_m_xts)

    if(!setequal(bench_returns_dates, stock_returns_dates)){
      stop("dates in daily_bench_returns_m_xts and daily_stock_returns_m_xts must be the same")
    }

    if(any(apply(daily_bench_returns_m_xts, 2, function(x) any(is.na(x))))){
      stop("daily_bench_returns_m_xts must not have any NA values")
    }

    if(!is.null(cov_matrix_benchmark) && !cov_matrix_benchmark %in% colnames(daily_bench_returns_m_xts)){
      stop("cov_matrix_benchmark must be present in daily_bench_returns_m_xts")
    }
  }

  #######benchmark_returns_m_xts
  ###################
  if (!is.null(benchmark_returns_m_xts)){
    if(!xts::is.xts(benchmark_returns_m_xts)){
      stop("benchmark_returns_m_xts must be a xts object")
    }
    #get dates
    benchmark_returns_dates <- zoo::index(benchmark_returns_m_xts)

    if(class(benchmark_returns_dates) != "Date"){
      stop("dates in benchmark_returns_m_xts must be of class Date")
    }

    if(!is.null(selected_benchmark) && !selected_benchmark %in% colnames(benchmark_returns_m_xts)){
      stop("selected_benchmark should be present in benchmark_returns_m_xts")
    }

    if(any(apply(benchmark_returns_m_xts, 2, function(x) any(is.na(x))))){
      stop("benchmark_returns_m_xts must not have any NA")
    }

    #Check if all dates in dates_m_vector are present
    if(!all(dates_m_vector %in% benchmark_returns_dates)){
      stop("all dates in signals_m_df must be present in benchmark_returns_m_xts")
    }

    benchmark_returns_dates_after_initial_buffer <- benchmark_returns_dates[which(benchmark_returns_dates > dates_m_vector[initial_buffer_period])]
    if (length(benchmark_returns_dates_after_initial_buffer) < 1) {
      stop("There must be at least one date in benchmark_returns_m_xts after initial_buffer_period")
    }

    if(!setequal(seq.Date(from = benchmark_returns_dates[1], to = benchmark_returns_dates[length(benchmark_returns_dates)], by = "month"), benchmark_returns_dates) ||
       length(unique(benchmark_returns_dates)) != length(benchmark_returns_dates)){
      stop("benchmark_returns_m_xts must have sequential unique monthly dates")
    }
  } else {
    if (!is.null(selected_benchmark)) {
      stop("benchmark_returns_m_xts must be provided when selected_benchmark is provided")
    }
  }

  ####stock_groups_m_df
  #####################
  if (!is.null(stock_groups_m_df)){
    #coercibility to meta dataframe
    if(!is_coercible_to_meta_dataframe(stock_groups_m_df)){
      stop("stock_groups_m_df must be coercible to a meta dataframe")
    }

    ##Check if group columns are character
    if (any(sapply(stock_groups_m_df[,-c(1:3)], function(x) !is.character(x)))){
      stop("all group columns in stock_groups_m_df must be character")
    }

    ##Check if all stocks of signals_m_df are covered in stock_groups_m_df
    if(any(!unique(signals_m_df$id) %in% (stock_groups_m_df %>% dplyr::pull(id)))){
      stop("all ids from signals_m_df should be present in stock_groups_m_df")
    }

    ###Check for NAs
    if (any(is.na(stock_groups_m_df))){
      stop("stock_groups_m_df should not have NAs")
    }

  }

  ###################

  #######liquidity_m_df
  ###################
  #Check for correct format in liquidity_m_df
  if(!is_coercible_to_meta_dataframe(liquidity_m_df)){
    stop("liquidity_m_df must be coercible to a meta dataframe")
  }

  if(!all(sapply(liquidity_m_df[,-c(1:3)], function(x) is.numeric(x) && all(!is.na(x))))){
    stop("liquidity_m_df should contain only numeric columns with non-NAs.")
  }

  ##Check if all stocks of signals_m_df are covered in liquidity_m_df
  if(any(!unique(signals_m_df$id) %in% (liquidity_m_df %>% dplyr::pull(id)))){
    stop("all ids from signals_m_df should be present in liquidity_m_df")
  }

  ##Check if main_liquidity_metric is covered and correct
  if(!stringr::str_detect(main_liquidity_metric, "mean_volfin")){
    stop("main_liquidity_metric must contain the string 'mean_volfin'")
  }
  if(!main_liquidity_metric %in% colnames(liquidity_m_df)){
    stop("main_liquidity_metric must be present in liquidity_m_df")
  }

  #Check normalization
  if(any(apply(as.data.frame(liquidity_m_df[,-c(1:3)]), 2, function(x) all(x >= -1 & x <= 1)))){
    stop("values in liquidity_m_df should not be normalized")
  }

  ###################

  #######volatility_m_df
  ###################
  #Check for correct format in volatility_m_df
  if(!is_coercible_to_meta_dataframe(volatility_m_df)){
    stop("volatility_m_df must be coercible to a meta dataframe")
  }

  if(!all(sapply(volatility_m_df[,-c(1:3)], function(x) is.numeric(x) && all(!is.na(x))))){
    stop("volatility_m_df should contain only numeric columns with non-NAs.")
  }

  ##Check if all stocks of signals_m_df are covered in volatility_m_df
  if(any(!dplyr::pull(signals_m_df, id) %in% (volatility_m_df %>% dplyr::pull(id)))){
    stop("all ids from signals_m_df should be present in volatility_m_df")
  }

  ##Check if main_liquidity_metric is covered
  if(!"daily_vol" %in% colnames(volatility_m_df)){
    stop("daily_vol must be present in volatility_m_df in order to calculate indirect costs")
  }

  #Check structure of dates_m_vector and volatility_m_df$dates
  if(!all(as.character(dates_m_vector) %in% unique(as.character(volatility_m_df$dates))) ||
     !all(unique(as.character(volatility_m_df$dates)) %in% as.character(dates_m_vector))){
    stop("all dates in dates_m_vector must have a correspondence in volatility_m_df")
  }
  #Check normalization
  if(any(apply(as.data.frame(volatility_m_df[,-c(1:3)]), 2, function(x) all(x >= -1 & x <= 1)))){
    stop("values in volatility_m_df should not be normalized")
  }

  ###################

  #######benchmark_weights_m_df
  ###################
  if (!is.null(benchmark_weights_m_df)){
    #Selected benchmark
    if (is.null(selected_benchmark)){
      stop("selected_benchmark must be provided when benchmark_weights_m_df is provided")
    }
    #Coercibility
    if(!is_coercible_to_meta_dataframe(benchmark_weights_m_df)){
      stop("benchmark_weights_m_df must be coercible to a meta dataframe")
    }
    #numeric non-na
    if(!all(sapply(benchmark_weights_m_df[,-c(1:3)], function(x) is.numeric(x) && all(!is.na(x))))){
      stop("benchmark_weights_m_df should contain only numeric columns with non-NAs.")
    }
    #Check if all stocks of signals_m_df are covered in benchmark_weights_m_df
    if(any(!unique(signals_m_df$id) %in% (benchmark_weights_m_df %>% dplyr::pull(id)))){
      stop("all ids from signals_m_df should be present in benchmark_weights_m_df")
    }
    #Check for selected_benchmark
    if(!selected_benchmark %in% colnames(benchmark_weights_m_df)){
      stop("selected_benchmark should be present in benchmark_weights_m_df")
    }

    #Check if w are right
    if(any(apply(as.data.frame(benchmark_weights_m_df[,-c(1:3)]), 2, function(x) !all(x >= 0 & x <= 1)))){
      stop("values in benchmark_weights_m_df should be between 0 and 1")
    }

    #Get sum of benchmark weights by date
    benchmark_weights_sum <- benchmark_weights_m_df %>%
      dplyr::group_by(dates) %>%
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(., na.rm = TRUE), .names = "sum_{col}"))

    if(any(apply(as.data.frame(benchmark_weights_sum[,-1]), 2, function(x) any(abs(x - 1) > 0.02)))){
      stop("weights in benchmark_weights_m_df should sum to 1 in every date.")
    }
  } else {
    if (!is.null(selected_benchmark)){
      stop("benchmark_weights_m_df must be provided when selected_benchmark is provided")
    }
  }


  ###################

  #######custom_stock_weights_m_df
  ###################
  if (!is.null(custom_stock_weights_m_df)){

    #Coercibility
    if(!is_coercible_to_meta_dataframe(custom_stock_weights_m_df)){
      stop("custom_stock_weights_m_df must be coercible to a meta dataframe")
    }
    #numeric non-na
    if(!all(sapply(custom_stock_weights_m_df[,-c(1:3)], function(x) is.numeric(x) && all(!is.na(x))))){
      stop("custom_stock_weights_m_df should contain only numeric columns with non-NAs.")
    }
    #Check if all stocks of signals_m_df are covered in custom_stock_weights_m_df
    if(any(!unique(signals_m_df$id) %in% (custom_stock_weights_m_df %>% dplyr::pull(id)))){
      stop("all ids from signals_m_df should be present in custom_stock_weights_m_df")
    }
    #Check for weight column
    if(colnames(custom_stock_weights_m_df)[4] != "weights"){
      stop("custom_stock_weights_m_df should have a column named weights")
    }

    #Check if w are right
    if(any(custom_stock_weights_m_df$weights < 0 | custom_stock_weights_m_df$weights > 1)){
      stop("weights in custom_stock_weights_m_df should be between 0 and 1")
    }

    #Get sum of stock weights by date
    stock_weights_sum <- custom_stock_weights_m_df %>%
      dplyr::group_by(dates) %>%
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(., na.rm = TRUE), .names = "sum_{col}"))

    if(any(apply(as.data.frame(stock_weights_sum[,-1]), 2, function(x) any(abs(x - 1) > 0.02)))){
      stop("weights in custom_stock_weights_m_df should sum to 1 in every date.")
    }
  }

  ###################

  #######fwd_return_m_df
  #####################

  #Check for correct format in fwd_return_m_df
  if(!(is_coercible_to_meta_dataframe(fwd_return_m_df))){
    stop("fwd_return_m_df should be coercible to meta_dataframe object")
  }

  #fwd_returm_1m presence
  if (!c("fwd_return_1m") %in% colnames(fwd_return_m_df)){
    stop("fwd_return_1m should be present in fwd_return_m_df")
  }

  fwd_dates <- unique(fwd_return_m_df %>% dplyr::pull(dates)) %>% sort()
  #Check for only NAs in first rebalancing period
  if (all(is.na(fwd_return_m_df %>% dplyr::filter(dates %in% fwd_dates[initial_buffer_period]) %>% dplyr::pull(fwd_return_1m)))){
    stop("fwd_return_m_df can't have NAs in the first backtesting period")
  }

  #Get dates allowed to be NA
  dates_allowed_to_be_NA_in_fwd_return_m_df <- fwd_dates[length(fwd_dates)]
  if(length(dates_allowed_to_be_NA_in_fwd_return_m_df) > 1){
    stop("number of dates in fwd_return_m_df with NAs should be at most equal to 1")
  }
  if(any(is.na(fwd_return_m_df[-which(fwd_return_m_df %>% dplyr::pull(dates) %in% dates_allowed_to_be_NA_in_fwd_return_m_df), "fwd_return_1m"]))){
    stop("fwd_return_m_df before last period should contain only numeric columns with non-NAs.")
  }

  #Get dates with effective NAs
  dates_allowed_to_be_NA_but_are_not_na <- fwd_return_m_df %>%
    dplyr::filter(dates %in% dates_allowed_to_be_NA_in_fwd_return_m_df, !is.na(fwd_return_1m)) %>%
    dplyr::pull(dates) %>%
    unique()

  dates_allowed_to_be_NA_and_really_are_na <- as.Date(setdiff(dates_allowed_to_be_NA_in_fwd_return_m_df, dates_allowed_to_be_NA_but_are_not_na))

  if(all(length(dates_allowed_to_be_NA_and_really_are_na) != 0, verbose)){
    message("The following dates from signals_m_df contemplate NA rows in fwd_return_m_df: ", paste(dates_allowed_to_be_NA_and_really_are_na, collapse = " "))
  }
  if(all(length(dates_allowed_to_be_NA_but_are_not_na) != 0, verbose)){
    message("The following final dates from fwd_return_m_df are expected to be NA in an up-to-date backtest, but are not: ", paste(dates_allowed_to_be_NA_but_are_not_na, collapse = " "))
  }

  #Check structure between fwd_return_m_df and signals_m_df
  if(nrow(fwd_return_m_df) != nrow(signals_m_df)){
    stop("signals_m_df and fwd_return_m_df must possess same number of rows.")
  }

  if(any(fwd_return_m_df %>% dplyr::pull(id) != signals_m_df %>% dplyr::pull(id))){
    stop("id in signals_m_df and in fwd_return_m_df must match.")
  }

  if(any(fwd_return_m_df %>% dplyr::pull(tickers) != signals_m_df %>% dplyr::pull(tickers))){
    stop("tickers in signals_m_df and in fwd_return_m_df must match.")
  }

  if(any(fwd_return_m_df %>% dplyr::pull(dates) != signals_m_df %>% dplyr::pull(dates))){
    stop("dates in signals_m_df and in fwd_return_m_df must match.")
  }

  if(length(fwd_return_m_df %>% dplyr::pull(dates) %>% unique()) <= 1 || length(signals_m_df %>% dplyr::pull(dates) %>% unique()) <= 1){
    stop("fwd_return_m_df and signals_m_df should have more than one date")
  }

  #Check if last date (without NAs) of fwd_return_m_df is covered by benchmark_returns_m_xts
  if (!is.null(benchmark_returns_m_xts) && !is.null(selected_benchmark)){
    last_fwd_date_non_NA <- fwd_return_m_df %>% dplyr::select(dates, fwd_return_1m) %>% tidyr::drop_na() %>% dplyr::pull(dates) %>% unique() %>% dplyr::last()
    if (!lubridate::add_with_rollback(last_fwd_date_non_NA, months(1)) %in% zoo::index(benchmark_returns_m_xts)){
      stop("last date of fwd_return_m_df should be covered by benchmark_returns_m_xts")
    }
  }

  #Normalization
  if (all(fwd_return_m_df %>% dplyr::pull(fwd_return_1m) >= -1 & fwd_return_m_df %>% dplyr::pull(fwd_return_1m) <= 1, na.rm = TRUE)){
    stop("values in fwd_return_m_df should not be normalized")
  }


  #####################

  #######custom_stock_metrics_m_df
  #####################
  if (!is.null(custom_stock_metrics_m_df)){

    ###Coercible
    if (!(is_coercible_to_meta_dataframe(custom_stock_metrics_m_df))){
      stop("custom_stock_metrics_m_df should be coercible to meta_dataframe object")
    }

    ###Check structure between custom_stock_metrics_m_df and signals_m_df
    if (any(!(signals_m_df %>% dplyr::filter(dates >= dates_m_vector[initial_buffer_period]) %>% dplyr::pull(id)) %in%
            (custom_stock_metrics_m_df %>% dplyr::pull(id)))){
      stop("all id's from signals_m_df after initial_buffer_period must have a correspondence in custom_stock_metrics_m_df")
    }

    ###Check for NAs
    if (any(is.na(custom_stock_metrics_m_df))){
      stop("custom_stock_metrics_m_df should not have NAs")
    }
  }

  #######################

  #Concentration constraint policy
  ##############################
  if(!is.null(concentration_constraint_policy)){

    ##Check validity
    validate_concentration_constraint_policy(concentration_constraint_policy)

    ##Check if benchmarks match
    if (concentration_constraint_policy$benchmark != selected_benchmark){
      stop("Error in concentration_constraint_policy: benchmark must match selected_benchmark")
    }

    ##Check if benchmark_weights_m_df are present if constraint is set
    if(is.null(benchmark_weights_m_df)){
      stop("Error in concentration_constraint_policy: benchmark_weights_m_df can't be missing if concentration_constraint_policy is set")
    }

    ##Check if chosen benchmark is present in benchmark_weights_m_df
    if(!concentration_constraint_policy$benchmark %in% colnames(benchmark_weights_m_df)){
      stop("Error in concentration_constraint_policy: chosen_benchmark is not present in benchmark_weights_m_df")
    }

    ##Check if stock_groups_m_df are present if group constraint is set
    if(!is.null(concentration_constraint_policy$max_abs_active_group_weight) && (is.null(stock_groups_m_df))){
      stop("Error in concentration_constraint_policy: stock_groups_m_df can't be missing if max_abs_active_group_weight of concentration_constraint_policy is set")
    }

    ##Check if groups in stock_groups_m_df match group constraints
    if(!is.null(concentration_constraint_policy$max_abs_active_group_weight) &&
       !all(names(concentration_constraint_policy$max_abs_active_group_weight) == colnames(stock_groups_m_df[,-c(1:3)]))){
      stop("Error in concentration_constraint_policy: names of group constraints must match groups in stock_groups_m_df")
    }

    #Check accordance with port_construction_method
    if (!is.null(concentration_constraint_policy) && port_construction_method != "mvo"){
      message("concentration_constraint_policy is only available for port_construction_method = 'mvo'. Ignoring concentration_constraint_policy")
    }
  }

  ##############################

  #liquidity_constraint_policy
  ##############################
  if(!is.null(liquidity_constraint_policy)){

    ##Check validity
    validate_liquidity_constraint_policy(liquidity_constraint_policy)

    ##Check if liquidity_m_df are present if constraint is set
    if(is.null(liquidity_m_df)){
      stop("Error in liquidity_constraint_policy: liquidity_m_df can't be missing if liquidity_constraint_policy is set")
    }

    ##Check if liquidity_floor_cutoffs is null
    if(is.null(liquidity_floor_cutoffs)){
      stop("Error in liquidity_constraint_policy: liquidity_floor_cutoffs can't be missing if liquidity_constraint_policy is set")
    }

    ##Check if its present in liquidity_floor_cutoffs
    if (!is.null(liquidity_constraint_policy$liquidity_floor_rule) &&
        !liquidity_constraint_policy$liquidity_floor_rule %in% dplyr::pull(liquidity_floor_cutoffs, liquidity_classification)){
      stop("Error in liquidity_constraint_policy: liquidity_floor_rule not present in liquidity_floor_cutoffs")
    }

    #Check accordance with port_construction_method
    if (!is.null(liquidity_constraint_policy$liquidity_cap_rules) && port_construction_method != "mvo"){
      message("liquidity_cap_rules are only available for port_construction_method = 'mvo'. Ignoring liquidity_cap_rules")
    }

  }

  ##########################

  #liquidity_floor_cutoffs
  ###############################
  if(!is.null(liquidity_floor_cutoffs)){

    #Check if all liquidity_floor_cutoffs are contemplated
    if (!all(colnames(liquidity_floor_cutoffs)[-1] %in% colnames(liquidity_m_df))){
      stop("all liquidity_floor_cutoffs must be present in liquidity_m_df")
    }

    validate_liquidity_floor_cutoffs(liquidity_floor_cutoffs, main_liquidity_metric)

  }

  ##############################

  #Check turnover constraint policy
  ##################################
  ##Check turnover constraint
  if(!is.null(turnover_constraint_policy)){

    ###Check validity
    validate_turnover_constraint_policy(turnover_constraint_policy)

    ##Check if is possible to classify liquidity in case turnover_constraint_policy is set
    if(is.null(liquidity_floor_cutoffs) || is.null(liquidity_m_df)){
      stop("liquidity_floor_cutoffs and liquidity_m_df are needed if turnover_constraint_policy is set")
    }

    #Check accordance with port_construction_method
    if (!is.null(turnover_constraint_policy) && port_construction_method != "mvo"){
      message("turnover_constraint_policy is only available for port_construction_method = 'mvo'. Ignoring turnover_constraint_policy")
    }
  }

  ##################################

  #User constraints
  ######################
  #user defined constraints
  ##Check user_defined_OR_rules_m_df
  if(!is.null(user_defined_OR_rules_m_df)){

    ##Coercibilit
    if(!(is_coercible_to_meta_dataframe(user_defined_OR_rules_m_df))){
      stop("user_defined_OR_rules_m_df should be coercible to meta_dataframe object")
    }

    ##Check that there are 5 columns
    if(ncol(user_defined_OR_rules_m_df) != 5){
      stop("user_defined_OR_rules_m_df should have 5 columns")
    }

    #Check that fifth column is binary (0 or 1)
    if(!all(user_defined_OR_rules_m_df[[5]] %in% c(0,1))){
      stop("fifth column of user_defined_OR_rules_m_df should be 0 or 1")
    }

    #Check that it contemplates all signals_m_df id's after initial_buffer_period
    if(!all((signals_m_df %>% dplyr::filter(dates >= dates_m_vector[initial_buffer_period]) %>% dplyr::pull(id)) %in%
            user_defined_OR_rules_m_df$id)){
      stop("user_defined_OR_rules_m_df should contemplate all signals_m_df id's after initial_buffer_period")
    }

  }

  ##Check user_defined_AND_rules_m_df
  if(!is.null(user_defined_AND_rules_m_df)){

    ##Coercibilit
    if(!(is_coercible_to_meta_dataframe(user_defined_AND_rules_m_df))){
      stop("user_defined_AND_rules_m_df should be coercible to meta_dataframe object")
    }

    ##Check that there are 5 columns
    if(ncol(user_defined_AND_rules_m_df) != 5){
      stop("user_defined_AND_rules_m_df should have 5 columns")
    }

    #Check that fifth column is binary (0 or 1)
    if(!all(user_defined_AND_rules_m_df[[5]] %in% c(0,1))){
      stop("fifth column of user_defined_AND_rules_m_df should be 0 or 1")
    }

    #Check that it contemplates all signals_m_df id's after initial_buffer_period
    if(!all((signals_m_df %>% dplyr::filter(dates >= dates_m_vector[initial_buffer_period]) %>% dplyr::pull(id)) %in%
            user_defined_AND_rules_m_df$id)){
      stop("user_defined_AND_rules_m_df should contemplate all signals_m_df id's after initial_buffer_period")
    }

  }


  ######################

  #Portfolio Construction Method
  ###################################
  if(is.null(port_construction_method)){
    stop("port_construction_method can't be missing")
  }

  if(!port_construction_method %in% c("ew", "sw", "cw", "cs", "rp", "mvo", "custom_weights")){
    stop("port_construction_method must be one of 'ew', 'sw', 'cw', 'cs', 'rp', 'mvo' or 'custom_weights'")
  }

  #RP or MVO
  if(port_construction_method %in% c("rp", "mvo")){

    if(is.null(cov_estimation_method)){
      stop("cov_estimation_method can't be missing if port_construction_method is 'rp' or 'mvo'")
    }
    if(is.null(cov_matrix_sample_size)){
      stop("cov_matrix_sample_size can't be missing if port_construction_method is 'rp' or 'mvo'")
    }
    if(is.null(daily_stock_returns_m_xts)){
      stop("daily_stock_returns_m_xts can't be missing if port_construction_method is 'rp' or 'mvo'")
    }
    if(active_returns && is.null(daily_bench_returns_m_xts)){
      stop("daily_bench_returns_m_xts can't be NULL if active_returns is TRUE")
    }

  }

  #Custom Weights
  if(port_construction_method == "custom_weights" && is.null(custom_stock_weights_m_df)){
    stop("custom_stock_weights_m_df must be provided when port_construction_method is 'custom_weights'")
  }

  ###################################

  #Transaction_cost
  ##########################
  if (length(transaction_costs_parameters) == 0){
    stop("transaction_costs_parameters can't be missing")
  }

  #Validate
  validate_transaction_costs_parameters(transaction_costs_parameters)

  #Check if strategy_aum and main_liquidity_metric can be expected to be in same units
  median_main_liq_metric <- median(liquidity_m_df[[main_liquidity_metric]])
  median_order_size_est <- ((transaction_costs_parameters$strategy_aum)/100)/median_main_liq_metric

  if (median_order_size_est > 0.25 || median_order_size_est < 0.0002){
    warning("Please be sure that strategy_aum is in same units as main_liquidity_metric")
  }

}


