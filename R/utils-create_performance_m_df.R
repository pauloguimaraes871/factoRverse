#' Create a Performance Metrics Data Frame
#'
#' This function calculates a wide range of performance and risk metrics for financial signals based on backtest returns and a benchmark.
#' The results are returned as a structured data frame.
#'
#' @param selected_backtest_returns_corrected_positions_m_xts_upd_ref An `xts` object containing backtested returns for the selected signals
#' Each column represents a signal, and rows represent time periods.
#'
#' @param selected_market_factor_proxy_m_xts_upd_ref A xts containing benchmark returns data.
#'
#' @param active_returns If TRUE, calculate ative returns before applying performance functions.
#'
#' @return A data frame containing performance metrics for the specified signals. The metrics include:
#' - **ID Information**: `id`, `tickers`, `dates`.
#' - **Return Metrics**: Arithmetic mean return, geometric mean return, annualized return.
#' - **Risk Metrics**: Standard deviation, semi deviation, downside deviation, maximum drawdown, and more.
#' - **Performance Ratios**: Sharpe ratio, Sortino ratio, Calmar ratio, Omega ratio, Rachev ratio, and others.
#' - **Additional Metrics**: Average recovery, Hurst index, probabilistic Sharpe ratio, and minimum track record.
#'
#' @param verbose If TRUE, will print messages to the console.
#'
#' @export
create_performance_m_df <- function(selected_backtest_returns_corrected_positions_m_xts_upd_ref, selected_market_factor_proxy_m_xts_upd_ref, active_returns, verbose = TRUE){

  #Check for selected_market_factor_proxy if active_returns is TRUE
  if(is.null(selected_market_factor_proxy_m_xts_upd_ref) && active_returns){
    stop("The selected_market_factor_proxy_m_xts_upd_ref object can't be NULL when active_returns is TRUE.")
  }

  #Initial Preparations
  ##################
  ###Get objects from selected_backtest_returns_corrected_positions_m_xts_upd_ref
  selected_signals <- colnames(selected_backtest_returns_corrected_positions_m_xts_upd_ref)
  current_date <- zoo::index(selected_backtest_returns_corrected_positions_m_xts_upd_ref) %>% max()

  ###Get baseline benchmark
  baseline_benchmark_m_xts_upd_ref <- xts::xts(rowMeans(selected_backtest_returns_corrected_positions_m_xts_upd_ref, na.rm = TRUE),
                                             order.by = zoo::index(selected_backtest_returns_corrected_positions_m_xts_upd_ref))

  ###Get decimals
  selected_backtest_returns_corrected_positions_m_xts_upd_ref_decimals <- selected_backtest_returns_corrected_positions_m_xts_upd_ref/100
  selected_market_factor_proxy_vector_upd_ref_decimals <- as.vector(selected_market_factor_proxy_m_xts_upd_ref/100)
  baseline_benchmark_m_xts_upd_ref_decimals <- baseline_benchmark_m_xts_upd_ref/100

  ##Check if active returns should be calculated
  if(active_returns){
    ##Get geometric active returns
    ###selected_backtest_returns_corrected_positions_m_xts_upd_ref_decimals
    selected_backtest_returns_corrected_positions_m_xts_upd_ref_decimals <- xts::xts(
      sapply(
        #For each series
        colnames(selected_backtest_returns_corrected_positions_m_xts_upd_ref_decimals),
        function(series) {
          #Apply geometric return difference formula
          purrr::map2_dbl(
            selected_backtest_returns_corrected_positions_m_xts_upd_ref_decimals[, series], #.x
            selected_market_factor_proxy_vector_upd_ref_decimals, #.y
            ~ (1 + .x) / (1 + .y) - 1 #.f
          )
        }
      ),
      order.by = zoo::index(selected_backtest_returns_corrected_positions_m_xts_upd_ref_decimals)
    )
    ###baseline_benchmark_m_xts_upd_ref_decimals
    baseline_benchmark_m_xts_upd_ref_decimals <- (1 + baseline_benchmark_m_xts_upd_ref_decimals)/(1 + selected_market_factor_proxy_vector_upd_ref_decimals) - 1
  }

  ###Message
  if(verbose){
    # Display the starting message based on the type of returns
    if(active_returns) {
      cat("\nStarting to calculate active performance metrics...\n")
    } else {
      cat("\nStarting to calculate raw performance metrics...\n")
    }

    # Identify columns where all values are positive
    positive_columns <- colnames(selected_backtest_returns_corrected_positions_m_xts_upd_ref_decimals)[
      apply(selected_backtest_returns_corrected_positions_m_xts_upd_ref_decimals, 2, function(x) all(x > 0))
    ]
    positive_columns <- positive_columns[which(!is.na(positive_columns))] #Remove NAs

    # Check if there are any columns with all positive returns
    if(length(positive_columns) > 0){
      if(active_returns) {
        warning("The following active return columns only contains positive values, compromising some calculations: ",
                paste(positive_columns, collapse = ", "))
      } else {
        warning("The following raw return columns only contains positive values, compromising some calculations: ",
                paste(positive_columns, collapse = ", "))
      }
    }

    # Identify columns where all values are NA
    NA_columns <- colnames(selected_backtest_returns_corrected_positions_m_xts_upd_ref_decimals)[
      apply(selected_backtest_returns_corrected_positions_m_xts_upd_ref_decimals, 2, function(x) all(is.na(x)))
    ]
    # Check if there are any columns with all positive returns
    if(length(NA_columns) > 0){
      if(active_returns) {
        warning("The following active return columns only contains NA: ",
                paste(NA_columns, collapse = ", "))
      } else {
        warning("The following raw return columns only contains NA: ",
                paste(NA_columns, collapse = ", "))
      }
    }


  }


  #################

  ##Calculate base metrics using PerformanceAnalytics
  ##########################

  ##Create own function to deal with NAs properly
  safe_compute <- function(strategy_returns, metric_fun, multiply = 1, benchmark_returns = NULL, ...) {
    if (!is.null(benchmark_returns)) {
      # Align based on non-NA strategy returns
      common_index <- zoo::index(strategy_returns)[!is.na(strategy_returns)]
      strategy_clean <- strategy_returns[common_index]
      benchmark_clean <- benchmark_returns[common_index]

      # Check if data exists after alignment
      if (length(strategy_clean) == 0 || length(benchmark_clean) == 0) {
        return(NA)
      }

      # Attempt to compute the metric with benchmark
      result <- tryCatch(
        multiply * as.numeric(metric_fun(strategy_clean, Rb = benchmark_clean, ...)),
        error = function(e) NA
      )
    } else {
      # Remove NA values from strategy returns
      strategy_clean <- strategy_returns[!is.na(strategy_returns)]

      # Check if data exists after removing NAs
      if (length(strategy_clean) == 0) {
        return(NA)
      }

      # Attempt to compute the metric without benchmark
      result <- tryCatch(
        multiply * as.numeric(metric_fun(strategy_clean, ...)),
        error = function(e) NA
      )
    }

    return(result)
  }

  ##Build the data.frame with column-wise computations

  ### Ensure Alignment of Strategy and Benchmark Data
  ### This step assumes that both xts objects share the same date index or have overlapping dates.
  ### Adjust as necessary based on your data structure.
  common_dates <- zoo::index(selected_backtest_returns_corrected_positions_m_xts_upd_ref_decimals) %in% zoo::index(baseline_benchmark_m_xts_upd_ref_decimals)
  selected_returns_aligned <- selected_backtest_returns_corrected_positions_m_xts_upd_ref_decimals[common_dates]
  benchmark_aligned <- baseline_benchmark_m_xts_upd_ref_decimals[common_dates]


  ##Construct the performance_m_df Data Frame
  performance_m_df <- data.frame(
    # ID
    id = paste0(selected_signals, "-", current_date),

    # Tickers
    tickers = selected_signals,

    # Dates
    dates = current_date,

    # Return Metrics
    ## Mean Arithmetic Return
    arith_mean_ret = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::Mean.arithmetic,
        multiply = 100,
        na.rm = TRUE
      )
    }),

    ## Mean Geometric Return
    geom_mean_ret = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::mean.geometric,
        multiply = 100,
        na.rm = TRUE
      )
    }),

    ## Annualized Return
    ann_ret = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::Return.annualized,
        multiply = 100
      )
    }),

    # Risk Metrics
    ## Standard Deviation
    std_dev = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::StdDev,
        multiply = 100
      )
    }),

    ## Annualized Standard Deviation
    ann_std_dev = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::StdDev.annualized,
        multiply = 100
      )
    }),

    ## Semi Deviation
    semi_dev = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::SemiDeviation,
        multiply = 100
      )
    }),

    ## Downside Deviation
    down_dev = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::DownsideDeviation,
        multiply = 100
      )
    }),

    ## Drawdown Deviation
    dd_dev = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::DrawdownDeviation,
        multiply = 100
      )
    }),

    ## Downside Frequency
    down_freq = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::DownsideFrequency
      )
    }),

    ## Expected Shortfall
    exp_short = sapply(selected_returns_aligned, function(x) {
      # Multiply by -100 as per original code
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::ETL,
        multiply = -100
      )
    }),

    ## Pain Index (Average Absolute Drawdown)
    pain = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::PainIndex,
        multiply = 100
      )
    }),

    ## Ulcer Index (Average Squared Drawdown)
    ulcer = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::UlcerIndex,
        multiply = 100
      )
    }),

    ## Maximum Drawdown
    max_dd = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::maxDrawdown,
        multiply = 100
      )
    }),

    ## Skewness
    skew = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::skewness
      )
    }),

    ## Kurtosis
    kurt = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::kurtosis
      )
    }),

    # Ratios
    ## Sharpe Ratio
    sharpe_ratio = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::SharpeRatio,
        FUN = "StdDev"
      )
    }),

    ## Annualized Sharpe Ratio
    ann_sharpe_ratio = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::SharpeRatio.annualized
      )
    }),

    ## Sharpe Ratio (Semi Deviation)
    sharpe_ratio_semi_dev = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::SharpeRatio,
        FUN = "SemiSD"
      )
    }),

    ## Sortino Ratio
    sortino_ratio = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::SortinoRatio
      )
    }),

    ## Annualized Burke Ratio
    ann_burke_ratio = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::BurkeRatio,
        modified = TRUE
      )
    }),

    ## Inverted DRatio
    inv_d_ratio = sapply(selected_returns_aligned, function(x) {
      d_ratio <- safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::DRatio
      )
      if (is.na(d_ratio) || d_ratio == 0) {
        return(NA)
      } else {
        return(1 / d_ratio)
      }
    }),

    ## Sharpe Ratio (Expected Shortfall)
    sharpe_ratio_exp_short = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::SharpeRatio,
        FUN = "ES"
      )
    }),

    ## Annualized Pain Ratio
    ann_pain_ratio = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::PainRatio
      )
    }),

    ## Annualized Martin Ratio
    ann_martin_ratio = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::MartinRatio
      )
    }),

    ## Annualized Calmar Ratio
    ann_calmar_ratio = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::CalmarRatio
      )
    }),

    ## Adjusted Sharpe Ratio
    ann_adj_sharpe_ratio = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::AdjustedSharpeRatio
      )
    }),

    ## Omega Ratio
    omega = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::Omega,
        L = 0
      )
    }),

    ## Rachev Ratio
    rachev_ratio = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::RachevRatio
      )
    }),

    # Other Metrics
    ## Average Recovery
    avg_dd_rec = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::AverageRecovery
      )
    }),

    ## Average Drawdown Length
    avg_dd_length = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::AverageLength
      )
    }),

    ## Hurst Index
    hurst = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::HurstIndex
      )
    }),

    ## Minimum Track Record (for statistical significance)
    min_track_record = sapply(selected_returns_aligned, function(x) {
      strategy_clean <- x[!is.na(x)]
      if (length(strategy_clean) == 0) {
        return(NA)
      }
      tryCatch(
        PerformanceAnalytics::MinTrackRecord(strategy_clean, refSR = 0)$num_of_extra_obs_needed,
        error = function(e) NA
      )
    }),

    ## Probabilistic Sharpe Ratio
    prob_sharpe_ratio = sapply(selected_returns_aligned, function(x) {
      strategy_clean <- x[!is.na(x)]
      if (length(strategy_clean) == 0) {
        return(NA)
      }
      tryCatch(
        PerformanceAnalytics::ProbSharpeRatio(strategy_clean, refSR = 0)$sr_prob,
        error = function(e) NA
      )
    }),

    # Benchmark-Relative Metrics
    ## Modigliani Ratio (using single benchmark column)
    modigliani = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::Modigliani,
        multiply = 100,
        benchmark_returns = benchmark_aligned
      )
    }),

    ## MSquared Ratio (using single benchmark column)
    ann_modigliani = sapply(selected_returns_aligned, function(x) {
      safe_compute(
        strategy_returns = x,
        metric_fun = PerformanceAnalytics::MSquared,
        multiply = 100,
        benchmark_returns = benchmark_aligned
      )
    })
  )


  ##########################
  ##Rename according to active_returns
  if(active_returns){
    colnames(performance_m_df)[-c(1:3)] <- paste0("act_", colnames(performance_m_df)[-c(1:3)])
    colnames(performance_m_df)[c(7, 8, 19, 20, 21, 25, 29, 36)] <-
      c("track_err", "ann_track_err", "info_ratio", "ann_info_ratio", "info_ratio_semi_dev", "info_ratio_exp_short", "ann_adj_info_ratio", "prob_info_ratio")
  }

  ###Message
  if(verbose) cat("\nPerformance metrics calculated.\n")
  return(performance_m_df)

}
