#' Create a Performance Metrics Data Frame
#'
#' This function calculates a wide range of performance and risk metrics for financial signals based on backtest returns and a benchmark.
#' The results are returned as a structured data frame.
#'
#' @param selected_backtest_returns_corrected_positions_xts_upd_ref An `xts` object containing backtested returns for the selected signals
#' Each column represents a signal, and rows represent time periods.
#'
#' @param selected_market_factor_proxy_xts_upd_ref A xts containing benchmark returns data.
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
#' @examples
#' # Example usage:
#' performance_m_df <- create_performance_m_df(
#'   selected_signals = c("Signal_A", "Signal_B"),
#'   current_date = "2024-12-12",
#'   selected_backtest_returns_corrected_positions_xts_upd_ref = backtest_xts,
#'   baseline_benchmark_xts_upd_ref = benchmark_xts
#' )
#' @export
create_performance_m_df <- function(selected_backtest_returns_corrected_positions_xts_upd_ref, selected_market_factor_proxy_xts_upd_ref, active_returns){

  #Initial Preparations
  ##################
  ###Get objects from selected_backtest_returns_corrected_positions_xts_upd_ref
  selected_signals <- colnames(selected_backtest_returns_corrected_positions_xts_upd_ref)
  current_date <- zoo::index(selected_backtest_returns_corrected_positions_xts_upd_ref)[length(zoo::index(selected_backtest_returns_corrected_positions_xts_upd_ref))]

  ###Get baseline benchmark
  baseline_benchmark_xts_upd_ref <- xts::xts(rowMeans(selected_backtest_returns_corrected_positions_xts_upd_ref),
                                             order.by = zoo::index(selected_backtest_returns_corrected_positions_xts_upd_ref))

  ###Get decimals
  selected_backtest_returns_corrected_positions_xts_upd_ref_decimals <- selected_backtest_returns_corrected_positions_xts_upd_ref/100
  selected_market_factor_proxy_vector_upd_ref_decimals <- as.vector(selected_market_factor_proxy_xts_upd_ref/100)
  baseline_benchmark_xts_upd_ref_decimals <- baseline_benchmark_xts_upd_ref/100

  ##Check if active returns should be calculated
  if(active_returns){
    ##Get geometric active returns
    ###selected_backtest_returns_corrected_positions_xts_upd_ref_decimals
    selected_backtest_returns_corrected_positions_xts_upd_ref_decimals <- xts::xts(
      sapply(
        #For each series
        colnames(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals),
        function(series) {
          #Apply geometric return differnece formula
          purrr::map2_dbl(
            selected_backtest_returns_corrected_positions_xts_upd_ref_decimals[, series], #.x
            selected_market_factor_proxy_vector_upd_ref_decimals, #.y
            ~ (1 + .x) / (1 + .y) - 1 #.f
          )
        }
      ),
      order.by = zoo::index(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
    )
    ###baseline_benchmark_xts_upd_ref_decimals
    baseline_benchmark_xts_upd_ref_decimals <- (1 + baseline_benchmark_xts_upd_ref_decimals)/(1 + selected_market_factor_proxy_vector_upd_ref_decimals) - 1
  }

  #################

  ##Calculate base metrics using PerformanceAnalytics
  ##########################
    performance_m_df <- data.frame(
      # ID
      id = paste0(selected_signals, "-", current_date),
      # Tickers
      tickers = selected_signals,
      # Dates
      dates = current_date,
      # Return Metrics
      ## Mean Arithmetic Return
      arith_mean_ret = as.numeric(
        PerformanceAnalytics::Mean.arithmetic(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals, na.rm = TRUE) * 100
      ),
      ## Mean Geometric Return
      geom_mean_ret = as.numeric(
        PerformanceAnalytics::mean.geometric(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals, na.rm = TRUE) * 100
      ),
      ## Annualized Return
      ann_ret = as.numeric(
        PerformanceAnalytics::Return.annualized(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals) * 100
      ),
      # Risk Metrics
      ## Standard Deviation
      std_dev = as.numeric(
        PerformanceAnalytics::StdDev(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals) * 100
      ),
      ## Annualized Standard Deviation
      ann_std_dev = as.numeric(
        PerformanceAnalytics::StdDev.annualized(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals) * 100
      ),
      ## Semi Deviation
      semi_dev = as.numeric(
        PerformanceAnalytics::SemiDeviation(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals) * 100
      ),
      ## Downside Deviation
      down_dev = as.numeric(
        PerformanceAnalytics::DownsideDeviation(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals) * 100
      ),
      ## Drawdown Deviation
      dd_dev = as.numeric(
        PerformanceAnalytics::DrawdownDeviation(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals) * 100
      ),
      ## Downside Frequency
      down_freq = as.numeric(
        PerformanceAnalytics::DownsideFrequency(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      ## Expected Shortfall
      exp_short = as.numeric(
        PerformanceAnalytics::ETL(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals) * 100
      ),
      ## Pain Index (Average Absolute Drawdown)
      pain = as.numeric(
        PerformanceAnalytics::PainIndex(R = selected_backtest_returns_corrected_positions_xts_upd_ref_decimals) * 100
      ),
      ## Ulcer Index (Average Squared Drawdown)
      ulcer = as.numeric(
        PerformanceAnalytics::UlcerIndex(R = selected_backtest_returns_corrected_positions_xts_upd_ref_decimals) * 100
      ),
      ## Maximum Drawdown
      max_dd = as.numeric(
        PerformanceAnalytics::maxDrawdown(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals) * 100
      ),
      ## Skewness
      skew = as.numeric(
        PerformanceAnalytics::skewness(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      ## Kurtosis
      kurt = as.numeric(
        PerformanceAnalytics::kurtosis(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      # Ratios
      ## Sharpe Ratio
      sharpe_ratio = as.numeric(
        PerformanceAnalytics::SharpeRatio(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals, FUN = "StdDev")
      ),
      ## Annualized Sharpe Ratio
      ann_sharpe_ratio = as.numeric(
        PerformanceAnalytics::SharpeRatio.annualized(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      ## Sharpe Ratio (Semi Deviation)
      sharpe_ratio_semi_dev = as.numeric(
        PerformanceAnalytics::SharpeRatio(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals, FUN = "SemiSD")
      ),
      ## Sortino Ratio
      sortino_ratio = as.numeric(
        PerformanceAnalytics::SortinoRatio(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      ## Annualized Burke Ratio
      ann_burke_ratio = as.numeric(
        PerformanceAnalytics::BurkeRatio(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals, modified = TRUE)
      ),
      ## Inverted DRatio
      inv_d_ratio = as.numeric(
        1 / PerformanceAnalytics::DRatio(R = selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      ## Sharpe Ratio (Expected Shortfall)
      sharpe_ratio_exp_short = as.numeric(
        PerformanceAnalytics::SharpeRatio(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals, FUN = "ES")
      ),
      ## Annualized Pain Ratio
      ann_pain_ratio = as.numeric(
        PerformanceAnalytics::PainRatio(R = selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      ## Annualized Martin Ratio
      ann_martin_ratio = as.numeric(
        PerformanceAnalytics::MartinRatio(R = selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      ## Annualized Calmar Ratio
      ann_calmar_ratio = as.numeric(
        PerformanceAnalytics::CalmarRatio(R = selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      ## Adjusted Sharpe Ratio
      ann_adj_sharpe_ratio = as.numeric(
        PerformanceAnalytics::AdjustedSharpeRatio(R = selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      ## Omega Ratio
      omega = as.numeric(
        PerformanceAnalytics::Omega(R = selected_backtest_returns_corrected_positions_xts_upd_ref_decimals, L = 0)
      ),
      ## Rachev Ratio
      rachev_ratio = as.numeric(
        PerformanceAnalytics::RachevRatio(R = selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      # Other Metrics
      ## Average Recovery
      avg_dd_rec = as.numeric(
        PerformanceAnalytics::AverageRecovery(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      ## Average Drawdown Length
      avg_dd_length = as.numeric(
        PerformanceAnalytics::AverageLength(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      ## Hurst Index
      hurst = as.numeric(
        PerformanceAnalytics::HurstIndex(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals)
      ),
      ## Minimum Track Record (for statistical significance)
      min_track_record = as.numeric(
        apply(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals, 2, function(x) {
          tryCatch(
            PerformanceAnalytics::MinTrackRecord(x, refSR = 0)$num_of_extra_obs_needed,
            error = function(e) NA
          )
        })
      ),
      ## Probabilistic Sharpe Ratio
      prob_sharpe_ratio = as.numeric(
        apply(selected_backtest_returns_corrected_positions_xts_upd_ref_decimals, 2, function(x) {
          tryCatch(
            PerformanceAnalytics::ProbSharpeRatio(x, refSR = 0)$sr_prob,
            error = function(e) NA
          )
        })
      ),
      # Benchmark-Relative Metrics
      ## Modigliani Ratio
      modigliani = as.numeric(
        PerformanceAnalytics::Modigliani(Ra = selected_backtest_returns_corrected_positions_xts_upd_ref_decimals,
                                         Rb = baseline_benchmark_xts_upd_ref_decimals)*100
      ),
      ## MSquared Ratio
      ann_modigliani = as.numeric(
        PerformanceAnalytics::MSquared(Ra = selected_backtest_returns_corrected_positions_xts_upd_ref_decimals,
                                       Rb = baseline_benchmark_xts_upd_ref_decimals)*100
      )
    )

  ##########################

  return(performance_m_df)

}
