#' Estimate a Portfolio Sleeve's Current Risk
#'
#' Measures how risky a portfolio is as of a given date, which is the denominator of the
#' risk-targeting rule in the `risk_targeted` meta-portfolio path. Returns an annualised figure in
#' percentage points, matching the units a `cml_parameters` target is stated in.
#'
#' @details
#' # Which risk
#'
#' \code{target_metric} decides what is being measured, and the weights follow from it.
#' \itemize{
#'   \item \code{"volatility"} uses the sleeve's own weights, giving total volatility.
#'   \item \code{"tracking_error"} uses \emph{active} weights, the sleeve's weights minus the
#'     benchmark's, against a raw covariance matrix. That is how \code{calculate_port_stats()}
#'     derives \code{act_risk}, so the convention is inherited rather than invented. Estimating the
#'     covariance on active returns as well would subtract the benchmark twice, which
#'     \code{cml_parameters} refuses.
#' }
#'
#' # Where the number comes from
#'
#' \describe{
#'   \item{\code{"ex_ante"}}{A covariance matrix estimated from daily stock returns over a short
#'     window ending at \code{current_date}, applied to the weights held then. This describes the
#'     portfolio as it stands. The alternatives do not: a window of past monthly portfolio returns
#'     describes a chain of past compositions, and the \code{act_risk} already sitting in the base
#'     backtest's \code{port_stats} was estimated over a long window and only refreshed on that
#'     backtest's own rebalance dates.}
#'   \item{\code{"realized_rolling"}}{Standard deviation of the sleeve's own past monthly returns,
#'     active returns for a tracking-error target.}
#'   \item{\code{"supplied"}}{Read from a series the caller computed, taken as already annualised.}
#' }
#'
#' # Units
#'
#' Returns are in percentage points throughout the package, so a standard deviation of them is too.
#' A daily covariance gives a daily standard deviation, annualised here by \eqn{\sqrt{252}}; a
#' monthly one by \eqn{\sqrt{12}}. A supplied series is assumed to be annualised already, since the
#' caller computed it and only they know its frequency.
#'
#' @param current_date The date to measure at. Only data up to and including it is used.
#' @param cml_params A \code{cml_parameters} object.
#' @param risky_port_backtest_results The \code{port_backtest_results} for the sleeve being scaled.
#' @param daily_stock_returns_m_xts Daily stock returns, required for \code{"ex_ante"}.
#' @param selected_benchmark Character naming the benchmark, required for a tracking-error target.
#' @param stock_groups_m_d_ref Optional groups for the date, passed through to
#'   \code{estimate_covariance_matrix()}. It is needed whenever the daily return sample contains
#'   missing values, since those are filled from group medians; without it the estimator fails on
#'   any series with gaps.
#' @param vol_m_df Optional \code{data.frame} with \code{tickers}, \code{dates} and a risk column,
#'   required for \code{"supplied"}.
#' @param return_basis \code{"net"} or \code{"raw"}, for \code{"realized_rolling"}.
#'
#' @return A single annualised risk figure in percentage points, or \code{NA_real_} when there is
#'   not enough history to estimate one.
#'
#' @seealso \code{\link{cml_parameters-class}}, \code{\link{estimate_covariance_matrix}}
#' @keywords internal
estimate_sleeve_risk <- function(current_date,
                                 cml_params,
                                 risky_port_backtest_results,
                                 daily_stock_returns_m_xts = NULL,
                                 selected_benchmark = NULL,
                                 stock_groups_m_d_ref = NULL,
                                 vol_m_df = NULL,
                                 return_basis = "net") {

  current_date <- as.Date(current_date)
  is_tracking_error <- cml_params@target_metric == "tracking_error"

  #Supplied
  ####################
  if (cml_params@vol_source == "supplied") {
    if (is.null(vol_m_df)) {
      rlang::abort("vol_m_df must be supplied when vol_source is 'supplied'.")
    }
    risk_column <- setdiff(names(vol_m_df), c("id", "tickers", "dates"))
    if (length(risk_column) != 1L) {
      rlang::abort("vol_m_df must carry exactly one risk column besides id, tickers and dates.")
    }
    row <- vol_m_df[as.Date(vol_m_df$dates) == current_date, , drop = FALSE]
    if (nrow(row) == 0L) return(NA_real_)
    ##Taken as annualised: only the caller knows what frequency it was computed at
    return(as.numeric(row[[risk_column]][1]))
  }
  ####################

  #Realized rolling
  ####################
  if (cml_params@vol_source == "realized_rolling") {
    returns_xts <- risky_port_backtest_results@port_returns_m_xts@data
    return_column <- if (is_tracking_error) {
      paste0(return_basis, "_active_return")
    } else {
      paste0(return_basis, "_return")
    }
    if (!return_column %in% colnames(returns_xts)) {
      rlang::abort(paste0("The risky sleeve has no '", return_column, "' column. A tracking-error ",
                          "target needs the sleeve to have been run against a benchmark."))
    }

    window_xts <- returns_xts[zoo::index(returns_xts) <= current_date, return_column, drop = FALSE]
    observations <- stats::na.omit(as.numeric(window_xts))
    if (length(observations) < cml_params@vol_window) return(NA_real_)

    observations <- utils::tail(observations, cml_params@vol_window)
    ##Monthly observations, so annualise by the square root of twelve
    return(stats::sd(observations) * sqrt(12))
  }
  ####################

  #Ex ante
  ####################
  if (is.null(daily_stock_returns_m_xts)) {
    rlang::abort("daily_stock_returns_m_xts is required when vol_source is 'ex_ante'.")
  }

  ##Weights held at this date. bench_weights sits in the same object when the sleeve was run
  ##against a benchmark, which is what makes active weights available.
  weights_m_d_ref <- risky_port_backtest_results@port_weights_m_df@data %>%
    dplyr::filter(as.Date(dates) == current_date)
  if (nrow(weights_m_d_ref) == 0L) return(NA_real_)

  if (is_tracking_error) {
    if (!"bench_weights" %in% names(weights_m_d_ref)) {
      rlang::abort("The risky sleeve carries no bench_weights, so active weights cannot be formed ",
                   "for a tracking-error target. Run it against a benchmark first.")
    }
    ##Active weights against a raw covariance: the same formulation calculate_port_stats() uses
    weights_m_d_ref$risk_weights <- weights_m_d_ref$eop_port_weights - weights_m_d_ref$bench_weights
  } else {
    weights_m_d_ref$risk_weights <- weights_m_d_ref$eop_port_weights
  }

  ##Only names carrying a position contribute, and a covariance matrix cannot be formed for names
  ##the daily series does not cover
  held <- weights_m_d_ref %>%
    dplyr::filter(abs(risk_weights) > 0) %>%
    dplyr::filter(tickers %in% colnames(daily_stock_returns_m_xts))
  if (nrow(held) == 0L) return(NA_real_)

  returns_upd_ref <- daily_stock_returns_m_xts[
    zoo::index(daily_stock_returns_m_xts) <= current_date, , drop = FALSE]
  if (nrow(returns_upd_ref) < cml_params@vol_cov_est_method@cov_matrix_sample_size) {
    return(NA_real_)
  }

  covariance_matrix <- estimate_covariance_matrix(
    tickers = held$tickers,
    returns_m_xts_upd_ref = returns_upd_ref,
    cov_matrix_sample_size = cml_params@vol_cov_est_method@cov_matrix_sample_size,
    cov_estimation_method = cml_params@vol_cov_est_method@cov_estimation_method,
    active_returns = FALSE,
    selected_benchmark_m_xts_upd_ref = NULL,
    groups_m_d_ref = stock_groups_m_d_ref,
    verbose = FALSE
  )

  risk_weights <- held$risk_weights
  names(risk_weights) <- held$tickers
  risk_weights <- risk_weights[rownames(covariance_matrix)]

  daily_variance <- as.numeric(t(risk_weights) %*% covariance_matrix %*% risk_weights)
  if (!is.finite(daily_variance) || daily_variance < 0) return(NA_real_)

  ##Daily observations, so annualise by the square root of the trading year
  sqrt(daily_variance) * sqrt(252)
}


#' Turn a Risk Estimate Into a Weight on the Risky Sleeve
#'
#' Applies the risk-targeting rule
#' \eqn{w = (target / risk)^{p}} and clips it to the configured bounds. A risk estimate at the
#' target gives full exposure; twice the target gives half at \eqn{p = 1} and a quarter at
#' \eqn{p = 2}.
#'
#' @param risk A single annualised risk figure, in the target's units.
#' @param cml_params A \code{cml_parameters} object.
#'
#' @return A single weight in \code{[min_weight, max_weight]}, or \code{NA_real_} when the risk
#'   estimate is missing or not positive.
#' @keywords internal
risk_to_weight <- function(risk, cml_params) {

  if (length(risk) != 1L || is.na(risk) || !is.finite(risk) || risk <= 0) {
    return(NA_real_)
  }

  raw_weight <- (cml_params@target / risk)^cml_params@p

  min(max(raw_weight, cml_params@min_weight), cml_params@max_weight)
}
