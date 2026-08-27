#' Derive an Exposure Signal From a Metric
#'
#' Maps a per-date metric onto the exposure multiplier \eqn{s_t} of a risk-targeted allocation,
#' whose weight on the risky sleeve is
#' \deqn{w_t = s_t \times \left(\frac{target}{risk_t}\right)^{p}.}
#'
#' @details
#' # Why the two terms are separate
#'
#' They answer different questions and neither substitutes for the other. \eqn{s_t} says which way
#' and how strongly to lean, from a trend or a valuation signal. The risk ratio says how large that
#' lean should be given what the sleeve currently risks. Multiplying them is the standard
#' construction for running trend and volatility management together: time-series momentum in the
#' sense of Moskowitz, Ooi and Pedersen is exactly \eqn{sign(r_{t-12,t})} times an inverse-volatility
#' scaling, which is this expression at \code{method = "trend"} and \eqn{p = 1}.
#'
#' A \emph{constant} \eqn{s} would be redundant, since it could be folded into \code{target} by
#' rescaling it to \eqn{target \times s^{1/p}}. A time-varying \eqn{s_t} cannot, which is what makes
#' it a real degree of freedom rather than a second name for the target.
#'
#' # The mappings
#'
#' \describe{
#'   \item{\code{"trend"}}{\eqn{s_t = center + sensitivity \times sign(metric_t)}. Exposure depends
#'     on the direction of a trailing return, not its size, which is the time-series momentum rule.}
#'   \item{\code{"ts_adjusted"}}{The metric is z-scored against its own trailing window and
#'     \eqn{s_t = center + sensitivity \times z_t}. This is the valuation and signal-strength family:
#'     a sleeve expensive relative to its own history, or carrying an unusually strong expected
#'     return score. Comparing a metric to its own past rather than to a cross-section is what makes
#'     it a timing rule.}
#'   \item{\code{"as_is"}}{The metric is already an exposure multiplier and passes through, for a
#'     caller who computed one elsewhere.}
#' }
#'
#' There is deliberately no inverse-of-risk mapping here. That is what the \eqn{(target/risk)^p}
#' term does, and offering it in both places would let the volatility scaling be applied twice
#' without it being visible in the output.
#'
#' # Sensitivity carries the direction
#'
#' \code{sensitivity} is signed and has no default, because its sign is the entire economic claim.
#' A sleeve expensive against its own history argues for less exposure, so a valuation metric takes
#' a negative sensitivity; a strong expected-return score argues for more, so it takes a positive
#' one. Defaulting it would let the direction be chosen by accident.
#'
#' # Look-ahead
#'
#' The metric on date \code{t} sets the exposure for \code{t}, and the trailing window of
#' \code{"ts_adjusted"} ends at \code{t} inclusive. Whether the metric itself was knowable at
#' \code{t} is the caller's responsibility.
#'
#' @param metric_m_df A \code{meta_dataframe} or \code{data.frame} with \code{id}, \code{tickers}
#'   and \code{dates} plus the metric column, carrying exactly one ticker: the risky sleeve.
#' @param metric Character naming the metric column. Defaults to the only non-key column when there
#'   is exactly one.
#' @param method One of \code{"trend"}, \code{"ts_adjusted"} or \code{"as_is"}.
#' @param window Positive whole number, required for \code{"ts_adjusted"}. Length of the trailing
#'   window in observations. Dates without a full window get no exposure.
#' @param center Numeric, default 1. The multiplier a neutral signal maps to.
#' @param sensitivity Numeric, signed, required for \code{"trend"} and \code{"ts_adjusted"}.
#' @param min_exposure,max_exposure Numeric bounds on \eqn{s_t} itself, default 0 and 1. These bound
#'   the lean before the risk ratio scales it; the final weight is bounded separately by
#'   \code{risk_target_parameters}.
#' @param verbose Logical, default \code{TRUE}.
#'
#' @return A \code{data.frame} with columns \code{dates} and \code{exposure}, covering the dates for
#'   which the mapping could be computed.
#'
#' @examples
#' \dontrun{
#'   # Time-series momentum: full exposure while the trailing return is positive, half when not
#'   exposure <- derive_exposure_signal(
#'     trailing_return_m_df, metric = "return_12m",
#'     method = "trend", center = 0.75, sensitivity = 0.25
#'   )
#' }
#'
#' @seealso \code{\link{risk_target_parameters-class}}, \code{\link{estimate_sleeve_risk}}
#' @export
derive_exposure_signal <- function(metric_m_df,
                                   metric = NULL,
                                   method = c("trend", "ts_adjusted", "as_is"),
                                   window = NULL,
                                   center = 1,
                                   sensitivity = NULL,
                                   min_exposure = 0,
                                   max_exposure = 1,
                                   verbose = TRUE) {

  #Validate inputs
  ####################
  method <- match.arg(method)

  if (methods::is(metric_m_df, "meta_dataframe")) metric_m_df <- metric_m_df@data
  if (!is.data.frame(metric_m_df) ||
      !all(c("id", "tickers", "dates") %in% names(metric_m_df))) {
    rlang::abort("metric_m_df must be a data.frame or meta_dataframe with id, tickers and dates.")
  }

  ##A risk-targeted allocation scales exactly one sleeve, so an exposure signal describes one
  if (length(unique(metric_m_df$tickers)) != 1L) {
    rlang::abort(paste0("metric_m_df must carry exactly one ticker, the risky sleeve, but carries ",
                        length(unique(metric_m_df$tickers)), "."))
  }

  candidate_metrics <- setdiff(names(metric_m_df), c("id", "tickers", "dates"))
  if (is.null(metric)) {
    if (length(candidate_metrics) != 1L) {
      rlang::abort(paste0("metric must be named when metric_m_df carries more than one metric ",
                          "column. Available: ", paste(candidate_metrics, collapse = ", "), "."))
    }
    metric <- candidate_metrics
  }
  if (!metric %in% candidate_metrics) {
    rlang::abort(paste0("'", metric, "' is not a column of metric_m_df. Available: ",
                        paste(candidate_metrics, collapse = ", "), "."))
  }
  if (!is.numeric(metric_m_df[[metric]])) {
    rlang::abort(paste0("'", metric, "' must be numeric."))
  }
  if (any(is.na(metric_m_df[[metric]]))) {
    rlang::abort(paste0("'", metric, "' must not contain NA. Decide how a missing metric should be ",
                        "treated before mapping it onto an exposure."))
  }

  check_single_number <- function(value, name, allow_null = FALSE) {
    if (is.null(value)) {
      if (allow_null) return(invisible(NULL))
      rlang::abort(paste0(name, " must be supplied."))
    }
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      rlang::abort(paste0(name, " must be a single finite number."))
    }
    invisible(NULL)
  }

  check_single_number(center, "center")
  check_single_number(min_exposure, "min_exposure")
  check_single_number(max_exposure, "max_exposure")
  if (min_exposure < 0) {
    rlang::abort("min_exposure must not be negative: exposure scales a long-only sleeve.")
  }
  if (min_exposure > max_exposure) {
    rlang::abort("min_exposure must not exceed max_exposure.")
  }

  if (method %in% c("trend", "ts_adjusted")) {
    check_single_number(sensitivity,
                        paste0("sensitivity, which is required for method '", method,
                               "' and whose sign sets the direction of the rule"))
  }
  if (method == "ts_adjusted") {
    if (is.null(window) || !is.numeric(window) || length(window) != 1L || is.na(window) ||
        window < 2 || window %% 1 != 0) {
      rlang::abort("window must be a single whole number of at least 2 for method 'ts_adjusted'.")
    }
  }
  ####################

  #Map the metric onto an exposure
  ####################
  metric_m_df <- metric_m_df %>%
    dplyr::mutate(dates = as.Date(dates)) %>%
    dplyr::arrange(dates)
  metric_values <- metric_m_df[[metric]]

  if (method == "trend") {
    ##sign(0) is 0, so a flat trailing return sits at the centre rather than picking a side
    raw_exposure <- center + sensitivity * sign(metric_values)

  } else if (method == "as_is") {
    raw_exposure <- metric_values

  } else {
    ##Z-score against the sleeve's own trailing history, ending at the date being weighted
    raw_exposure <- rep(NA_real_, length(metric_values))
    if (length(metric_values) >= window) {
      for (i in seq(window, length(metric_values))) {
        history <- metric_values[(i - window + 1):i]
        history_sd <- stats::sd(history)
        ###A flat window says nothing about where the metric stands, so it is neutral
        z_score <- if (is.na(history_sd) || history_sd == 0) 0 else
          (metric_values[i] - mean(history)) / history_sd
        raw_exposure[i] <- center + sensitivity * z_score
      }
    }

    incomplete_window <- sum(is.na(raw_exposure))
    if (incomplete_window > 0L && isTRUE(verbose)) {
      ##Dropped rather than computed on a shorter window, which would be a different estimator,
      ##and never back-filled, which would borrow from dates that had not happened
      message(incomplete_window, " date(s) lacked a full ", window,
              "-observation window and carry no exposure.")
    }
  }

  exposure_m_df <- data.frame(
    dates = metric_m_df$dates,
    exposure = pmin(pmax(raw_exposure, min_exposure), max_exposure),
    stringsAsFactors = FALSE
  )
  exposure_m_df <- exposure_m_df[!is.na(exposure_m_df$exposure), , drop = FALSE]
  rownames(exposure_m_df) <- NULL

  if (nrow(exposure_m_df) == 0L) {
    rlang::abort(paste0("No date has an exposure: the metric history is shorter than the ",
                        "configured window."))
  }

  if (isTRUE(verbose)) {
    message("Derived an exposure signal by '", method, "' over ", nrow(exposure_m_df),
            " date(s), ranging from ", round(min(exposure_m_df$exposure), 4), " to ",
            round(max(exposure_m_df$exposure), 4), ".")
  }
  ####################

  return(exposure_m_df)
}
