#' Derive Timing Weights From a Metric
#'
#' Maps a per-asset time series of some metric onto portfolio weights, so an allocation can be
#' scaled up and down through time by a signal the user chooses. This is the outer half of a timing
#' overlay: it produces weights, which are then handed to a backtest rather than derived inside one.
#'
#' @details
#' # The four mappings
#'
#' Each mirrors the canonical implementation of its idea.
#'
#' \describe{
#'   \item{\code{"inverse"}}{\eqn{w = (target / metric)^{exponent}}. At \code{exponent = 1} this is
#'     ordinary volatility or tracking-error targeting: a metric of 8 against a target of 4 halves
#'     the exposure. At \code{exponent = 2} it is the inverse-variance response of a
#'     volatility-managed portfolio in the sense of Moreira and Muir. The metric must be positive,
#'     since it stands for a risk measure.}
#'   \item{\code{"ts_adjusted"}}{The metric is z-scored against its own trailing history, then
#'     \eqn{w = center + sensitivity \times z}. This is the valuation- and signal-timing family:
#'     a portfolio's price-to-earnings against its own long-run average, or its expected-return
#'     score against the same. Comparing a metric to its own history rather than to a
#'     cross-section is what makes it a timing rule rather than a selection rule.}
#'   \item{\code{"trend"}}{\eqn{w = center + sensitivity \times sign(metric)}, the time-series
#'     momentum rule: exposure depends on whether a trailing return was positive, not on its size.}
#'   \item{\code{"as_is"}}{The metric is already a weight and is passed through, so a caller who
#'     has computed weights elsewhere can use the same box and residual handling.}
#' }
#'
#' # Sensitivity carries the economics
#'
#' \code{sensitivity} is signed and has no default for \code{"ts_adjusted"} and \code{"trend"},
#' because its sign is the entire economic claim. A portfolio expensive relative to its own history
#' argues for less exposure, so a valuation metric takes a negative sensitivity; an expected-return
#' score high relative to its own history argues for more, so it takes a positive one. Defaulting it
#' would let the direction be chosen by accident.
#'
#' # Why target must be supplied
#'
#' For \code{"inverse"}, \code{target} has to be given. The volatility-managed portfolio literature
#' often sets the constant so that the managed and unmanaged series have equal volatility over the
#' whole sample, which is a full-sample calibration and therefore look-ahead in a backtest. There is
#' no option here to compute it that way: either state the target, or compute one yourself on an
#' expanding window and pass it in.
#'
#' # Box constraints and the residual asset
#'
#' Weights are clipped to \code{[min_weight, max_weight]} after mapping. What happens next depends
#' on whether a residual asset is named:
#' \itemize{
#'   \item With \code{residual_asset}, that asset absorbs whatever the others leave unallocated,
#'     so the box holds exactly on the assets it constrains. This is the risk-free-plus-risky case:
#'     a floor of 0.5 on the risky sleeve means the cash line never exceeds a half.
#'   \item Without one, weights are rescaled to sum to one, so the box acts on the mapping output
#'     rather than on the final weights. It bounds the relative tilt, not the absolute allocation.
#' }
#'
#' # Look-ahead
#'
#' The metric on date \code{t} is used to set the weight for \code{t}, and the trailing window of
#' \code{"ts_adjusted"} ends at \code{t} inclusive. Whether the metric itself was knowable at
#' \code{t} is the caller's responsibility: a realized volatility stamped at \code{t} and measured
#' over the month ending there is fine, one measured over the month beginning there is not.
#'
#' @param metric_m_df A \code{meta_dataframe} or \code{data.frame} with \code{id}, \code{tickers}
#'   and \code{dates}, plus the metric column.
#' @param metric Character naming the metric column. Defaults to the only non-key column when
#'   there is exactly one.
#' @param method One of \code{"inverse"}, \code{"ts_adjusted"}, \code{"trend"} or \code{"as_is"}.
#' @param target Numeric, required for \code{"inverse"}. The metric level that maps to full
#'   exposure, in the metric's own units.
#' @param exponent Numeric, default 1. The aggressiveness of the \code{"inverse"} response.
#' @param window Positive whole number, required for \code{"ts_adjusted"}. Length of the trailing
#'   window, in observations. Dates without a full window are dropped.
#' @param center Numeric, default 1. The weight a neutral signal maps to under \code{"ts_adjusted"}
#'   and \code{"trend"}.
#' @param sensitivity Numeric, required for \code{"ts_adjusted"} and \code{"trend"}. Signed.
#' @param min_weight,max_weight Numeric bounds applied after mapping. Default \code{0} and \code{1}.
#' @param residual_asset Optional character naming an asset that absorbs the unallocated remainder,
#'   typically a risk-free-like sleeve. Its own metric, if present, is ignored.
#' @param weights_name Optional character naming the resulting object.
#' @param verbose Logical, default \code{TRUE}.
#'
#' @return A \code{weights_m_df} with columns \code{id}, \code{tickers}, \code{dates} and
#'   \code{weights}, summing to one on every date.
#'
#' @examples
#' \dontrun{
#'   # Tracking-error targeting: hold less of the risky sleeve when its tracking error runs hot,
#'   # never less than half of it, with cash taking the remainder.
#'   timing_weights <- derive_timing_weights(
#'     metric_m_df = rolling_tracking_error_m_df, metric = "ann_track_err",
#'     method = "inverse", target = 4,
#'     min_weight = 0.5, max_weight = 1, residual_asset = "cash"
#'   )
#'
#'   # Trend following on a trailing six-month return.
#'   trend_weights <- derive_timing_weights(
#'     metric_m_df = trailing_return_m_df, metric = "return_6m",
#'     method = "trend", center = 0.75, sensitivity = 0.25,
#'     residual_asset = "cash"
#'   )
#' }
#'
#' @seealso \code{\link{project_meta_weights_to_stocks}}, \code{\link{run_port_backtest}}
#' @export
derive_timing_weights <- function(metric_m_df,
                                  metric = NULL,
                                  method = c("inverse", "ts_adjusted", "trend", "as_is"),
                                  target = NULL,
                                  exponent = 1,
                                  window = NULL,
                                  center = 1,
                                  sensitivity = NULL,
                                  min_weight = 0,
                                  max_weight = 1,
                                  residual_asset = NULL,
                                  weights_name = NULL,
                                  verbose = TRUE) {

  #Validate inputs
  ####################
  method <- match.arg(method)

  if (methods::is(metric_m_df, "meta_dataframe")) metric_m_df <- metric_m_df@data
  if (!is.data.frame(metric_m_df) ||
      !all(c("id", "tickers", "dates") %in% names(metric_m_df))) {
    rlang::abort("metric_m_df must be a data.frame or meta_dataframe with id, tickers and dates.")
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
    rlang::abort(paste0("'", metric, "' must not contain NA. Decide how a missing metric should ",
                        "be treated before mapping it onto a weight."))
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
  check_single_number(min_weight, "min_weight")
  check_single_number(max_weight, "max_weight")
  if (min_weight > max_weight) {
    rlang::abort("min_weight must not exceed max_weight.")
  }
  if (min_weight < 0) {
    rlang::abort("min_weight must not be negative: these are long-only allocation weights.")
  }

  ##Method-specific requirements. The ones without defaults are the ones whose value is a claim
  ##about the world rather than a convenience.
  if (method == "inverse") {
    check_single_number(target, "target, which is required for method 'inverse'")
    check_single_number(exponent, "exponent")
    if (target <= 0) rlang::abort("target must be positive.")
    if (any(metric_m_df[[metric]] <= 0)) {
      rlang::abort(paste0("method 'inverse' expects a positive metric, since it stands for a risk ",
                          "measure, but '", metric, "' holds values at or below zero."))
    }
  }
  if (method %in% c("ts_adjusted", "trend")) {
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
  if (!is.null(residual_asset)) {
    if (!is.character(residual_asset) || length(residual_asset) != 1L) {
      rlang::abort("residual_asset must be a single character value.")
    }
    if (!residual_asset %in% metric_m_df$tickers) {
      rlang::abort(paste0("residual_asset '", residual_asset, "' is not among the tickers of ",
                          "metric_m_df."))
    }
  }
  ####################

  #Split the residual asset out
  ####################
  metric_m_df <- metric_m_df %>%
    dplyr::mutate(dates = as.Date(dates)) %>%
    dplyr::arrange(tickers, dates)

  ##The residual asset is a destination, not a decision: its own metric never enters the mapping
  timed_m_df <- if (is.null(residual_asset)) {
    metric_m_df
  } else {
    metric_m_df %>% dplyr::filter(tickers != residual_asset)
  }
  if (nrow(timed_m_df) == 0L) {
    rlang::abort("No asset is left to time once residual_asset is removed.")
  }
  ####################

  #Map the metric onto a raw weight
  ####################
  metric_values <- timed_m_df[[metric]]

  if (method == "inverse") {
    timed_m_df$raw_weight <- (target / metric_values)^exponent

  } else if (method == "trend") {
    timed_m_df$raw_weight <- center + sensitivity * sign(metric_values)

  } else if (method == "as_is") {
    timed_m_df$raw_weight <- metric_values

  } else {
    ##Z-score against the asset's own trailing history, ending at the date being weighted
    rolling_z_score <- function(values, window) {
      z <- rep(NA_real_, length(values))
      if (length(values) < window) return(z)
      for (i in seq(window, length(values))) {
        history <- values[(i - window + 1):i]
        history_sd <- stats::sd(history)
        ###A flat window carries no information about where the metric stands, so it is neutral
        z[i] <- if (is.na(history_sd) || history_sd == 0) 0 else
          (values[i] - mean(history)) / history_sd
      }
      z
    }

    timed_m_df <- timed_m_df %>%
      dplyr::group_by(tickers) %>%
      dplyr::mutate(raw_weight = center + sensitivity * rolling_z_score(.data[[metric]], window)) %>%
      dplyr::ungroup()

    incomplete_window <- sum(is.na(timed_m_df$raw_weight))
    if (incomplete_window > 0L) {
      ##Dropped rather than back-filled: a shorter window is a different estimator, and a
      ##back-filled one would borrow from dates that had not happened yet
      timed_m_df <- timed_m_df %>% dplyr::filter(!is.na(raw_weight))
      if (isTRUE(verbose)) {
        message(incomplete_window, " asset-date row(s) lacked a full ", window,
                "-observation window and were dropped.")
      }
    }
    if (nrow(timed_m_df) == 0L) {
      rlang::abort(paste0("No date has a full ", window, "-observation window; the metric history ",
                          "is shorter than the window."))
    }
  }

  #Box constraints
  timed_m_df$weights <- pmin(pmax(timed_m_df$raw_weight, min_weight), max_weight)
  ####################

  #Close the allocation
  ####################
  if (is.null(residual_asset)) {
    ##Without a residual asset the weights are relative, so they are rescaled to sum to one. The
    ##box therefore bounds the mapping output rather than the final weights.
    weights_m_df <- timed_m_df %>%
      dplyr::group_by(dates) %>%
      dplyr::mutate(total = sum(weights)) %>%
      dplyr::ungroup()

    if (any(weights_m_df$total <= 0)) {
      rlang::abort("Weights sum to zero or less on at least one date, so they cannot be rescaled.")
    }
    if (isTRUE(verbose) && (min_weight > 0 || max_weight < 1)) {
      message("No residual_asset was named, so weights are rescaled to sum to one and the ",
              "[", min_weight, ", ", max_weight, "] bounds apply to the mapping output rather ",
              "than to the final weights.")
    }

    weights_m_df <- weights_m_df %>%
      dplyr::mutate(weights = weights / total) %>%
      dplyr::select(id, tickers, dates, weights)

  } else {
    ##The residual asset takes whatever the timed assets leave, so the box holds exactly on them
    allocated <- timed_m_df %>%
      dplyr::group_by(dates) %>%
      dplyr::summarise(allocated = sum(weights), .groups = "drop")

    overallocated <- allocated %>% dplyr::filter(allocated > 1 + 1e-10)
    if (nrow(overallocated) > 0L) {
      rlang::abort(paste0("The timed assets claim more than the whole portfolio on ",
                          nrow(overallocated), " date(s), starting at ", overallocated$dates[1],
                          " (", round(overallocated$allocated[1], 4), "). Tighten max_weight."))
    }

    residual_rows <- allocated %>%
      dplyr::mutate(tickers = residual_asset,
                    weights = 1 - allocated,
                    id = paste0(residual_asset, "-", dates)) %>%
      dplyr::select(id, tickers, dates, weights)

    weights_m_df <- dplyr::bind_rows(
      timed_m_df %>% dplyr::select(id, tickers, dates, weights),
      residual_rows
    )
  }
  ####################

  #Final checks
  ####################
  weights_m_df <- weights_m_df %>%
    dplyr::arrange(id) %>%
    as.data.frame()
  rownames(weights_m_df) <- NULL

  date_sums <- weights_m_df %>%
    dplyr::group_by(dates) %>%
    dplyr::summarise(total = sum(weights), .groups = "drop") %>%
    dplyr::filter(abs(total - 1) > 1e-8)
  if (nrow(date_sums) > 0L) {
    rlang::abort(paste0("Timing weights do not sum to one on ", nrow(date_sums), " date(s)."))
  }
  if (any(weights_m_df$weights < 0 | weights_m_df$weights > 1)) {
    rlang::abort("Timing weights fall outside [0, 1].")
  }

  if (isTRUE(verbose)) {
    timed_assets <- setdiff(unique(weights_m_df$tickers), residual_asset)
    message("Derived timing weights by '", method, "' over ", length(timed_assets),
            " timed asset(s) and ", length(unique(weights_m_df$dates)), " date(s)",
            if (!is.null(residual_asset)) paste0(", with '", residual_asset,
                                                 "' taking the remainder") else "", ".")
  }
  ####################

  create_meta_dataframe(
    data = weights_m_df,
    meta_dataframe_name = if (is.null(weights_name)) paste0("timing_weights__", method) else weights_name,
    type = "weights"
  )
}
