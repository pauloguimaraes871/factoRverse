#' Validate Meta Portfolio Backtest Inputs
#'
#' Checks a \code{\link{port_metabacktest_config-class}}, the
#' \code{\link{port_backtest_cohort-class}} it will allocate across, the
#' \code{\link{port_universe_m_df-class}} derived from that cohort, and the base data objects, both
#' individually and for mutual consistency.
#'
#' @details
#' # Why the universe is an argument rather than derived here
#'
#' The universe-dependent checks are the substantive ones: whether the meta score names a real
#' column, whether it is populated, and how stale the statistics behind it are. Deriving the
#' universe inside this function would duplicate work the caller has to do anyway, so it is passed
#' in. \code{\link{derive_port_universe_m_df}} carries its own validation of the cohort, and runs
#' first; this function assumes only that it returned successfully.
#'
#' # What is checked
#'
#' \itemize{
#'   \item classes of every supplied object;
#'   \item the cohort holds at least two base portfolios, since a meta allocation needs something
#'     to allocate across. Exactly two is supported: signal weighting over a pair is ordinal rather
#'     than proportional, which is reported as a warning and not refused, since a two-portfolio
#'     allocation such as a risky and a defensive sleeve is a normal use;
#'   \item the meta score names a column of the universe, is not entirely missing, and is present
#'     at every meta rebalance date;
#'   \item the meta rebalance schedule falls inside the dates the cohort covers, and the meta
#'     buffer clears the base one;
#'   \item the benchmark agrees between config and cohort;
#'   \item every base data object is the one the cohort was built from, matched by name the same
#'     way \code{\link{extract_returns_m_xts}} matches them.
#' }
#'
#' # Statistics staleness never blocks
#'
#' Base portfolio statistics exist only on base rebalance dates, so a meta rebalance date may be
#' using figures formed months earlier. That is look-ahead safe and sometimes unavoidable, and the
#' right tolerance depends on how often the base portfolios rebalance, so it is reported as a
#' warning and never as an error. Supplying \code{max_stats_age_months} sets the threshold above
#' which the warning fires; leaving it \code{NULL} reports the observed maximum without judging it.
#'
#' @param config A \code{port_metabacktest_config}.
#' @param port_backtest_cohort The \code{port_backtest_cohort} holding the base portfolios.
#' @param port_universe_m_df The \code{port_universe_m_df} derived from that cohort.
#' @param signals_m_df,fwd_return_m_df,liquidity_m_df,volatility_m_df The base data objects, which
#'   must be the ones the cohort's backtests were run on.
#' @param benchmark_weights_m_df,benchmark_returns_m_xts Optional benchmark objects.
#' @param daily_stock_returns_m_xts,daily_bench_returns_m_xts Optional daily return objects.
#' @param stock_groups_m_df Optional groups object.
#' @param max_stats_age_months Optional positive whole number. Age above which carried statistics
#'   raise a warning at meta rebalance dates. \code{NULL} (default) reports without a threshold.
#' @param verbose Logical, default \code{TRUE}.
#'
#' @return Invisibly, a list with the resolved \code{meta_rebalance_dates} and the observed
#'   \code{max_stats_age_months}. Called for its validation side effects.
#'
#' @seealso \code{\link{derive_port_universe_m_df}}, \code{\link{create_port_metabacktest_config}}
#' @keywords internal
check_inputs_meta_port_backtest <- function(config,
                                            port_backtest_cohort,
                                            port_universe_m_df,
                                            signals_m_df, fwd_return_m_df,
                                            liquidity_m_df, volatility_m_df,
                                            benchmark_weights_m_df = NULL,
                                            benchmark_returns_m_xts = NULL,
                                            daily_stock_returns_m_xts = NULL,
                                            daily_bench_returns_m_xts = NULL,
                                            stock_groups_m_df = NULL,
                                            max_stats_age_months = NULL,
                                            verbose = TRUE) {

  #Classes
  ####################
  if (!methods::is(config, "port_metabacktest_config")) {
    rlang::abort("config must be an object of class 'port_metabacktest_config'.")
  }
  if (!methods::is(port_backtest_cohort, "port_backtest_cohort")) {
    rlang::abort("port_backtest_cohort must be an object of class 'port_backtest_cohort'.")
  }
  if (!methods::is(port_universe_m_df, "port_universe_m_df")) {
    rlang::abort("port_universe_m_df must be an object of class 'port_universe_m_df'.")
  }

  ##Mandatory base data objects
  mandatory_m_dfs <- list(signals_m_df = signals_m_df, fwd_return_m_df = fwd_return_m_df,
                          liquidity_m_df = liquidity_m_df, volatility_m_df = volatility_m_df)
  for (object_name in names(mandatory_m_dfs)) {
    if (!is_meta_dataframe(mandatory_m_dfs[[object_name]])) {
      rlang::abort(paste0(object_name, " must be a meta_dataframe object."))
    }
  }

  ##Optional objects, checked only when supplied
  optional_m_dfs <- list(benchmark_weights_m_df = benchmark_weights_m_df,
                         stock_groups_m_df = stock_groups_m_df)
  for (object_name in names(optional_m_dfs)) {
    if (!is.null(optional_m_dfs[[object_name]]) && !is_meta_dataframe(optional_m_dfs[[object_name]])) {
      rlang::abort(paste0("If provided, ", object_name, " must be a meta_dataframe object."))
    }
  }

  optional_m_xts <- list(benchmark_returns_m_xts = benchmark_returns_m_xts,
                         daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                         daily_bench_returns_m_xts = daily_bench_returns_m_xts)
  for (object_name in names(optional_m_xts)) {
    if (!is.null(optional_m_xts[[object_name]]) &&
        !inherits(optional_m_xts[[object_name]], "meta_xts")) {
      rlang::abort(paste0("If provided, ", object_name, " must be a meta_xts object."))
    }
  }

  if (!is.null(max_stats_age_months) &&
      (!is.numeric(max_stats_age_months) || length(max_stats_age_months) != 1L ||
       is.na(max_stats_age_months) || max_stats_age_months < 0 ||
       max_stats_age_months %% 1 != 0)) {
    rlang::abort("max_stats_age_months must be NULL or a single non-negative whole number.")
  }
  ####################

  #Completeness of the configuration
  ####################
  ##risk_target_parameters is allowed to be absent at construction so a configuration can be built and
  ##then completed with add_risk_target_parameters(). Nothing can run without it, so this is where the
  ##requirement bites.
  if (config@type == "risk_targeted" && is.null(config@risk_target_parameters)) {
    rlang::abort(paste0("This configuration has type 'risk_targeted' but carries no ",
                        "risk_target_parameters. Add them with add_risk_target_parameters() or pass them to ",
                        "create_port_metabacktest_config()."))
  }
  ####################

  #Cohort size
  ####################
  base_portfolios <- sort(unique(port_universe_m_df@data$tickers))
  n_base_portfolios <- length(base_portfolios)
  port_construction_method <- config@meta_port_backtest_config@port_construction_method
  is_risk_targeted <- config@type == "risk_targeted"

  ##A risk-targeted allocation scales a sleeve against a residual, so one base portfolio is the
  ##normal case. A multi-portfolio allocation needs something to allocate across.
  if (!is_risk_targeted && n_base_portfolios < 2L) {
    rlang::abort(paste0("A meta allocation needs at least two base portfolios; the cohort holds ",
                        n_base_portfolios, "."))
  }
  if (is_risk_targeted && n_base_portfolios != 1L) {
    rlang::abort(paste0("A risk-targeted allocation scales exactly one risky sleeve against the ",
                        "residual, but the cohort holds ", n_base_portfolios, " base portfolios. ",
                        "Combine them into a single sleeve with a 'multi_port' meta backtest first, ",
                        "then scale its result."))
  }

  ##signal_transform() winsorizes then z-scores the cross-section, and a two-element cross-section
  ##always z-scores to the same pair of values whatever the gap between the scores. Signal
  ##weighting over two portfolios is therefore ordinal: it tilts toward whichever portfolio scores
  ##higher by a fixed amount, and flips when the ranking flips. That is a coherent tactical rule
  ##for a pair such as a risky and a defensive portfolio, so it is reported rather than refused.
  is_two_portfolio_sw <- n_base_portfolios == 2L && port_construction_method == "sw"

  if (is_two_portfolio_sw) {
    rlang::warn(paste0(
      "port_construction_method 'sw' over exactly two base portfolios allocates on the ordering of ",
      "the meta score, not its magnitude. derive_stock_universe_m_d_ref() runs signal_transform() ",
      "before any weighting, and a two-element cross-section always z-scores to the same pair of ",
      "values, so the weights are a fixed 74.5/25.5 split toward whichever portfolio scores higher, ",
      "and 50/50 on a tie, whatever the gap between them. Every method that reads exp_ret_score is ",
      "affected the same way at two assets, 'mvo' included, since the transform runs before all of ",
      "them. That is a valid rule if an ordinal tilt is what you want; for weights proportional to ",
      "the size of the gap over a pair, compute them outside and supply them as custom weights."))
  }

  ##The generic small-cohort caveat is about sensitivity to small changes in the statistics, which
  ##is the opposite of what the two-portfolio 'sw' case does, so it is not raised alongside it
  ##and it does not apply to the risk-targeted path either, where a single sleeve is the required
  ##shape and there is no cross-section to rank at all
  if (n_base_portfolios < 4L && !is_two_portfolio_sw && !is_risk_targeted) {
    rlang::warn(paste0("The cohort holds only ", n_base_portfolios, " base portfolios. Scores are ",
                       "ranked cross-sectionally, so a cross-section this small makes the meta ",
                       "weights sensitive to small changes in the underlying statistics."))
  }

  ##A covariance matrix estimated from fewer observations than assets is singular, and one
  ##estimated from barely more is close to it. Portfolio analytics are computed from a covariance
  ##matrix whatever the construction method, so this is not limited to rp, hrp and mvo.
  cov_matrix_sample_size <- config@meta_port_backtest_config@cov_est_method@cov_matrix_sample_size
  if (cov_matrix_sample_size <= n_base_portfolios) {
    rlang::warn(paste0("cov_matrix_sample_size is ", cov_matrix_sample_size, " month(s) for ",
                       n_base_portfolios, " base portfolios, so the estimated covariance matrix is ",
                       "singular. Risk and every figure derived from it will be unreliable or ",
                       "undefined; allow at least a few times as many months as portfolios."))
  }
  ####################

  #Benchmark agreement
  ####################
  cohort_benchmark <- port_backtest_cohort@backtest_workflow_common$selected_benchmark
  config_benchmark <- config@meta_port_backtest_config@selected_benchmark

  if (!identical(config_benchmark, cohort_benchmark)) {
    rlang::abort(paste0("selected_benchmark differs between the meta config (",
                        if (is.null(config_benchmark)) "NULL" else config_benchmark,
                        ") and the cohort (",
                        if (is.null(cohort_benchmark)) "NULL" else cohort_benchmark, ")."))
  }
  ####################

  #Meta rebalance schedule
  ####################
  ##Resolved the same way run_port_backtest_internal() resolves it, so the dates checked here are
  ##the dates the backtest will actually rebalance on
  dates_m_vector <- sort(unique(as.Date(dplyr::pull(signals_m_df@data, dates))))
  initial_buffer_period <- config@meta_port_backtest_config@initial_buffer_period
  rebalancing_months <- config@meta_port_backtest_config@rebalancing_months

  if (initial_buffer_period > length(dates_m_vector)) {
    rlang::abort(paste0("initial_buffer_period (", initial_buffer_period, ") exceeds the number of ",
                        "dates in signals_m_df (", length(dates_m_vector), ")."))
  }

  dates_backtest <- dates_m_vector[initial_buffer_period:length(dates_m_vector)]
  meta_rebalance_dates <- sort(unique(c(
    min(dates_backtest),
    dates_backtest[lubridate::month(dates_backtest) %in% rebalancing_months]
  )))

  ##The meta buffer must clear the base one: before it, the cohort has no portfolio at all
  base_initial_buffer_period <- port_backtest_cohort@backtest_workflow_common$initial_buffer_period
  if (!is.null(base_initial_buffer_period) && initial_buffer_period < base_initial_buffer_period) {
    rlang::abort(paste0("initial_buffer_period (", initial_buffer_period, ") is shorter than the ",
                        "base backtests' own (", base_initial_buffer_period, "). The cohort ",
                        "produces no portfolio before its own buffer elapses."))
  }

  universe_dates <- sort(unique(port_universe_m_df@data$dates))
  uncovered_dates <- setdiff(meta_rebalance_dates, universe_dates)
  if (length(uncovered_dates) > 0L) {
    rlang::abort(paste0("The meta rebalance schedule includes dates the cohort does not cover: ",
                        paste(as.character(as.Date(uncovered_dates, origin = "1970-01-01")),
                              collapse = ", "), "."))
  }
  ####################

  #The residual sleeve, on the risk-targeted path
  ####################
  if (is_risk_targeted) {
    risk_target_params <- config@risk_target_parameters
    residual_ticker <- risk_target_params@residual_ticker

    ##The residual has to be tradable in the stock universe: it is held like any other position,
    ##so the engine prices its trades against its own liquidity and volatility
    signal_tickers <- unique(dplyr::pull(signals_m_df@data, tickers))
    if (!residual_ticker %in% signal_tickers) {
      rlang::abort(paste0("residual_ticker '", residual_ticker, "' is not a row of signals_m_df. ",
                          "It has to be tradable in the stock universe, with its own return, ",
                          "liquidity and volatility rows, so the engine can price its trades."))
    }
    for (object_name in c("fwd_return_m_df", "liquidity_m_df", "volatility_m_df")) {
      object_tickers <- unique(dplyr::pull(mandatory_m_dfs[[object_name]]@data, tickers))
      if (!residual_ticker %in% object_tickers) {
        rlang::abort(paste0("residual_ticker '", residual_ticker, "' is missing from ",
                            object_name, "."))
      }
    }
    ##Optional objects still have to cover it when they are supplied, since the engine treats the
    ##residual as an ordinary holding: groups in particular are used to fill missing returns
    for (object_name in names(optional_m_dfs)) {
      optional_object <- optional_m_dfs[[object_name]]
      if (!is.null(optional_object) &&
          !residual_ticker %in% unique(dplyr::pull(optional_object@data, tickers))) {
        rlang::abort(paste0("residual_ticker '", residual_ticker, "' is missing from ",
                            object_name, ". Every stock-level object the backtest receives has to ",
                            "cover it, because it is held and traded like any other position."))
      }
    }

    ##A residual that is also a base portfolio would be double counted
    if (residual_ticker %in% base_portfolios) {
      rlang::abort(paste0("residual_ticker '", residual_ticker, "' is also a base portfolio of ",
                          "the cohort."))
    }

    ##The ex-ante estimator has nothing to work from without daily returns
    if (risk_target_params@vol_source == "ex_ante" && is.null(daily_stock_returns_m_xts)) {
      rlang::abort(paste0("vol_source is 'ex_ante', which estimates risk from a short window of ",
                        "daily stock returns, so daily_stock_returns_m_xts must be supplied."))
    }

    ##The residual has to match the target metric, and only the data can say whether it does. An
    ##index-tracking residual makes tracking error scale linearly toward zero as the sleeve is
    ##cut. A residual that does not track has a tracking error of its own that the blend can
    ##never get below, so the weight pins at a bound and the rule moves risk the wrong way. The
    ##pathological case is a constant-return line: its tracking error equals the benchmark's own
    ##volatility, the furthest any asset can sit from the index.
    if (risk_target_params@target_metric == "tracking_error" && !is.null(benchmark_returns_m_xts)) {

      residual_forward <- fwd_return_m_df@data %>%
        dplyr::filter(tickers == residual_ticker) %>%
        dplyr::arrange(dates)
      benchmark_xts <- benchmark_returns_m_xts@data

      if (nrow(residual_forward) > 2L && cohort_benchmark %in% colnames(benchmark_xts)) {

        ###fwd_return_1m at t is the return from t to t+1, and the engine reads the benchmark
        ###return at t+1 to form the active return, so the two are compared on that alignment
        own_dates <- residual_forward$dates
        benchmark_by_date <- stats::setNames(
          as.numeric(benchmark_xts[, cohort_benchmark]),
          as.character(zoo::index(benchmark_xts)))
        next_benchmark <- vapply(own_dates, function(current) {
          later <- own_dates[own_dates > current]
          if (length(later) == 0L) return(NA_real_)
          key <- as.character(min(later))
          if (!key %in% names(benchmark_by_date)) return(NA_real_)
          unname(benchmark_by_date[[key]])
        }, numeric(1))

        tracking_difference <- residual_forward$fwd_return_1m - next_benchmark
        usable <- sum(is.finite(tracking_difference))

        if (usable > 2L) {
          ###Both annualised the same way, so the ratio is what matters rather than either level
          residual_tracking_error <- stats::sd(tracking_difference, na.rm = TRUE) * sqrt(12)
          benchmark_volatility <- stats::sd(as.numeric(benchmark_xts[, cohort_benchmark]),
                                            na.rm = TRUE) * sqrt(12)

          ###A replication tracks to a small fraction of the index's own volatility. Half of it is
          ###far beyond anything a tracker produces and still well short of the constant-return
          ###case, which sits at the whole of it.
          if (is.finite(residual_tracking_error) && is.finite(benchmark_volatility) &&
              benchmark_volatility > 0 &&
              residual_tracking_error > 0.5 * benchmark_volatility) {
            rlang::warn(paste0(
              "'", residual_ticker, "' has a realised tracking error of ",
              round(residual_tracking_error, 2), " against '", cohort_benchmark,
              "', whose own volatility is ", round(benchmark_volatility, 2),
              ". A 'tracking_error' target assumes the residual tracks the benchmark, so that ",
              "blending toward it drives tracking error to zero. This one does not track: ",
              "blending toward it will raise tracking error instead, and the weight will pin at ",
              "a bound. Use an index-replicating residual, or set target_metric = 'volatility' ",
              "if the residual is meant to be riskless."))
          }
        } else {
          rlang::warn(paste0(
            "Not enough overlapping observations to check whether '", residual_ticker,
            "' tracks '", cohort_benchmark, "'. A 'tracking_error' target assumes it does."))
        }
      } else {
        rlang::warn(paste0(
          "Could not read a forward return series for '", residual_ticker, "' or a '",
          cohort_benchmark, "' column, so whether the residual tracks the benchmark was not ",
          "checked. A 'tracking_error' target assumes it does."))
      }
    }
  }
  ####################

  #Base data objects match the cohort
  ####################
  ##Ahead of the type branch, because both paths execute against these objects. The
  ##risk-targeted route used to return before reaching this, so a cohort built from one set of
  ##inputs could be run against another without complaint.
  ##Matched by name, the same way extract_returns_m_xts() checks a cohort against its inputs
  ##Only the objects both levels must share. Stock groups and the two daily return series are
  ##optional extras the meta level may legitimately need and the base portfolios may never have
  ##seen: the ex-ante risk estimate reads daily returns whether or not the sleeve was run with
  ##them, so a difference there is not a disagreement about the data being backtested.
  workflow_common <- port_backtest_cohort@backtest_workflow_common

  expected_names <- list(
    signals_object_name = if (!is.null(signals_m_df)) signals_m_df@meta_dataframe_name else NULL,
    fwd_return_object_name = if (!is.null(fwd_return_m_df)) fwd_return_m_df@meta_dataframe_name else NULL,
    liquidity_object_name = if (!is.null(liquidity_m_df)) liquidity_m_df@meta_dataframe_name else NULL,
    volatility_object_name = if (!is.null(volatility_m_df)) volatility_m_df@meta_dataframe_name else NULL,
    benchmark_weights_object_name = if (!is.null(benchmark_weights_m_df)) benchmark_weights_m_df@meta_dataframe_name else NULL,
    benchmark_returns_object_name = if (!is.null(benchmark_returns_m_xts)) benchmark_returns_m_xts@meta_xts_name else NULL
  )

  for (workflow_field in names(expected_names)) {
    supplied_name <- expected_names[[workflow_field]]
    if (is.null(supplied_name)) next

    cohort_name <- workflow_common[[workflow_field]]
    if (is.null(cohort_name)) next

    if (!identical(supplied_name, cohort_name)) {
      rlang::abort(paste0("Object name mismatch for ", workflow_field, ": the cohort was built ",
                          "with '", cohort_name, "' but '", supplied_name, "' was supplied. The ",
                          "meta backtest must run on the same data as its base portfolios."))
    }
  }
  ####################

  #Meta score
  ####################
  ##A risk-targeted allocation derives its weight from the targeting rule, so there is no score
  if (is_risk_targeted) {
    if (isTRUE(verbose)) {
      message("Meta backtest inputs validated: risk-targeted allocation of '",
              paste(base_portfolios, collapse = ", "), "' against '",
              config@risk_target_parameters@residual_ticker, "' over ",
              length(meta_rebalance_dates), " meta rebalance dates.")
    }
    return(invisible(list(meta_rebalance_dates = meta_rebalance_dates,
                          max_stats_age_months = NA_real_)))
  }

  meta_score <- names(config@meta_port_backtest_config@chosen_score_metric_and_position)
  universe_data <- port_universe_m_df@data

  if (!meta_score %in% names(universe_data)) {
    rlang::abort(paste0("The meta score '", meta_score, "' is not a column of the derived ",
                        "port_universe_m_df. Available columns: ",
                        paste(setdiff(names(universe_data), c("id", "tickers", "dates")),
                              collapse = ", "), "."))
  }

  meta_score_values <- universe_data[[meta_score]]

  ##A statistic can be structurally present but never computed. The position-derived block
  ##(act_risk, IR and the rest) is entirely missing whenever the base backtests ran without
  ##daily_stock_returns_m_xts, since there is then no covariance matrix to derive it from.
  if (all(is.na(meta_score_values))) {
    rlang::abort(paste0("The meta score '", meta_score, "' is missing at every date. If it is a ",
                        "position-derived figure such as 'act_risk' or 'IR', the base backtests ",
                        "were most likely run without daily_stock_returns_m_xts, leaving no ",
                        "covariance matrix to compute it from."))
  }

  ##Only the rebalance dates matter: the score is read when weights are set, nowhere else
  rebalance_rows <- universe_data[universe_data$dates %in% meta_rebalance_dates, , drop = FALSE]
  missing_rows <- rebalance_rows[is.na(rebalance_rows[[meta_score]]), , drop = FALSE]

  if (nrow(missing_rows) > 0L) {
    rlang::abort(paste0("The meta score '", meta_score, "' is missing at ", nrow(missing_rows),
                        " portfolio-date combinations on the meta rebalance schedule, starting at ",
                        min(missing_rows$dates), ". Realized statistics are undefined until the ",
                        "base portfolios have a return history, so consider a larger ",
                        "initial_buffer_period."))
  }

  ##Report which flavour of the statistic was chosen, since ex-ante and realized versions of the
  ##same idea sit side by side in this object
  if (isTRUE(verbose)) {
    message_meta_score_basis(stat_name = meta_score, verbose = TRUE)
  }
  ####################

  #Statistics staleness (warning only)
  ####################
  observed_max_age <- suppressWarnings(max(rebalance_rows$stats_age_months, na.rm = TRUE))
  if (!is.finite(observed_max_age)) observed_max_age <- NA_real_

  if (!is.na(observed_max_age) && observed_max_age > 0) {
    if (is.null(max_stats_age_months)) {
      if (isTRUE(verbose)) {
        message("Base statistics on the meta rebalance schedule are up to ", observed_max_age,
                " month(s) old, because they are produced only on base rebalance dates.")
      }
    } else if (observed_max_age > max_stats_age_months) {
      rlang::warn(paste0("Base statistics on the meta rebalance schedule are up to ",
                         observed_max_age, " month(s) old, above the ", max_stats_age_months,
                         "-month tolerance. This is look-ahead safe but the figures may no longer ",
                         "describe the portfolios; align the meta rebalancing_months with the base ",
                         "ones to remove it."))
    }
  }
  ####################

  if (isTRUE(verbose)) {
    message("Meta backtest inputs validated: ", n_base_portfolios, " base portfolios over ",
            length(meta_rebalance_dates), " meta rebalance dates, scored on '", meta_score, "'.")
  }

  invisible(list(meta_rebalance_dates = meta_rebalance_dates,
                 max_stats_age_months = observed_max_age))
}
