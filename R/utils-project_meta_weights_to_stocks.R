#' Project Meta Portfolio Weights Down to Stocks
#'
#' Turns an allocation across base portfolios into the stock-level weights that allocation
#' implies, by multiplying each base portfolio's meta weight through its own end-of-period stock
#' weights and summing across portfolios.
#'
#' @details
#' For a stock \eqn{s} on date \eqn{t}, the projected weight is
#' \deqn{w(s, t) = \sum_p m(p, t) \times v(p, s, t)}
#' where \eqn{m(p, t)} is the meta weight of base portfolio \eqn{p} and \eqn{v(p, s, t)} is that
#' portfolio's own weight in \eqn{s}. Since each base portfolio's weights sum to one and the meta
#' weights sum to one, the projection sums to one as well.
#'
#' # Why project rather than allocate across portfolios directly
#'
#' Chiefly so that costs are charged on the trades that actually happen. Projecting first lets the
#' existing engine price each stock-level order against that stock's own liquidity and volatility,
#' and brings delisting and IPO handling along unchanged, none of which a portfolio-level run could
#' do without inventing liquidity and volatility for a portfolio.
#'
#' Offsetting trades are a second reason, but a smaller one than it first appears. Where two base
#' portfolios hold the same name and move it in opposite directions, the trades net at stock level,
#' and implied turnover is then at most the meta-weighted average of the base portfolios' own. That
#' inequality always holds when the meta weights are unchanged, but how much it bites depends
#' entirely on how differently the sleeves trade: measured on a toy cohort of two long-only sleeves
#' driven by different signals and rebalancing on the same dates, the saving ranged from nothing to
#' about two percent of turnover, and two sleeves driven by the same signal saved almost nothing.
#' Treat netting as a bonus that scales with genuine disagreement between sleeves, not as the
#' headline reason for this design.
#'
#' # Dates between meta rebalances
#'
#' Meta weights are set on the meta rebalance schedule. The backtest engine reads custom weights
#' only on its own rebalance dates, but \code{check_inputs_port_backtest()} requires a complete
#' panel that sums to one on \emph{every} date, so intervening dates are filled by holding the last
#' meta weights constant and re-projecting them onto that date's base weights. Those rows satisfy
#' the contract; they are not a claim about how the allocation drifts between rebalances.
#'
#' # Dates before the first meta weight
#'
#' The meta backtest's own buffer normally starts at or after the first meta rebalance, so earlier
#' dates are never consumed. They still have to be present and sum to one, and are filled with an
#' equal weight across the stocks quoted on that date. An equal weight is used deliberately rather
#' than the first projected vector, so no row carries information from a later date even though
#' nothing reads it.
#'
#' @param meta_weights_m_df A \code{data.frame} or \code{meta_dataframe} with columns \code{id},
#'   \code{tickers} and \code{dates}, plus a \code{weights} column. Its \code{tickers} are base
#'   backtest identifiers, not stocks.
#' @param port_backtest_cohort The \code{port_backtest_cohort} whose \code{port_weights_m_df}
#'   supplies each base portfolio's stock weights.
#' @param residual_ticker Optional character naming a meta-level asset that is a single stock-level
#'   holding rather than a portfolio, typically the residual sleeve of a risk-targeted allocation.
#'   Its meta weight becomes that ticker's stock weight directly, with no portfolio weights to
#'   multiply through. It must not be one of the cohort's base portfolios, and it must be a row of
#'   \code{signals_m_df}.
#' @param signals_m_df Optional \code{meta_dataframe} or \code{data.frame} of the stock signals the
#'   meta backtest will run on. When supplied, the result is extended to cover its full
#'   asset-by-date panel, which is what \code{check_inputs_port_backtest()} requires. It is needed
#'   rather than a plain vector of dates because the cohort's weights begin at its own buffer, so
#'   only this object knows which stocks were quoted on the earlier dates. Defaults to \code{NULL},
#'   covering just the dates the cohort spans.
#' @param tolerance Numeric, default \code{0.02}. How far a per-date weight sum may sit from one
#'   before the projection is refused.
#' @param verbose Logical, default \code{TRUE}.
#'
#' @return A \code{weights_m_df} with columns \code{id}, \code{tickers}, \code{dates} and
#'   \code{weights}, suitable as \code{custom_stock_weights_m_df}.
#'
#' @seealso \code{\link{derive_port_universe_m_df}}, \code{\link{create_port_backtest_cohort}}
#' @keywords internal
project_meta_weights_to_stocks <- function(meta_weights_m_df,
                                           port_backtest_cohort,
                                           signals_m_df = NULL,
                                           residual_ticker = NULL,
                                           tolerance = 0.02,
                                           verbose = TRUE) {

  #Validate inputs
  ####################
  if (!methods::is(port_backtest_cohort, "port_backtest_cohort")) {
    rlang::abort("port_backtest_cohort must be an object of class 'port_backtest_cohort'.")
  }
  if (methods::is(meta_weights_m_df, "meta_dataframe")) {
    meta_weights_m_df <- meta_weights_m_df@data
  }
  if (!is.data.frame(meta_weights_m_df)) {
    rlang::abort("meta_weights_m_df must be a data.frame or a meta_dataframe object.")
  }
  if (!all(c("tickers", "dates", "weights") %in% names(meta_weights_m_df))) {
    rlang::abort("meta_weights_m_df must contain 'tickers', 'dates' and 'weights' columns.")
  }
  if (any(is.na(meta_weights_m_df$weights))) {
    rlang::abort("meta_weights_m_df must not contain NA weights.")
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) || tolerance <= 0) {
    rlang::abort("tolerance must be a single positive number.")
  }

  meta_weights_m_df <- meta_weights_m_df %>%
    dplyr::mutate(dates = as.Date(dates)) %>%
    dplyr::arrange(dates, tickers)

  ##Meta weights must sum to one on each date they are set on
  meta_sums <- meta_weights_m_df %>%
    dplyr::group_by(dates) %>%
    dplyr::summarise(total = sum(weights), .groups = "drop")
  if (any(abs(meta_sums$total - 1) > tolerance)) {
    offending <- meta_sums$dates[abs(meta_sums$total - 1) > tolerance]
    rlang::abort(paste0("Meta weights do not sum to one on: ",
                        paste(as.character(offending), collapse = ", "), "."))
  }
  ####################

  #Split off the residual sleeve
  ####################
  ##A residual sleeve is a single holding rather than a portfolio, so its meta weight is already a
  ##stock weight and there is nothing to multiply through.
  residual_weights_m_df <- NULL
  if (!is.null(residual_ticker)) {
    if (!is.character(residual_ticker) || length(residual_ticker) != 1L) {
      rlang::abort("residual_ticker must be a single character value.")
    }
    if (residual_ticker %in% names(port_backtest_cohort@port_weights_m_df@data)) {
      rlang::abort(paste0("residual_ticker '", residual_ticker, "' is also a base portfolio of the ",
                          "cohort. A residual sleeve is a single holding, not a portfolio."))
    }
    if (!residual_ticker %in% meta_weights_m_df$tickers) {
      rlang::abort(paste0("residual_ticker '", residual_ticker, "' carries no meta weight."))
    }

    residual_weights_m_df <- meta_weights_m_df %>%
      dplyr::filter(tickers == residual_ticker) %>%
      dplyr::select(tickers, dates, weights) %>%
      dplyr::mutate(id = paste0(tickers, "-", dates)) %>%
      dplyr::select(id, tickers, dates, weights)

    meta_weights_m_df <- meta_weights_m_df %>% dplyr::filter(tickers != residual_ticker)
    if (nrow(meta_weights_m_df) == 0L) {
      rlang::abort("Every meta weight belongs to the residual sleeve, leaving no portfolio to project.")
    }
  }
  ####################

  #Base portfolio stock weights
  ####################
  base_weights <- port_backtest_cohort@port_weights_m_df@data
  portfolio_names <- sort(unique(meta_weights_m_df$tickers))

  missing_portfolios <- setdiff(portfolio_names, names(base_weights))
  if (length(missing_portfolios) > 0L) {
    rlang::abort(paste0("The cohort's port_weights_m_df has no column for: ",
                        paste(missing_portfolios, collapse = ", "),
                        ". Meta weights must be named by base backtest identifier."))
  }

  base_long <- base_weights %>%
    dplyr::select(id, tickers, dates, dplyr::all_of(portfolio_names)) %>%
    dplyr::mutate(dates = as.Date(dates)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(portfolio_names),
                        names_to = "portfolio", values_to = "base_weight")

  if (any(is.na(base_long$base_weight))) {
    rlang::abort("The cohort's port_weights_m_df contains NA weights.")
  }
  ##A long-short base portfolio cannot be projected into a custom_stock_weights_m_df, whose
  ##weights must lie in [0, 1]. create_port_backtest_cohort() already refuses such a cohort, so
  ##this is a guard against reaching here by another route rather than an expected condition.
  if (any(base_long$base_weight < 0)) {
    rlang::abort(paste0("The cohort holds negative base weights, which cannot be projected into ",
                        "a long-only stock weight panel."))
  }

  ##Each base portfolio must itself be fully invested on every date, or the projection inherits
  ##the shortfall and the result would not sum to one
  base_sums <- base_long %>%
    dplyr::group_by(dates, portfolio) %>%
    dplyr::summarise(total = sum(base_weight), .groups = "drop") %>%
    dplyr::filter(abs(total - 1) > tolerance)
  if (nrow(base_sums) > 0L) {
    rlang::abort(paste0("Base portfolio weights do not sum to one for ", nrow(base_sums),
                        " portfolio-date combinations, starting with '", base_sums$portfolio[1],
                        "' on ", base_sums$dates[1], " (sum ", round(base_sums$total[1], 4), ")."))
  }
  ####################

  #Hold meta weights forward across the base dates
  ####################
  base_dates <- sort(unique(base_long$dates))
  meta_dates <- sort(unique(meta_weights_m_df$dates))

  uncovered_meta_dates <- setdiff(meta_dates, base_dates)
  if (length(uncovered_meta_dates) > 0L) {
    rlang::abort(paste0("Meta weights are set on dates the cohort does not cover: ",
                        paste(as.character(as.Date(uncovered_meta_dates, origin = "1970-01-01")),
                              collapse = ", "), "."))
  }

  ##For each base date, the meta weights most recently set at or before it
  projectable_dates <- base_dates[base_dates >= min(meta_dates)]
  source_positions <- findInterval(projectable_dates, meta_dates)
  held_meta_weights <- purrr::map_dfr(seq_along(projectable_dates), function(i) {
    meta_weights_m_df %>%
      dplyr::filter(dates == meta_dates[source_positions[i]]) %>%
      dplyr::select(portfolio = tickers, meta_weight = weights) %>%
      dplyr::mutate(dates = projectable_dates[i])
  })
  ####################

  #Project onto stocks
  ####################
  projected <- base_long %>%
    dplyr::inner_join(held_meta_weights, by = c("portfolio", "dates")) %>%
    dplyr::group_by(id, tickers, dates) %>%
    dplyr::summarise(weights = sum(base_weight * meta_weight), .groups = "drop")

  ##The residual sleeve joins as its own stock-level row, held forward on the same schedule as
  ##the portfolio weights so the two halves stay in step
  if (!is.null(residual_weights_m_df)) {
    residual_held <- purrr::map_dfr(seq_along(projectable_dates), function(i) {
      residual_weights_m_df %>%
        dplyr::filter(dates == meta_dates[source_positions[i]]) %>%
        dplyr::mutate(dates = projectable_dates[i],
                      id = paste0(tickers, "-", dates))
    })
    ##The cohort's weight panel already carries a row for the residual ticker, normally at zero
    ##since the sleeves do not hold it, so the two contributions are summed rather than appended.
    ##That also stays correct in the case where a sleeve does hold it.
    projected <- dplyr::bind_rows(projected, residual_held) %>%
      dplyr::group_by(id, tickers, dates) %>%
      dplyr::summarise(weights = sum(weights), .groups = "drop")
  }
  ####################

  #Extend to the signals panel
  ####################
  if (!is.null(signals_m_df)) {
    if (methods::is(signals_m_df, "meta_dataframe")) signals_m_df <- signals_m_df@data
    if (!is.data.frame(signals_m_df) ||
        !all(c("id", "tickers", "dates") %in% names(signals_m_df))) {
      rlang::abort("signals_m_df must be a data.frame or meta_dataframe with id, tickers and dates.")
    }

    signal_keys <- signals_m_df %>%
      dplyr::select(id, tickers, dates) %>%
      dplyr::mutate(dates = as.Date(dates))
    dates_grid <- sort(unique(signal_keys$dates))

    ##Dates the projection already covers must match the signals panel asset for asset, or the
    ##weights would not sum to one over the assets the backtest actually walks
    projected_dates <- sort(unique(projected$dates))
    panel_mismatch <- signal_keys %>%
      dplyr::filter(dates %in% projected_dates) %>%
      dplyr::anti_join(projected, by = "id")
    if (nrow(panel_mismatch) > 0L) {
      rlang::abort(paste0(nrow(panel_mismatch), " signals_m_df id(s) have no projected weight, ",
                          "starting with '", panel_mismatch$id[1], "'. The cohort and signals_m_df ",
                          "must describe the same assets."))
    }

    ##The residual sleeve is a row of signals_m_df that the cohort never held, so it is expected
    ##to be absent from the base weights and must not count as a hole in the panel
    if (!is.null(residual_ticker)) {
      residual_in_signals <- signal_keys %>% dplyr::filter(tickers == residual_ticker)
      if (nrow(residual_in_signals) == 0L) {
        rlang::abort(paste0("residual_ticker '", residual_ticker, "' is not a row of signals_m_df. ",
                            "The residual sleeve has to be tradable in the stock universe, with ",
                            "its own return, liquidity and volatility."))
      }
    }

    early_dates <- dates_grid[dates_grid < min(meta_dates)]
    if (length(early_dates) > 0L) {
      ##Equal weights carry nothing from any later date. These rows exist only because the weight
      ##panel must be complete and sum to one; the engine's buffer means nothing reads them.
      early_rows <- signal_keys %>%
        dplyr::filter(dates %in% early_dates) %>%
        dplyr::group_by(dates) %>%
        dplyr::mutate(weights = 1 / dplyr::n()) %>%
        dplyr::ungroup()

      projected <- dplyr::bind_rows(projected, early_rows)

      if (isTRUE(verbose)) {
        message(length(early_dates), " date(s) precede the first meta weight and were filled with ",
                "equal weights. The backtest's buffer starts later, so these rows are never read.")
      }
    }
  } else {
    dates_grid <- base_dates
  }
  ####################

  #Final checks
  ####################
  projected <- projected %>%
    dplyr::filter(dates %in% dates_grid) %>%
    dplyr::arrange(id) %>%
    as.data.frame()
  rownames(projected) <- NULL

  uncovered_grid_dates <- setdiff(dates_grid, unique(projected$dates))
  if (length(uncovered_grid_dates) > 0L) {
    rlang::abort(paste0("The projection does not cover ", length(uncovered_grid_dates),
                        " date(s), starting at ",
                        min(as.Date(uncovered_grid_dates, origin = "1970-01-01")),
                        ". The cohort's weights do not reach that far back."))
  }

  ##The contract downstream is weights in [0, 1] summing to one on every date
  projected_sums <- projected %>%
    dplyr::group_by(dates) %>%
    dplyr::summarise(total = sum(weights), .groups = "drop") %>%
    dplyr::filter(abs(total - 1) > tolerance)
  if (nrow(projected_sums) > 0L) {
    rlang::abort(paste0("Projected stock weights do not sum to one on ", nrow(projected_sums),
                        " date(s), starting at ", projected_sums$dates[1], "."))
  }
  if (any(projected$weights < 0 | projected$weights > 1)) {
    rlang::abort("Projected stock weights fall outside [0, 1].")
  }

  if (isTRUE(verbose)) {
    held <- sum(projected$weights > 0)
    message("Projected ", length(portfolio_names), " base portfolios onto ",
            length(unique(projected$tickers)), " stocks over ", length(dates_grid),
            " dates; ", held, " of ", nrow(projected), " positions are non-zero.")
  }
  ####################

  create_meta_dataframe(
    data = projected,
    meta_dataframe_name = paste0(port_backtest_cohort@cohort_name, "__projected_weights"),
    type = "weights"
  )
}
