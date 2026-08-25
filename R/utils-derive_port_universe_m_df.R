#' Derive a Portfolio Universe from a Backtest Cohort
#'
#' Assembles a \code{\link{port_universe_m_df-class}} from a \code{\link{port_backtest_cohort-class}}:
#' one row per base portfolio per date, carrying the portfolio analytics, running cost figures and
#' aggregated custom metrics that could be used to set a meta-level allocation weight.
#'
#' @details
#' # What is joined
#'
#' \itemize{
#'   \item \strong{Portfolio statistics} come from each base result's \code{port_stats_m_df},
#'     filtered to the requested return basis. These exist only on base rebalance dates, so they
#'     are carried forward to intervening dates (see below).
#'   \item \strong{Costs} come from \code{port_costs_m_xts_list} as a running average, prefixed
#'     \code{avg_}.
#'   \item \strong{Custom metrics} come from \code{port_metrics_m_xts_list} at the row's date,
#'     prefixed \code{metric_}.
#'   \item \strong{User-supplied metrics} come from \code{custom_port_metrics_m_df}, joined on
#'     \code{id}, under whatever names the user gave them. This is the route for characteristics
#'     the cohort does not compute, or more timely versions of ones it does.
#' }
#'
#' # Carry-forward and staleness
#'
#' Base portfolio statistics are produced only on the base backtests' own rebalance dates, so a
#' meta rebalance date need not have a fresh value. The most recent available value is carried
#' forward and \code{stats_age_months} records how far. This is look-ahead safe, since a carried
#' value was formed strictly earlier, but it is not harmless: a tracking error or information ratio
#' formed six months ago may no longer describe the portfolio. A warning is raised whenever any
#' value is carried.
#'
#' # Point-in-time construction
#'
#' Every column on date \code{t} is knowable at \code{t}. Base statistics are computed at their own
#' rebalance date from returns realized up to that date and from positions held then. Cost averages
#' use observations \emph{strictly before} \code{t}: costs are stamped one day after the rebalance
#' they pay for, so this excludes the cost of the rebalance currently being decided rather than
#' assuming it is observable. Custom metrics are weight-aggregations of data at \code{t}.
#'
#' # Realized versus ex-ante statistics
#'
#' A base \code{port_stats_m_df} row splices two families whose names do not advertise the
#' difference. \code{track_err} and \code{info_ratio} are computed from a realized return series,
#' while \code{act_risk} and \code{IR} are computed from the portfolio's positions and a covariance
#' matrix at the formation date. Both are carried into this object under their original names;
#' \code{\link{message_meta_score_basis}} announces which kind a chosen meta score is, so the two
#' cannot be confused at selection time.
#'
#' Note also that the ex-ante figures are identical on the \code{raw} and \code{net} bases: that
#' block is joined to both rows of each base \code{port_stats_m_df} and is derived from positions,
#' not returns. Only the realized figures respond to \code{return_basis}.
#'
#' # Units
#'
#' Returns and the statistics derived from them are in percentage points (2.0 means 2 percent),
#' matching \code{\link{returns_meta_xts-class}}.
#'
#' @param port_backtest_cohort An object of class \code{port_backtest_cohort}.
#' @param return_basis Character, \code{"net"} (default) or \code{"raw"}. Selects which row of each
#'   base \code{port_stats_m_df} is used: net of transaction costs, or gross.
#' @param cost_lookback Optional positive integer. Number of trailing cost observations averaged
#'   into the \code{avg_} columns. \code{NULL} (default) uses an expanding average.
#' @param custom_port_metrics_m_df Optional \code{meta_dataframe} of user-computed per-portfolio
#'   metrics, joined on \code{id}. Its \code{tickers} must be the base backtest identifiers and it
#'   must carry only numeric, non-missing columns on a complete ticker-by-date panel. Column names
#'   may not collide with statistics already derived from the cohort. Dates it does not cover are
#'   left as \code{NA} rather than dropped.
#' @param port_universe_name Optional character naming the resulting object. Defaults to the cohort
#'   name suffixed with the return basis.
#' @param verbose Logical, default \code{TRUE}.
#'
#' @return An object of class \code{port_universe_m_df}, ordered by \code{id}, with \code{tickers}
#'   holding the base backtest identifiers.
#'
#' @seealso \code{\link{port_universe_m_df-class}}, \code{\link{create_port_backtest_cohort}},
#'   \code{\link{message_meta_score_basis}}
#' @export
derive_port_universe_m_df <- function(port_backtest_cohort,
                                      return_basis = c("net", "raw"),
                                      cost_lookback = NULL,
                                      custom_port_metrics_m_df = NULL,
                                      port_universe_name = NULL,
                                      verbose = TRUE) {

  #Validate inputs
  ####################
  if (!methods::is(port_backtest_cohort, "port_backtest_cohort")) {
    rlang::abort("port_backtest_cohort must be an object of class 'port_backtest_cohort'.")
  }
  return_basis <- match.arg(return_basis)

  if (!is.null(cost_lookback) &&
      (!is.numeric(cost_lookback) || length(cost_lookback) != 1L || is.na(cost_lookback) ||
       cost_lookback < 1 || cost_lookback %% 1 != 0)) {
    rlang::abort("cost_lookback must be NULL or a single positive whole number.")
  }

  ##Decision dates: the grid on which a meta allocation could be formed
  dates_grid <- port_backtest_cohort@backtest_workflow_common$dates_backtest
  if (is.null(dates_grid) || length(dates_grid) == 0L) {
    rlang::abort("port_backtest_cohort@backtest_workflow_common$dates_backtest is missing or empty.")
  }
  dates_grid <- sort(unique(as.Date(dates_grid)))

  results_list <- port_backtest_cohort@port_backtest_results_list
  if (length(results_list) == 0L) {
    rlang::abort("port_backtest_cohort carries no port_backtest_results to derive a universe from.")
  }
  if (length(results_list) < 2L) {
    rlang::warn(paste0("Only one base portfolio in the cohort. A meta allocation needs at least ",
                       "two; the resulting universe is derivable but not allocatable."))
  }
  ####################

  #Portfolio stats, tagged by backtest identifier
  ####################
  ##port_stats_m_df is already long-form, with one row per return basis per rebalance date, so the
  ##whole block is a filter and a retag rather than a reshape. Reading the per-result objects
  ##rather than the cohort's nested meta_xts list avoids un-pivoting them back into this shape.
  basis_row <- paste0(return_basis, "_return")

  stats_m_df <- purrr::map_dfr(results_list, function(x) {
    if (is.null(x@port_stats_m_df)) {
      rlang::abort(paste0("Backtest '", x@backtest_identifier,
                          "' carries no port_stats_m_df (an update without a rebalance?)."))
    }
    x@port_stats_m_df@data %>%
      dplyr::filter(tickers == basis_row) %>%
      dplyr::mutate(tickers = x@backtest_identifier) %>%
      dplyr::select(-id)
  })

  if (nrow(stats_m_df) == 0L) {
    rlang::abort(paste0("No '", basis_row, "' rows found in the cohort's port_stats_m_df objects."))
  }
  ####################

  #Carry forward onto the full date grid
  ####################
  ##source_dates survives the fill and is what makes staleness measurable
  port_universe_m_df <- stats_m_df %>%
    dplyr::mutate(source_dates = dates) %>%
    tidyr::complete(tickers = unique(stats_m_df$tickers), dates = dates_grid) %>%
    dplyr::filter(dates %in% dates_grid) %>%
    dplyr::arrange(tickers, dates) %>%
    dplyr::group_by(tickers) %>%
    tidyr::fill(dplyr::everything(), .direction = "down") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      ###Average month length, so a whole number of calendar months rounds cleanly
      stats_age_months = dplyr::if_else(
        is.na(source_dates),
        NA_real_,
        round(as.numeric(dates - source_dates) / 30.4375)
      )
    ) %>%
    dplyr::select(-source_dates)

  max_age <- suppressWarnings(max(port_universe_m_df$stats_age_months, na.rm = TRUE))
  if (is.finite(max_age) && max_age > 0) {
    rlang::warn(paste0(
      "Base portfolio statistics are produced only on base rebalance dates and have been carried ",
      "forward by up to ", max_age, " month(s). Stale values are most misleading for risk and ",
      "ratio figures such as track_err and info_ratio; see stats_age_months."
    ))
  }
  ####################

  #Running cost averages
  ####################
  cost_list <- port_backtest_cohort@port_costs_m_xts_list
  if (length(cost_list) > 0L) {
    costs_m_df <- purrr::map_dfr(names(cost_list), function(cost_slot) {
      cost_xts <- cost_list[[cost_slot]]@data
      purrr::map_dfr(dates_grid, function(current_date) {
        ###Strictly before: costs are stamped one day after the rebalance they pay for, so this
        ###drops the cost of the rebalance being decided now
        window_xts <- cost_xts[zoo::index(cost_xts) < current_date, , drop = FALSE]
        if (!is.null(cost_lookback) && nrow(window_xts) > cost_lookback) {
          window_xts <- utils::tail(window_xts, cost_lookback)
        }
        means <- if (nrow(window_xts) == 0L) {
          stats::setNames(rep(NA_real_, ncol(cost_xts)), colnames(cost_xts))
        } else {
          colMeans(window_xts, na.rm = TRUE)
        }
        data.frame(tickers = names(means),
                   dates = current_date,
                   stat = paste0("avg_", stringr::str_remove(cost_slot, "_m_xts$")),
                   value = unname(means),
                   stringsAsFactors = FALSE)
      })
    }) %>%
      tidyr::pivot_wider(names_from = "stat", values_from = "value")

    port_universe_m_df <- port_universe_m_df %>%
      dplyr::left_join(costs_m_df, by = c("tickers", "dates"))
  }
  ####################

  #Aggregated custom metrics
  ####################
  metric_list <- port_backtest_cohort@port_metrics_m_xts_list
  if (length(metric_list) > 0L) {
    metrics_m_df <- purrr::map_dfr(names(metric_list), function(metric_slot) {
      metric_xts <- metric_list[[metric_slot]]@data
      metric_xts <- metric_xts[zoo::index(metric_xts) %in% dates_grid, , drop = FALSE]
      ###as.numeric() unrolls an xts column-major, so tickers repeat by column and dates by row
      data.frame(tickers = rep(colnames(metric_xts), each = nrow(metric_xts)),
                 dates = rep(zoo::index(metric_xts), times = ncol(metric_xts)),
                 stat = paste0("metric_", stringr::str_remove(metric_slot, "_m_xts$")),
                 value = as.numeric(metric_xts),
                 stringsAsFactors = FALSE)
    }) %>%
      tidyr::pivot_wider(names_from = "stat", values_from = "value")

    port_universe_m_df <- port_universe_m_df %>%
      dplyr::left_join(metrics_m_df, by = c("tickers", "dates"))
  }
  ####################

  #User-supplied metrics
  ####################
  port_universe_m_df <- port_universe_m_df %>%
    dplyr::mutate(id = paste0(tickers, "-", dates))

  if (!is.null(custom_port_metrics_m_df)) {
    port_universe_m_df <- attach_custom_port_metrics(
      port_universe_m_df = port_universe_m_df,
      custom_port_metrics_m_df = custom_port_metrics_m_df,
      verbose = verbose
    )
  }
  ####################

  #Assemble
  ####################
  port_universe_m_df <- port_universe_m_df %>%
    dplyr::relocate(id, tickers, dates) %>%
    dplyr::relocate(stats_age_months, .after = dplyr::last_col()) %>%
    dplyr::arrange(id) %>%
    as.data.frame()
  rownames(port_universe_m_df) <- NULL

  ##Announce which statistics come in both an ex-ante and a realized flavour, so the pair is
  ##visible before a meta score is chosen from them
  if (isTRUE(verbose)) {
    ambiguous_present <- intersect(colnames(port_universe_m_df),
                                   meta_score_basis_table()$stat)
    if (length(ambiguous_present) > 0L) {
      message("This universe carries statistics in both ex-ante and realized flavours: ",
              paste(sort(ambiguous_present), collapse = ", "),
              ". Selecting one as the meta score will report which flavour it is.")
    }
  }

  if (is.null(port_universe_name)) {
    port_universe_name <- paste0(port_backtest_cohort@cohort_name, "__", return_basis)
  }

  create_meta_dataframe(
    data = port_universe_m_df,
    meta_dataframe_name = port_universe_name,
    type = "port_universe",
    port_metabacktest_workflow = list(
      source_cohort_name = port_backtest_cohort@cohort_name,
      return_basis = return_basis,
      cost_lookback = cost_lookback,
      custom_port_metrics_object_name = if (!is.null(custom_port_metrics_m_df)) {
        custom_port_metrics_m_df@meta_dataframe_name
      } else {
        NULL
      },
      custom_port_metrics = if (!is.null(custom_port_metrics_m_df)) {
        setdiff(names(custom_port_metrics_m_df@data), c("id", "tickers", "dates"))
      } else {
        NULL
      },
      selected_benchmark = port_backtest_cohort@backtest_workflow_common$selected_benchmark,
      base_portfolios = sort(unique(port_universe_m_df$tickers)),
      dates_covered = dates_grid,
      current_date = max(dates_grid)
    )
  )
}


#Helpers-----------------------------------------------

#' Attach user-supplied per-portfolio metrics to a port universe
#'
#' Joins a \code{meta_dataframe} of user-computed metrics onto the derived universe by \code{id},
#' mirroring how \code{\link{derive_signal_universe_m_df}} handles
#' \code{custom_signal_universe_metrics_m_df}. This is the route for characteristics the cohort
#' does not compute, or more timely versions of ones it does, for example a trailing-window
#' tracking error in place of the base backtests' expanding-window one.
#'
#' @details
#' The validation mirrors the signal-blending path: coercible to a \code{meta_dataframe}, numeric
#' columns only, no missing values, a complete ticker-by-date panel, and every base portfolio
#' covered.
#'
#' Two deliberate departures from that path:
#' \itemize{
#'   \item Rows are \strong{not} dropped when the supplied object covers fewer dates than the
#'     cohort. A port universe legitimately carries missing values (no realized statistics exist at
#'     the first rebalance date, no prior cost exists on the first decision date), so dropping
#'     incomplete rows would delete valid dates rather than clean the data. Uncovered rows are left
#'     as \code{NA} and reported.
#'   \item A supplied column whose name matches one already derived from the cohort is refused
#'     rather than joined. A silent join would rename both to \code{<name>.x} and \code{<name>.y},
#'     leaving the meta score to select an arbitrary one of the two.
#' }
#'
#' @param port_universe_m_df The universe assembled so far, carrying \code{id}, \code{tickers} and
#'   \code{dates}.
#' @param custom_port_metrics_m_df A \code{meta_dataframe} whose \code{tickers} are the base
#'   backtest identifiers.
#' @param verbose Logical, default \code{TRUE}.
#'
#' @return \code{port_universe_m_df} with the supplied metric columns joined on.
#' @keywords internal
attach_custom_port_metrics <- function(port_universe_m_df, custom_port_metrics_m_df,
                                       verbose = TRUE) {

  ##Class
  if (!methods::is(custom_port_metrics_m_df, "meta_dataframe")) {
    rlang::abort("custom_port_metrics_m_df must be a meta_dataframe object.")
  }
  custom_data <- custom_port_metrics_m_df@data

  ##Coercibility
  if (!is_coercible_to_meta_dataframe(custom_data)) {
    rlang::abort("custom_port_metrics_m_df not coercible to meta_dataframe.")
  }

  metric_cols <- setdiff(names(custom_data), c("id", "tickers", "dates"))
  if (length(metric_cols) == 0L) {
    rlang::abort("custom_port_metrics_m_df must carry at least one metric column.")
  }

  ##Only numeric
  if (!all(vapply(custom_data[metric_cols], is.numeric, logical(1)))) {
    rlang::abort("custom_port_metrics_m_df should only contain numeric values.")
  }

  ##No missing values in what the user supplied. Missingness introduced later by partial date
  ##coverage is tolerated and reported; missingness supplied outright is not.
  if (any(is.na(custom_data))) {
    rlang::abort("custom_port_metrics_m_df should not contain NA's.")
  }

  ##Complete panel
  expected_rows <- length(unique(custom_data$tickers)) * length(unique(custom_data$dates))
  if (nrow(custom_data) != expected_rows) {
    rlang::abort("custom_port_metrics_m_df should have nrows equal to tickers * dates.")
  }

  ##Every base portfolio covered
  missing_tickers <- setdiff(unique(port_universe_m_df$tickers), unique(custom_data$tickers))
  if (length(missing_tickers) > 0L) {
    rlang::abort(paste0("all port_universe_m_df tickers should be contemplated in ",
                        "custom_port_metrics_m_df. Missing: ",
                        paste(missing_tickers, collapse = ", "), "."))
  }

  ##Name collisions would become <name>.x and <name>.y, leaving the meta score to pick an
  ##arbitrary one of the two, so refuse instead of mangling
  collisions <- intersect(metric_cols, names(port_universe_m_df))
  if (length(collisions) > 0L) {
    rlang::abort(paste0("custom_port_metrics_m_df columns collide with statistics already derived ",
                        "from the cohort: ", paste(collisions, collapse = ", "),
                        ". Rename them so both remain available and distinguishable."))
  }

  ##Join on id
  port_universe_m_df <- port_universe_m_df %>%
    dplyr::left_join(custom_data %>% dplyr::select(-tickers, -dates), by = "id")

  ##Partial date coverage leaves NAs, which are reported rather than dropped
  if (isTRUE(verbose) &&
      any(vapply(port_universe_m_df[metric_cols], function(x) any(is.na(x)), logical(1)))) {
    message("custom_port_metrics_m_df does not cover every date in the cohort's backtest grid; ",
            "the uncovered rows carry NA.")
  }

  return(port_universe_m_df)
}


#' Ex-ante and realized counterparts among portfolio statistics
#'
#' Lookup table pairing the portfolio statistics that exist in two flavours. A base
#' \code{port_stats_m_df} stores both side by side under names that do not advertise the
#' difference: figures such as \code{track_err} and \code{info_ratio} are computed from a realized
#' return series, while \code{act_risk} and \code{IR} are computed from the portfolio's positions
#' and its covariance matrix at the formation date.
#'
#' @return A \code{data.frame} with columns \code{stat}, \code{basis} (\code{"ex-ante"} or
#'   \code{"realized"}), \code{counterpart} and \code{note}.
#' @keywords internal
meta_score_basis_table <- function() {

  ##Ex-ante: derived from positions and a covariance matrix at the formation date
  ex_ante <- data.frame(
    stat = c("exp_ret", "act_exp_ret", "risk", "act_risk", "SR", "IR"),
    counterpart = c("ann_ret", "act_ann_ret", "ann_std_dev", "ann_track_err",
                    "ann_sharpe_ratio", "ann_info_ratio"),
    stringsAsFactors = FALSE
  )
  ex_ante$basis <- "ex-ante"
  ex_ante$note <- "computed from the portfolio's positions and its covariance matrix at the formation date, not from any realized return"

  ##Expected-return figures are weighted exp_ret_score values, which are dimensionless
  ex_ante$note[ex_ante$stat %in% c("exp_ret", "act_exp_ret")] <- paste0(
    "computed from the portfolio's positions at the formation date as a weighted average of ",
    "exp_ret_score, which is a dimensionless cross-sectional score rather than a return in percent"
  )

  ##Realized: computed from the portfolio's realized return series up to the formation date
  realized <- data.frame(
    stat = c("arith_mean_ret", "geom_mean_ret", "ann_ret",
             "act_arith_mean_ret", "act_geom_mean_ret", "act_ann_ret",
             "std_dev", "ann_std_dev", "track_err", "ann_track_err",
             "sharpe_ratio", "ann_sharpe_ratio", "info_ratio", "ann_info_ratio"),
    counterpart = c("exp_ret", "exp_ret", "exp_ret",
                    "act_exp_ret", "act_exp_ret", "act_exp_ret",
                    "risk", "risk", "act_risk", "act_risk",
                    "SR", "SR", "IR", "IR"),
    stringsAsFactors = FALSE
  )
  realized$basis <- "realized"
  realized$note <- "computed from the portfolio's realized return series over the window ending at the formation date"

  rbind(ex_ante[c("stat", "basis", "counterpart", "note")],
        realized[c("stat", "basis", "counterpart", "note")])
}


#' Announce whether a chosen meta score is ex-ante or realized
#'
#' Emits a message naming the basis of a statistic selected as a meta-portfolio score, together
#' with its counterpart of the other basis. Mixing the two silently changes what a meta allocation
#' optimizes: an ex-ante information ratio expresses the conviction embedded in current positions,
#' while a realized one measures what the portfolio actually earned.
#'
#' @param stat_name Character. Name of the statistic chosen as the meta score.
#' @param verbose Logical, default \code{TRUE}. When \code{FALSE}, nothing is emitted.
#'
#' @return Invisibly, the basis (\code{"ex-ante"} or \code{"realized"}), or \code{NULL} when the
#'   statistic has no counterpart of the other basis.
#' @keywords internal
message_meta_score_basis <- function(stat_name, verbose = TRUE) {

  if (!is.character(stat_name) || length(stat_name) != 1L || is.na(stat_name)) {
    rlang::abort("stat_name must be a single non-missing character string.")
  }

  basis_table <- meta_score_basis_table()
  entry <- basis_table[basis_table$stat == stat_name, , drop = FALSE]

  ##Statistics without a counterpart of the other basis are unambiguous, so stay quiet
  if (nrow(entry) == 0L) {
    return(invisible(NULL))
  }

  if (isTRUE(verbose)) {
    message("Meta score '", stat_name, "' is the ", toupper(entry$basis[1]), " version: ",
            entry$note[1], ". Its ", if (entry$basis[1] == "ex-ante") "realized" else "ex-ante",
            " counterpart is '", entry$counterpart[1], "'.")
  }

  invisible(entry$basis[1])
}
