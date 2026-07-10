# meta_dataframe--------------------------------------------------------

#' Create a meta_dataframe Object
#'
#' @title Create a meta_dataframe
#' @description
#' Generic constructor that creates an S4 \code{meta_dataframe} or an appropriate subclass from supplied input.
#' Dispatches on the class of \code{data} and implements specialized behavior for \code{data.frame} and structured
#' wide-panel \code{list} inputs.
#'
#' @param data Input data. See method-specific docs for required shapes:
#'   - \code{data.frame} method: long-format frame with first three columns \code{id}, \code{tickers}, \code{dates}.
#'   - \code{list} method: named list of matrices/data.frames/tibbles (wide features).
#' @param meta_dataframe_name Character. Short identifier for the resulting object (default: \code{"not_identified"}).
#' @param workflow Optional list. Workflow entries describing how the data was produced (stored in the \code{workflow} slot).
#' @param ss_backtest_workflow Optional list. Required when creating \code{signal_universe} objects.
#' @param sb_backtest_workflow Optional list. Required when creating \code{oos_sb_outputs} objects.
#' @param port_backtest_workflow Optional list. Required when creating \code{stock_universe} objects.
#' @param type Character. When using the \code{data.frame} method this selects the subclass to instantiate.
#'   Accepted values include \code{"generic"}, \code{"signals"}, \code{"features"}, \code{"signal_universe"},
#'   \code{"stock_universe"}, \code{"oos_sb_outputs"}, \code{"groups"}, \code{"target"},
#'   \code{"weights"}, \code{"priors"}, \code{"feature_importance"}, \code{"raw"}.
#' @param tickers Character vector. (list-method) entity identifiers for rows of wide input.
#' @param dates Date vector. (list-method) column identifiers / time points for wide input.
#' @param features_names Character vector. (list-method) names assigned to each element of \code{data}.
#' @param data_format Character. (list-method) currently only \code{"wide"} is supported.
#' @param tickers_on Character. (list-method) currently only \code{"rows"} is supported.
#' @param ... Additional arguments forwarded to specific methods.
#'
#' @return An S4 object of class \code{meta_dataframe} or an appropriate subclass. Populated slots:
#'   \code{data}, \code{signals}, \code{unique_dates}, \code{unique_tickers}, \code{n_obs},
#'   \code{current_date}, \code{meta_dataframe_name}, and optional workflow/backtest slots.
#'
#' @details
#' Prefer this high-level constructor to calling \code{new(...)} directly because it performs structural
#' validation and populates metadata consistently. The generic only dispatches — see the method-specific
#' documentation for exact validation rules and error conditions (a number of tests assert these validations).
#'
#' @examples
#' \dontrun{
#' # See the data.frame and list method examples below.
#' }
#'
#' @seealso \code{\link{meta_dataframe-class}}, \code{\link{create_meta_xts}}, \code{\link{create_tickers_catalog}}
#'
#' @name create_meta_dataframe
#' @rdname create_meta_dataframe
#' @export

setGeneric("create_meta_dataframe", function(data, meta_dataframe_name = "not_identified", ...) {
  standardGeneric("create_meta_dataframe")
})



#' Create a meta_dataframe from a data.frame
#'
#' @describeIn create_meta_dataframe Create a \code{meta_dataframe} (or subclass) from a long-format \code{data.frame}.
#'
#' @param data A \code{data.frame} whose first three columns must be exactly \code{id}, \code{tickers}, \code{dates}.
#'   - \code{id} must equal \code{paste0(tickers, "-", dates)}.
#'   - \code{dates} must be class \code{Date} and ordered ascending.
#'   - \code{tickers} must be a \code{character} vector.
#'   - Remaining columns are interpreted as signals/features and populate the \code{signals} slot.
#' @param meta_dataframe_name Character. Name for the resulting object.
#' @param workflow Optional list. Workflow entries stored in the \code{workflow} slot.
#' @param ss_backtest_workflow Optional list. Required when \code{type = "signal_universe"}.
#' @param sb_backtest_workflow Optional list. Required when \code{type = "oos_sb_outputs"}.
#' @param port_backtest_workflow Optional list. Required when \code{type = "stock_universe"}.
#' @param type Character. Determines which subclass is instantiated (see generic \code{type} values). For \code{"generic"} the method
#'   warns about missing monthly dates but still constructs the object; specialized \code{type} values enforce additional required workflow args.
#' @param ... Additional arguments (ignored by many branches).
#'
#' @return An object of class \code{meta_dataframe} or one of its subclasses with metadata:
#'   \code{unique_dates}, \code{unique_tickers}, \code{n_obs}, \code{current_date}, \code{signals}, \code{meta_dataframe_name}.
#'
#' @details — Validation performed by this method (tests assert these behaviours)
#' - Calls \code{is_coercible_to_meta_dataframe()} which checks: required columns, \code{dates} class, \code{id} format, ordering, uniqueness and NA presence.
#' - If \code{type == "generic"} a warning is emitted for missing months in the sequence (monthly cadence).
#' - For specialized types the method requires the corresponding workflow argument: \code{ss_backtest_workflow} for \code{signal_universe},
#'   \code{sb_backtest_workflow} for \code{oos_sb_outputs}, \code{port_backtest_workflow} for \code{stock_universe}.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   id = c("A-2026-01-31","B-2026-01-31"),
#'   tickers = c("A","B"),
#'   dates = as.Date(c("2026-01-31","2026-01-31")),
#'   momentum = c(0.1, -0.2),
#'   value = c(0.05, 0.03),
#'   stringsAsFactors = FALSE
#' )
#' mdf <- create_meta_dataframe(df, meta_dataframe_name = "features_example", type = "generic")
#' # For signal_universe:
#' su <- create_meta_dataframe(df, meta_dataframe_name = "signal_universe_example",
#'                             type = "signal_universe",
#'                             ss_backtest_workflow = list(params = "example"))
#' }
#'
#' @exportMethod create_meta_dataframe

setMethod(
  "create_meta_dataframe", signature(data = "data.frame", meta_dataframe_name = "ANY"),
  function(data, meta_dataframe_name = "not_identified",
           workflow = NULL, ss_backtest_workflow = NULL, sb_backtest_workflow = NULL, port_backtest_workflow = NULL, type = "generic", ...) {
    # Check for type argument
    if (!type %in% c("generic", "signal_universe", "stock_universe", "oos_sb_outputs", "groups",
                     "target", "weights", "priors", "signals", "features", "feature_importance", "raw")) {
      stop("type argument must be one of 'generic', 'signal_universe', 'stock_universe', 'oos_sb_outputs', 'groups', 'target',
                     'weights', 'priors', 'signals', 'features', 'feature_importance' or 'raw'.")
    }

    # Is it coercible
    if (!is_coercible_to_meta_dataframe(data)) {
      stop("The data frame is not coercible to a meta_dataframe object")
    }

    # Ensure no gaps in the dates sequence FOR GENERIC
    unique_dates <- unique(data$dates)
    full_dates <- seq(min(unique_dates), max(unique_dates), by = "month")
    missing_dates <- setdiff(full_dates, unique_dates)

    if (length(missing_dates) > 0 && type == "generic") {
      warning("There are gaps in the dates sequence. Missing dates: ", paste(as.Date(missing_dates), collapse = ", "))
    }


    # Calculate metadata
    unique_dates_count <- length(unique(data$dates))
    unique_tickers_count <- length(unique(data$tickers))
    total_observations_count <- nrow(data)
    current_date <- unique_dates[which.max(unique_dates)]

    # Initialize workflow slot as an empty list
    if (type == "generic") {
      # Store metadata and column names
      return(
        methods::new("meta_dataframe",
                     data = data,
                     workflow = workflow,
                     signals = names(data)[-c(1:3)],
                     unique_dates = unique_dates_count,
                     unique_tickers = unique_tickers_count,
                     n_obs = total_observations_count,
                     meta_dataframe_name = meta_dataframe_name,
                     current_date = current_date
        )
      )
    }

    if (type %in% c("signals", "features")) {
      return(
        methods::new("signals_m_df",
                     data = data,
                     workflow = workflow,
                     signals = names(data)[-c(1:3)],
                     unique_dates = unique_dates_count,
                     unique_tickers = unique_tickers_count,
                     n_obs = total_observations_count,
                     meta_dataframe_name = meta_dataframe_name,
                     current_date = current_date
        )
      )
    }
    if (type == "signal_universe") {
      # Check for workflow
      if (is.null(ss_backtest_workflow)) {
        stop("ss_backtest_workflow argument must be provided for signal_universe type")
      }

      # Store metadata and column names
      return(
        methods::new("signal_universe_m_df",
                     data = data,
                     workflow = NULL,
                     signals = names(data)[-c(1:3)],
                     unique_dates = unique_dates_count,
                     unique_tickers = unique_tickers_count,
                     n_obs = total_observations_count,
                     meta_dataframe_name = meta_dataframe_name,
                     ss_backtest_workflow = ss_backtest_workflow,
                     current_date = current_date
        )
      )
    }
    if (type == "oos_sb_outputs") {
      # Check for workflow
      if (is.null(sb_backtest_workflow)) {
        stop("sb_backtest_workflow argument must be provided for oos_sb_outputs type")
      }

      return(
        methods::new("oos_sb_outputs_m_df",
                     data = data,
                     workflow = NULL,
                     signals = names(data)[-c(1:3)],
                     unique_dates = unique_dates_count,
                     unique_tickers = unique_tickers_count,
                     n_obs = total_observations_count,
                     meta_dataframe_name = meta_dataframe_name,
                     sb_backtest_workflow = sb_backtest_workflow,
                     current_date = current_date
        )
      )
    }

    if (type == "stock_universe") {
      # Check for workflow
      if (is.null(port_backtest_workflow)) {
        stop("port_backtest_workflow argument must be provided for stock_universe type")
      }

      # Store metadata and column names
      return(
        methods::new("stock_universe_m_df",
                     data = data,
                     workflow = NULL,
                     signals = names(data)[-c(1:3)],
                     unique_dates = unique_dates_count,
                     unique_tickers = unique_tickers_count,
                     n_obs = total_observations_count,
                     meta_dataframe_name = meta_dataframe_name,
                     port_backtest_workflow = port_backtest_workflow,
                     current_date = current_date
        )
      )
    }
    if (type == "groups") {
      return(
        methods::new("groups_m_df",
                     data = data,
                     workflow = workflow,
                     signals = names(data)[-c(1:3)],
                     unique_dates = unique_dates_count,
                     unique_tickers = unique_tickers_count,
                     n_obs = total_observations_count,
                     meta_dataframe_name = meta_dataframe_name,
                     current_date = current_date
        )
      )
    }

    if (type == "priors") {
      return(
        methods::new("priors_m_df",
                     data = data,
                     workflow = workflow,
                     signals = names(data)[-c(1:3)],
                     unique_dates = unique_dates_count,
                     unique_tickers = unique_tickers_count,
                     n_obs = total_observations_count,
                     meta_dataframe_name = meta_dataframe_name,
                     current_date = current_date
        )
      )
    }

    if (type == "target") {
      return(
        methods::new("target_m_df",
                     data = data,
                     workflow = workflow,
                     signals = names(data)[-c(1:3)],
                     unique_dates = unique_dates_count,
                     unique_tickers = unique_tickers_count,
                     n_obs = total_observations_count,
                     meta_dataframe_name = meta_dataframe_name,
                     current_date = current_date
        )
      )
    }

    if (type == "weights") {
      return(
        methods::new("weights_m_df",
                     data = data,
                     workflow = workflow,
                     signals = names(data)[-c(1:3)],
                     unique_dates = unique_dates_count,
                     unique_tickers = unique_tickers_count,
                     n_obs = total_observations_count,
                     meta_dataframe_name = meta_dataframe_name,
                     current_date = current_date
        )
      )
    }

    if (type == "feature_importance") {
      return(
        methods::new("feature_importance_m_df",
                     data = data,
                     workflow = workflow,
                     signals = names(data)[-c(1:3)],
                     unique_dates = unique_dates_count,
                     unique_tickers = unique_tickers_count,
                     n_obs = total_observations_count,
                     meta_dataframe_name = meta_dataframe_name,
                     current_date = current_date
        )
      )
    }

    if (type == "raw") {
      return(
        methods::new("raw_features_m_df",
                     data = data,
                     workflow = NULL,
                     signals = names(data)[-c(1:3)],
                     unique_dates = unique_dates_count,
                     unique_tickers = unique_tickers_count,
                     n_obs = total_observations_count,
                     meta_dataframe_name = meta_dataframe_name,
                     current_date = current_date
        )
      )
    }
  }
)



#' Create a meta_dataframe from a list (wide panel)
#'
#' @describeIn create_meta_dataframe Create a \code{meta_dataframe} from a structured \code{list} of wide feature matrices/tibbles.
#'
#' @param data A \code{list} where each element is a matrix, \code{data.frame} or tibble representing a feature. All elements must have identical dimensions (rows = entities, columns = time points).
#' @param tickers Character vector. Row identifiers for the elements of \code{data}; must be unique and length equal to nrow of each element.
#' @param dates Date vector. Column identifiers for the elements of \code{data}; must be \code{Date}, unique, have the same day-of-month, and be consecutive by month (monthly panel).
#' @param features_names Character vector. Names assigned to each element of \code{data}; length must equal \code{length(data)}.
#' @param meta_dataframe_name Character. Name for the resulting object (default: \code{"not_identified"}).
#' @param data_format Character. Currently only \code{"wide"} is supported; the method errors otherwise.
#' @param tickers_on Character. Currently only \code{"rows"} is supported; the method errors otherwise.
#'
#' @return A \code{raw_features_m_df} (subclass of \code{meta_dataframe}) containing a long-format \code{data.frame} with \code{id}, \code{tickers}, \code{dates} and one column per feature (named by \code{features_names}), plus metadata slots.
#'
#' @details — Validation performed by this method (tests assert these behaviours)
#' - \code{data} must be a \code{list}; each element must be a matrix, \code{data.frame}, or tibble.
#' - All elements must have identical numbers of rows and columns; otherwise an error is thrown (tests expect messages such as "All elements in the list must have the same number of rows/columns.").
#' - \code{features_names} length must equal \code{length(data)}.
#' - No element may consist entirely of \code{NA}; otherwise an error ("One or more datasets contain only NA values.").
#' - \code{tickers} must be a unique \code{character} vector whose length equals nrow of elements; errors on non-character or duplicated tickers.
#' - \code{dates} must be class \code{Date}, unique, all share the same day-of-month, and be consecutive by month; failures raise informative errors used in tests.
#' - Elements must not already contain columns named \code{tickers} or \code{dates}; elements must not contain values matching provided \code{tickers} or \code{dates} (these conditions raise errors asserted by tests).
#' - Only \code{data_format = "wide"} and \code{tickers_on = "rows"} are accepted.
#'
#' @examples
#' \dontrun{
#' tickers <- c("Stock A","Stock B")
#' dates <- as.Date(c("2001-03-15","2001-04-15"))
#' feat1 <- matrix(c(0,1,2,3), nrow = 2, byrow = TRUE)
#' feat2 <- matrix(c(4,5,6,7), nrow = 2, byrow = TRUE)
#' feat3 <- matrix(c(8,9,10,11), nrow = 2, byrow = TRUE)
#' features_list <- list(Alpha = feat1, Beta = feat2, Gamma = feat3)
#'
#' raw_mdf <- create_meta_dataframe(features_list,
#'                                  tickers = tickers,
#'                                  dates = dates,
#'                                  features_names = c("Alpha","Beta","Gamma"),
#'                                  meta_dataframe_name = "raw_features_example")
#' }
#'
#' @exportMethod create_meta_dataframe
setMethod(
  "create_meta_dataframe", signature(data = "list", meta_dataframe_name = "ANY"),
  function(data, tickers, dates, features_names, meta_dataframe_name = "not_identified",
           data_format = "wide", tickers_on = "rows") {

    #Initial checks
    #################
    ##Check that data is a list
    if (!is.list(data)) {
      stop("Input must be a list.")
    }
    ##Check that all elements in the list are matrices, data frames, or tibbles
    if (!all(sapply(data, function(x) is.data.frame(x) || is.matrix(x) || tibble::is_tibble(x)))) {
      stop("All elements of the list must be matrices, data frames, or tibbles.")
    }
    ##Check that all elements have the same number of rows
    if (length(unique(sapply(data, nrow))) != 1) {
      stop("All elements in the list must have the same number of rows.")
    }
    ##Check that all elements have the same number of columns
    if (length(unique(sapply(data, ncol))) != 1) {
      stop("All elements in the list must have the same number of columns.")
    }
    ##Check that the length of tickers equals the number of rows in each element
    if (length(tickers) != unique(sapply(data, nrow))) {
      stop("The length of tickers must equal the number of rows in each element of the list.")
    }
    ##Check that the length of dates equals the number of columns in each element
    if (length(dates) != unique(sapply(data, ncol))) {
      stop("The length of dates must equal the number of columns in each element of the list.")
    }
    ##Check that the length of features_names equals the number of list elements
    if (length(features_names) != length(data)) {
      stop("The length of features_names must equal the number of elements in the list.")
    }
    ##Check that there are no NA values in the data
    if (any(sapply(data, function(x) all(is.na(x))))) {
      stop("One or more datasets contain only NA values.")
    }
    ##Verify that tickers is a unique character vector
    if (!is.character(tickers)) {
      stop("tickers must be a character vector.")
    }
    if (any(duplicated(tickers))) {
      stop("tickers must be unique.")
    }
    ##Verify that dates is 'Date', unique, consecutive, and follows a monthly pattern
    if (!inherits(dates, "Date")) {
      stop("dates must be in Date format.")
    }
    if (any(duplicated(dates))) {
      stop("dates must be unique.")
    }
    ###Extract months and days as numeric values
    months <- as.numeric(format(sort(dates), "%m"))
    days <- as.numeric(format(sort(dates), "%d"))
    ###Check that all dates have the same day
    if (length(unique(days)) != 1) {
      stop("All dates must have the same day.")
    }
    ###Check that months follow consecutive order: each month should be exactly one more than the previous,
    ###with December (12) wrapping around to January (1)
    for (i in seq_along(months)[-length(months)]) {
      expected_next <- if (months[i] == 12) 1 else months[i] + 1
      if (months[i + 1] != expected_next) {
        stop("Dates must be consecutive by month.")
      }
    }
    ##Verify that none of the datasets contain columns named 'tickers' or 'dates'
    if (any(sapply(data, function(x) {
      cols <- colnames(x)
      !is.null(cols) && any(c("tickers", "dates") %in% cols)
    }))) {
      stop("One or more datasets already contain a column named 'tickers' or 'dates'.")
    }
    ##Verify that none of the datasets contain columns that exactly match any of the tickers or dates
    if (any(purrr::map_lgl(data, function(x) {
      # Check if any value in any column is in tickers or as.character(dates)
      any(apply(as.data.frame(x), 2, function(col) any(col %in% c(tickers, as.character(dates)))))
    }))) {
      stop("One or more datasets contain values in their columns that match provided tickers or dates.")
    }

    ##Check if arguments are valid
    if (data_format != "wide") stop("Only wide format is currently supported.")
    if (tickers_on != "rows") stop("Only tickers on rows is currently supported.")

    #################

    #Process data
    #################
    ##Convert each feature in features_list to data frame and rename with dates
    features_list <- purrr::map(data, function(x) {
      colnames(x) <- as.character(dates)
      as.data.frame(x)
    })
    ##Sort dates to be sure
    dates <- sort(dates)

    ##For each feature, pivot to long format with a column named after the feature
    feature_dfs_list <- purrr::map2(features_list, features_names, function(feature_df, feat_name) {
      feature_df %>%
        # Add the tickers column
        dplyr::mutate(tickers = tickers) %>%
        # Pivot longer: one column for dates and one column for the feature values
        tidyr::pivot_longer(
          cols = -tickers,
          names_to = "dates",
          values_to = feat_name
        ) %>%
        # Convert dates column to Date type
        dplyr::mutate(dates = as.Date(dates, format = "%Y-%m-%d"))
    })

    ##Join all feature data frames by tickers and dates
    panel_features_df <- purrr::reduce(feature_dfs_list, dplyr::full_join, by = c("tickers", "dates"))

    ##Add unique ID
    raw_features_m_df <- panel_features_df %>%
      dplyr::mutate(id = stringr::str_c(tickers, dates, sep = "-"), .before = tickers) %>%
      dplyr::arrange(id) %>%
      as.data.frame()
    #################

    ##Calculate metadata
    #################
    unique_dates_count <- length(unique(raw_features_m_df$dates))
    unique_tickers_count <- length(unique(raw_features_m_df$tickers))
    total_observations_count <- nrow(raw_features_m_df)
    current_date <- unique(raw_features_m_df$dates)[which.max(unique(raw_features_m_df$dates))]

    # Create meta_dataframe object
    raw_features_m_df <-  methods::new("raw_features_m_df",
                                       data = raw_features_m_df,
                                       workflow = list(),
                                       signals = features_names,
                                       unique_dates = unique_dates_count,
                                       unique_tickers = unique_tickers_count,
                                       n_obs = total_observations_count,
                                       meta_dataframe_name = meta_dataframe_name,
                                       current_date = current_date
    )
    #################

    return(raw_features_m_df)
  }
)

#' Create a target_m_df (prediction targets for backtests and meta-models)
#'
#' @title Build target_m_df for forward horizons
#' @description
#' Generate a \code{target_m_df} containing forward-return targets (and optional active returns) aligned
#' to the rows of a features/metafeatures \code{meta_dataframe}. This routine is the canonical helper
#' used by the package to build prediction targets for signal-blending and meta-model training.
#'
#' @param daily_returns_m_df A \code{meta_dataframe} containing daily (or higher-frequency) asset returns.
#'   Must cover a date range that includes the forward windows required by \code{fwd_horizon}.
#' @param daily_bench_returns_m_xts A \code{meta_xts} of benchmark returns (time-series indexed by Date).
#'   A column specified by \code{selected_bench} will be used to compute active returns and to fill NAs
#'   for delisted series when appropriate.
#' @param features_m_df A \code{meta_dataframe} whose rows (ids) define the observations for which targets
#'   will be produced. Targets are computed for each \code{features_m_df@data$id} / date pair.
#' @param past_ret_column Character scalar. Name of the return column inside \code{daily_returns_m_df@data}
#'   that holds realized returns (e.g. \code{"ret"}).
#' @param selected_bench Character scalar. Column name in \code{daily_bench_returns_m_xts} to use as the benchmark.
#' @param fwd_horizon Integer. Forward horizon in months for the target (e.g., \code{3} for 3-month forward).
#' @param active_returns Logical. If \code{TRUE} the returned target is active (asset return minus benchmark return).
#' @param parallel Logical. If \code{TRUE} uses \pkg{furrr} for parallelizing per-date processing; otherwise processes serially.
#' @param ... Additional arguments forwarded (internal use).
#'
#' @return An S4 object of class \code{target_m_df}. The \code{data} slot is a long \code{data.frame} with
#'   the canonical first-three columns \code{id}, \code{tickers}, \code{dates} and one or more forward-target
#'   metric columns named like \code{fwd_<metric>_<Nhorizon>m}. The \code{workflow} slot is appended with a record
#'   describing the call and parameters used to create the targets.
#'
#' @details
#' - For each row in \code{features_m_df}, the function locates the closest or matching date in \code{daily_returns_m_df}
#'   and aggregates returns across the forward window (days from next-day through end of the horizon window).
#' - Missing forward returns are conservatively replaced with benchmark returns only when appropriate (e.g., trailing
#'   NAs caused by delisting). The helper checks that NA patterns are trailing blocks for each series; violations stop with an error.
#' - If any forward dates exceed \code{daily_returns_m_df@current_date} the corresponding target rows are returned with NA values.
#' - Uses \code{create_meta_xts()} and \code{summarize_performance()} internally to compute summary forward metrics (alpha, IR, active returns, etc.)
#'   and then converts those summaries into a \code{target_m_df} (naming columns with the horizon suffix).
#' - Parallel execution uses \pkg{furrr} with reproducible seeding when available.
#'
#' @examples
#' \dontrun{
#' # daily_returns_m_df and features_m_df are meta_dataframe objects;
#' # daily_bench_returns_m_xts is a meta_xts
#' tgt <- create_target_m_df(
#'   daily_returns_m_df = daily_returns_m_df,
#'   daily_bench_returns_m_xts = daily_bench_returns_m_xts,
#'   features_m_df = features_m_df,
#'   past_ret_column = "ret",
#'   selected_bench = "ibov",
#'   fwd_horizon = 3,
#'   active_returns = TRUE,
#'   parallel = FALSE
#' )
#'
#' # Returned object is of class 'target_m_df'
#' class(tgt)
#' head(tgt@data)
#' }
#'
#' @seealso \code{\link{target_m_df-class}}, \code{\link{create_meta_dataframe}}, \code{\link{run_sb_backtest}}
#'
#' @name create_target_m_df
#' @rdname create_target_m_df
#' @export
setGeneric("create_target_m_df", function(daily_returns_m_df, daily_bench_returns_m_xts, features_m_df, ...) {
  standardGeneric("create_target_m_df")
})



#' @describeIn create_target_m_df Method implementation for meta_dataframe inputs
#'
#' Method signature: \code{daily_returns_m_df = "meta_dataframe"}, \code{daily_bench_returns_m_xts = "meta_xts"},
#' \code{features_m_df = "meta_dataframe"}.
#'
#' @section Input validation (as exercised by tests):
#' - \code{features_m_df@current_date} and \code{daily_returns_m_df@current_date} must be compatible (tests expect same current_date).
#' - All tickers referenced in \code{features_m_df} must exist in \code{daily_returns_m_df}; otherwise an informative error is thrown.
#' - Benchmark column \code{selected_bench} must exist in \code{daily_bench_returns_m_xts}.
#' - \code{fwd_horizon} must be >= 1.
#'
#' @exportMethod create_target_m_df
setMethod("create_target_m_df",
          signature(daily_returns_m_df = "meta_dataframe", daily_bench_returns_m_xts = "meta_xts", features_m_df = "meta_dataframe"),
          function(daily_returns_m_df, daily_bench_returns_m_xts, features_m_df, past_ret_column, selected_bench, fwd_horizon, active_returns, parallel = TRUE) {

            #Initial checks
            ###############
            if (features_m_df@current_date != daily_returns_m_df@current_date){
              stop("features_m_df and daily_returns_m_df must have the same current_date.")
            }
            if (!all(features_m_df@data$tickers %in% daily_returns_m_df@data$tickers)){
              stop("Some tickers from features_m_df do not exist in daily_returns_m_df")
            }
            if (!identical(as.character(sort(unique(daily_returns_m_df@data$dates))),
                           as.character(sort(zoo::index(daily_bench_returns_m_xts@data))))){
              stop("Dates from daily_returns_m_df and daily_bench_returns_m_xts should match.")
            }
            if (!selected_bench %in% colnames(daily_bench_returns_m_xts@data)){
              stop("selected_bench must be a column in benchmark_returns_m_xts.")
            }
            if (!past_ret_column %in% colnames(daily_returns_m_df@data)){
              stop("past_ret_column must be a column in daily_returns_m_df.")
            }
            if (fwd_horizon < 1){
              stop("fwd_horizon must be greater than 0.")
            }

            ##############

            #Calculate
            ##############

            ##Get selected dates and ids
            selected_ids     <- features_m_df@data %>% dplyr::pull(id)
            selected_tickers <- features_m_df@data %>% dplyr::pull(tickers) %>% unique()
            selected_daily_returns_m_df <- daily_returns_m_df@data %>% dplyr::filter(tickers %in% selected_tickers)
            selected_dates <- features_m_df@data %>% dplyr::pull(dates) %>% unique() %>% sort()

            ##Build fwd_date_process fun
            fwd_date_process <- function(i){

              ###Select bench
              selected_daily_bench_returns_m_xts <- daily_bench_returns_m_xts@data[, selected_bench]

              ###Subset current row and date
              current_date <- selected_dates[i]
              closest_date_in_daily_returns_m_d_ref <- selected_daily_returns_m_df %>%
                dplyr::filter(dates >= current_date) %>%
                dplyr::pull(dates) %>%
                min()
              selected_daily_returns_m_d_ref <- selected_daily_returns_m_df %>% dplyr::filter(dates %in% closest_date_in_daily_returns_m_d_ref)
              current_tickers <- selected_daily_returns_m_d_ref %>% dplyr::pull(tickers) %>% unname()

              ####Print
              cat(crayon::cyan(paste0("Processing date ", format(as.Date(current_date), "%Y-%m-%d"))))
              cat("\n")

              ####Ensure all tickers for current_date in features_m_df exist in daily_returns_m_df
              current_features_tickers <- features_m_df@data %>% dplyr::filter(dates == current_date) %>% dplyr::pull(tickers)
              #### If current date is the last date of selected_dates, we allow for the possibility of current_tickers being empty
              if (current_date == max(selected_dates) && length(current_tickers) == 0) {
                cat(crayon::yellow(paste0("Warning: No tickers found in daily_returns_m_df for the last date ",
                                          format(as.Date(current_date), "%Y-%m-%d"), ". Check if it is a holiday.")))
              } else {
              missing_tickers <- setdiff(current_features_tickers, current_tickers)
              if (length(missing_tickers) > 0L){
                stop("The following tickers from features_m_df for date ", current_date,
                     " are missing in daily_returns_m_df: ",
                     paste(missing_tickers, collapse = ", "))
                }
              }

              ###Compute forward dates
              fwd_dates <- lubridate::add_with_rollback(current_date, months(0:fwd_horizon))
              seq_fwd_dates <- seq.Date(from = fwd_dates[1] + 1, to = fwd_dates[length(fwd_dates)], by = "days")

              ###If any of the dates in exceed current_date, return NA
              ### We do this before checking for selected_daily_returns_m_d_ref rows in case day 15 is a holiday
              if (any(seq_fwd_dates > daily_returns_m_df@current_date)) {
                out <- data.frame(
                  tickers = current_features_tickers,
                  dates = current_date,
                  id = paste0(current_features_tickers, "-", current_date)
                ) %>% dplyr::select(id, tickers, dates)

                return(out)
              }

              ####Ensure current row is correctly assigned
              if (nrow(selected_daily_returns_m_d_ref) == 0) {
                stop("No data found for selected date: ", current_date)
              }

              ####Ensure there is at least one closest date in daily_returns_m_d_ref
              if (length(closest_date_in_daily_returns_m_d_ref) == 0 | is.infinite(closest_date_in_daily_returns_m_d_ref) | is.na(closest_date_in_daily_returns_m_d_ref)) {
                stop("No closest date found in daily_returns_m_df for selected date: ", current_date)
              }

              ###Retrieve all forward returns from selected_daily_returns_m_df and replace NAs with 0
              selected_daily_returns_m_d_fwd <- daily_returns_m_df@data %>% #Get from complete database
                dplyr::filter(tickers %in% current_features_tickers, dates %in% seq_fwd_dates) %>% #Subset ticker_i and only fwd dates
                dplyr::mutate(dplyr::across(dplyr::all_of(past_ret_column), ~ tidyr::replace_na(.x, 0))) #Replace NAs with 0

              ###Do the same with benchmark
              selected_daily_bench_returns_m_xts_fwd <- selected_daily_bench_returns_m_xts[
                which(zoo::index(selected_daily_bench_returns_m_xts) %in% seq_fwd_dates), #Only fwd dates
              ]

              ###Check if there are any NAs in selected_daily_returns_m_d_fwd or selected_daily_bench_returns_m_xts_fwd and stop
              if (any(is.na(selected_daily_returns_m_d_fwd)) | any(is.na(selected_daily_bench_returns_m_xts_fwd))) {
                stop("There are NAs in selected_daily_returns_m_d_fwd or selected_daily_bench_returns_m_xts_fwd.")
              }

              ###Convert to meta_xts
              selected_daily_returns_m_xts_fwd <- create_meta_xts(selected_daily_returns_m_d_fwd, type = "returns",
                                                                  data_format = "long", asset_type = "stocks")

              ###Replace NAs at this point with bench_returns
              ###NAs at this point come up because of binding the distinct sized return series
              ###If a series has a smaller size, it will be filled with NAs for dates in which the original series had no information
              ###This is inherent to a delisted stock

              ####Get NAs
              na_idx <- is.na(selected_daily_returns_m_xts_fwd@data)

              #####Conservatively check if NAs occur in a trailing block
              is_trailing_na <- function(col_na) {
                first_na <- which(col_na)[1]
                if (is.na(first_na)) return(TRUE)  # No NA in column
                all(col_na[first_na:length(col_na)])
              }

              ####Check each column of the logical matrix
              na_pattern_ok <- apply(na_idx, 2, is_trailing_na)

              ####Stop if any column violates the rule
              if (!all(na_pattern_ok)) {
                bad_tickers <- colnames(na_idx)[!na_pattern_ok]
                stop("Invalid NA pattern in the following tickers: ",
                     paste(bad_tickers, collapse = ", "),
                     ". NAs must form a trailing block (e.g., due to delisting).")
              }

              ####Ensure alignment of rows
              stopifnot(identical(zoo::index(selected_daily_returns_m_xts_fwd@data), zoo::index(selected_daily_bench_returns_m_xts_fwd)))

              ####Replace NAs row-wise
              selected_daily_returns_m_xts_fwd@data[na_idx] <-
                as.numeric(selected_daily_bench_returns_m_xts_fwd)[row(selected_daily_returns_m_xts_fwd@data)][na_idx] #Replace NAs with bench_returns

              ###Pass to summarize
              target_m_d_ref <- summarize_performance(
                selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_daily_returns_m_xts_fwd@data,
                selected_market_factor_proxy_m_xts_upd_ref = selected_daily_bench_returns_m_xts_fwd,
                model_structure = "no_pooled",
                model_spec_theme_level = NULL,
                lmer_control = NULL,
                selected_signal_themes_m_d_ref = NULL,
                active_returns = active_returns
              )$signal_universe_m_d_ref

              cat(crayon::cyan(paste0("Finished processing date ", format(as.Date(current_date), "%Y-%m-%d"))))
              cat("\n\n--------------------------------\n\n")

              ###Replace dates of target_m_d_ref with current_date
              target_m_d_ref$dates <- current_date

              return(target_m_d_ref)

            }

            ##Compute forward returns using purrr::map or future::future_map
            if (parallel){
              selected_target_m_df_list <- furrr::future_map(seq_len(length(selected_dates)),
                                                             .f = fwd_date_process,
                                                             .options = furrr::furrr_options(seed = TRUE)
              )
            } else {
              selected_target_m_df_list <- purrr::map(seq_len(length(selected_dates)), .f = fwd_date_process)
            }

            ##############

            #Finalize
            ##############

            ##Use bind_rows to combine all dataframes in the list and correct id
            target_m_df <- dplyr::bind_rows(selected_target_m_df_list) %>%
              dplyr::mutate(id = paste0(tickers, "-", dates)) %>%
              dplyr::arrange(id)

            ##Rename all metrics (except first three columns id, tickers and dates) to fwd_metric_fwd_horizon_m
            target_m_df <- target_m_df %>% dplyr::rename_with(~ paste0("fwd_", ., "_", fwd_horizon, "m"), .cols = -c(1:3))

            ##Create target_m_df object
            ###New enty to workflow
            new_entry <- list(
              list(current_date = daily_returns_m_df@current_date,  # Current date
                   timestamp = Sys.time(),        # Timestamp
                   selected_bench = selected_bench, # Selected benchmark
                   bench_returns_m_xts_name = daily_bench_returns_m_xts@meta_xts_name, # Benchmark name
                   past_ret_column = past_ret_column, # Past return column
                   fwd_horizon = fwd_horizon, # Forward horizon
                   selected_dates = selected_dates, # Selected dates
                   active_returns = active_returns, # Active returns
                   parallel = parallel,
                   call = match.call() # Call
              )
            )

            target_m_df <- create_meta_dataframe(target_m_df, type = "target",
                                                 meta_dataframe_name = daily_returns_m_df@meta_dataframe_name,
                                                 workflow = c(daily_returns_m_df@workflow, new_entry)
            )

            names(target_m_df@workflow)[length(target_m_df@workflow)] <- paste0("create_target_m_df_", daily_returns_m_df@current_date)

            return(target_m_df)
          })





#' Update a meta_dataframe with a new batch of data
#'
#' @title Update meta_dataframe by appending a batch
#' @description
#' Append a new batch (usually one month of observations) to an existing \code{meta_dataframe},
#' validating compatibility and consolidating metadata. Intended for incremental ingestion of feature
#' tables produced monthly (or daily) while preserving workflow provenance.
#'
#' @param old_features_m_df A \code{meta_dataframe} containing the existing (historical) panel.
#' @param new_features_m_df A \code{meta_dataframe} containing the new batch to append. Typically the new object
#'   contains a single date (monthly batch) and the same set of columns as \code{old_features_m_df}.
#' @param batch_type Character. Batch cadence: \code{"monthly"} (default) or \code{"daily"}. Controls validation of
#'   the number of unique dates in \code{new_features_m_df}.
#' @param ... Additional arguments (reserved / forwarded).
#'
#' @return A \code{meta_dataframe} with rows from \code{new_features_m_df} appended to \code{old_features_m_df},
#'   metadata consolidated via \code{consolidate_generic_meta_dataframes()}, and the \code{workflow} slot updated
#'   with a new \code{update_<date>} entry describing the batch.
#'
#' @details
#' The method performs strict compatibility checks before merging (tests assert these behaviours):
#' - Column names and classes must match exactly between old and new objects.
#' - There must be no overlapping \code{id} values or overlapping \code{dates}.
#' - For \code{batch_type == "monthly"} the new batch must contain exactly one unique date.
#' - For \code{batch_type == "daily"} the number of unique dates is validated to be in a reasonable range.
#' - \code{new_features_m_df@current_date} must equal one month after \code{old_features_m_df@current_date}.
#' - Neither object may be of class \code{raw_features_m_df}.
#' - Both objects must contain a \code{read_tickers_catalog} entry in their \code{workflow} (ensures consistent ticker mapping).
#'
#' On success, the function calls \code{consolidate_generic_meta_dataframes(..., type = "generic")} to merge
#' the tables and then appends a batch entry to the \code{workflow} slot describing the new date, batch name,
#' timestamp and the batch's own workflow.
#'
#' @examples
#' \dontrun{
#' # old_mdf and new_mdf are meta_dataframe objects
#' # (new_mdf typically contains a single new monthly date)
#' updated <- update_meta_dataframe(old_mdf, new_mdf, batch_type = "monthly")
#' }
#'
#' @name update_meta_dataframe
#' @rdname update_meta_dataframe
#' @export

setGeneric("update_meta_dataframe", function(old_features_m_df, new_features_m_df, ...) {
  standardGeneric("update_meta_dataframe")
})




#' @describeIn update_meta_dataframe Method implementation for meta_dataframe inputs
#'
#' Method signature: \code{old_features_m_df = "meta_dataframe"}, \code{new_features_m_df = "meta_dataframe"}.
#'
#' @exportMethod update_meta_dataframe
setMethod(
  "update_meta_dataframe", signature(old_features_m_df = "meta_dataframe", new_features_m_df = "meta_dataframe"),
  function(old_features_m_df, new_features_m_df, batch_type = "monthly"){

    #Initial prep
    ###############
    ##meta_dataframe_name
    old_features_m_df_name <- old_features_m_df@meta_dataframe_name
    new_features_m_df_name <- new_features_m_df@meta_dataframe_name

    ##workflow
    old_workflow <- old_features_m_df@workflow
    new_workflow <- new_features_m_df@workflow

    ##current dates
    old_current_date <- old_features_m_df@current_date
    new_current_date <- new_features_m_df@current_date

    ##object class
    old_class <- class(old_features_m_df)
    new_class <- class(new_features_m_df)

    ###############

    #Check if both meta_dataframes are compatible
    ###############
    ##Check colnames match
    if (ncol(old_features_m_df@data) != ncol(new_features_m_df@data) ||
        any(colnames(old_features_m_df@data) != colnames(new_features_m_df@data))){
      stop("Column names between old_features_m_df and new_features_m_df do not match.")
    }
    ##Check that there is NO id intersection between old_features_m_df and new_features_m_df
    if (length(dplyr::intersect(old_features_m_df@data$id, new_features_m_df@data$id)) > 0){
      stop("There are common ids between old_features_m_df and new_features_m_df.")
    }
    ##Check that there is NO date intersection between old_features_m_df and new_features_m_df
    if (length(dplyr::intersect(old_features_m_df@data$dates, new_features_m_df@data$dates)) > 0){
      stop("There are common dates between old_features_m_df and new_features_m_df.")
    }
    ##Check that number of unique dates in new_features_m_df is equal to expectations
    if (batch_type == "monthly"){
      if (length(unique(new_features_m_df@data$dates)) != 1){
        stop("Number of unique dates in new_features_m_df is not equal to 1.")
      }
    }
    if (batch_type == "daily"){
      if (length(unique(new_features_m_df@data$dates)) %in% c(15, 40)){
        stop("Number of unique dates in new_features_m_df is not in a reasonable range for daily data.")
      }
    }
    ##Check that current_date in new_features_m_df is 1 months ahead of current_date in old_features_m_df
    if (new_current_date != lubridate::add_with_rollback(old_current_date, months(1))){
      stop("Current date in new_features_m_df should be 1 months ahead of current_date in old_features_m_df.")
    }
    ##Check that each column class match between old_features_m_df and new_features_m_df
    if (!all(sapply(
      colnames(old_features_m_df@data),
      function(col) identical(class(old_features_m_df@data[[col]]), class(new_features_m_df@data[[col]]))
    ))) {
      stop("Column classes between old_features_m_df and new_features_m_df do not match.")
    }

    ##Check that old_features_m_df name is contained in new_features_m_df name
    if (!grepl(old_features_m_df_name, new_features_m_df_name)){
      stop("old_features_m_df name is not contained in new_features_m_df name.")
    }

    ##Check if any object is of class raw_features_m_df
    if (old_class == "raw_features_m_df" || new_class == "raw_features_m_df"){
      stop("old_features_m_df and new_features_m_df should not be of class raw_features_m_df.")
    }

    ##Check if they contain a read_tickers_catalog workflow
    if (!any(stringr::str_detect(names(old_workflow), "read_tickers_catalog"))){
      stop("old_features_m_df should contain a read_tickers_catalog workflow.")
    }
    if (!any(stringr::str_detect(names(new_workflow), "read_tickers_catalog"))){
      stop("new_features_m_df should contain a read_tickers_catalog workflow.")
    }

    ###############

    #Consolidate
    ###############
    updated_meta_dataframe <- consolidate_generic_meta_dataframes(
      main_generic_m_df = old_features_m_df,
      supplemental_generic_m_df = new_features_m_df,
      type = "generic",
      consolidate_name = FALSE
    )
    ###############

    #Update Workflow
    ###############
    batch_workflow <-
      list(
        list(
          new_date = new_current_date, #Add new date
          batch_features_m_df_name = new_features_m_df_name, #Name of batch
          timestamp = Sys.time(),
          batch_workflow = new_workflow #Specific workflow
        )
      )

    update_workflow <- c(old_workflow, batch_workflow) #Add to the old workflow
    names(update_workflow)[length(update_workflow)] <- paste0("update_", new_current_date)

    ###############

    #Update workflow
    ###############
    updated_meta_dataframe@workflow <- update_workflow

    return(updated_meta_dataframe)

  }
)







# tickers_catalog------------------------------------------------------
#' Create a tickers_catalog Object
#'
#' Constructs a \code{tickers_catalog} object by integrating stock metadata from multiple data sources.
#' Ensures data consistency, generates unique identifiers, and classifies stocks based on listing status.
#'
#' @param raw_features_m_df A \code{meta_dataframe} object with \code{type == "generic"}, containing stock tickers and dates.
#' @param date_first_quote A \code{data.frame} with columns \code{tickers} (character) and \code{date_first_quote} (Date).
#' @param date_last_quote A \code{data.frame} with columns \code{tickers} (character) and \code{date_last_quote} (Date).
#' @param n_days_tolerance A \code{numeric} indicating the maximum tolerance (in days) for which \code{date_last_quote}
#'   can be older than \code{current_date} without classifying the stock as delisted. Default: \code{10}.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{tickers_catalog}.
#'
#' @details
#' This generic performs these responsibilities (method implementations perform checks described below):
#' 1. Validate input types and required columns in \code{date_first_quote} and \code{date_last_quote}.
#' 2. Verify \code{tickers} are identical across \code{raw_features_m_df}, \code{date_first_quote} and \code{date_last_quote}.
#' 3. Enforce that \code{date_last_quote >= date_first_quote} when both are present (NAs allowed).
#' 4. Generate a deterministic \code{perm_id} per ticker from \code{ticker + date_first_quote}.
#' 5. Extract \code{current_date} as the most recent date in \code{raw_features_m_df@data$dates}.
#' 6. Classify tickers as \code{untraded} (both dates NA), \code{delisted} (last_quote < current_date - tolerance),
#'    or \code{listed} (last_quote >= current_date - tolerance).
#' 7. Emit informative errors or warnings for duplicate tickers, mismatched ticker sets, inconsistent NA patterns,
#'    invalid date ordering, or if no tradable stocks are found.
#'
#' @seealso \code{\link{tickers_catalog-class}}, \code{\link{update_tickers_catalog}}, \code{\link{read_tickers_catalog}}, \code{\link{create_meta_dataframe}}
#'
#' @importFrom digest digest
#' @export
setGeneric("create_tickers_catalog", function(raw_features_m_df, date_first_quote, date_last_quote, ...) {
  standardGeneric("create_tickers_catalog")
})

#' Construct tickers_catalog from raw_features_m_df + first/last-quote tables
#'
#' @describeIn create_tickers_catalog Method that builds a \code{tickers_catalog} from
#'   a \code{raw_features_m_df} and two \code{data.frame}s containing per-ticker
#'   first- and last-quote dates.
#'
#' @param raw_features_m_df A \code{raw_features_m_df} (source panel).
#' @param date_first_quote A \code{data.frame} with columns \code{tickers} and \code{date_first_quote} (coercible to \code{Date}).
#' @param date_last_quote A \code{data.frame} with columns \code{tickers} and \code{date_last_quote} (coercible to \code{Date}).
#' @param n_days_tolerance Numeric scalar (in days). Window used to decide whether a last-quote date indicates delisting relative to \code{raw_features_m_df@current_date}. Default: \code{10}.
#'
#' @return An object of class \code{tickers_catalog} with slots:
#'   \code{catalog}, \code{tickers}, \code{perm_id} (named by tickers), \code{tickers_first_quote},
#'   \code{tickers_last_quote}, \code{untraded}, \code{delisted}, \code{listed}, \code{old},
#'   \code{current_date}, \code{meta_dataframe_name}, \code{n_days_tolerance}, and \code{ticker_change_history}.
#'
#' @details
#' Method behavior and validations (errors/warnings mirror unit tests):
#' - Requires the date tables to contain the columns \code{tickers} + \code{date_first_quote} / \code{date_last_quote};
#'   otherwise errors with "date_first_quote must have columns 'tickers' and 'date_first_quote', and date_last_quote must have 'tickers' and 'date_last_quote'."
#' - Converts date columns to \code{Date} and errors on duplicate tickers ("Duplicate tickers found in date_first_quote or date_last_quote.").
#' - Errors with "No tradable stocks identified" when all dates are NA.
#' - Requires the ticker sets to match across inputs; otherwise errors "Mismatch in tickers between raw_features_m_df, date_first_quote, and date_last_quote."
#' - Enforces pairwise NA-ness (both NA or neither) and errors "date_first_quote and date_last_quote must both be NA or neither."
#' - Warns when the modal \code{date_last_quote} is older than \code{current_date - n_days_tolerance} with:
#'   "Most common date in date_last_quote is not the last date in raw_features_m_df - n_days_tolerance. Consider increasing n_days_tolerance"
#' - Errors if any \code{date_last_quote < date_first_quote} with:
#'   "date_last_quote must be greater than or equal to date_first_quote for all tickers."
#' - Warns if any \code{date_last_quote} is > \code{current_date}: "Some date_last_quote values are greater than the current_date. This may indicate future dates or errors in the data."
#' - Generates deterministic \code{perm_id} values using an MD5-derived short hash of \code{ticker + first_quote} (NA first-quote uses "NA"), sorts the internal \code{catalog} by \code{perm_id}, and classifies tickers into \code{untraded}, \code{delisted}, \code{listed}, \code{old}.
#'
#' @examples
#' \dontrun{
#' df_first <- data.frame(tickers = c("A","B"),
#'                        date_first_quote = as.Date(c("1995-01-01","1996-01-01")))
#' df_last  <- data.frame(tickers = c("A","B"),
#'                        date_last_quote  = as.Date(c(Sys.Date()-1, Sys.Date()-20)))
#' tc <- create_tickers_catalog(raw_features_m_df = raw_mdf,
#'                              date_first_quote = df_first,
#'                              date_last_quote  = df_last,
#'                              n_days_tolerance = 10)
#' }
#'
#' @exportMethod create_tickers_catalog
setMethod("create_tickers_catalog",
          signature(
            raw_features_m_df = "raw_features_m_df",
            date_first_quote = "data.frame",
            date_last_quote = "data.frame"
          ),
          function(raw_features_m_df, date_first_quote, date_last_quote, n_days_tolerance = 10) {

            #Initial checks
            #############

            ##Check dfs constraints
            if (!all(c("tickers", "date_first_quote") %in% colnames(date_first_quote)) ||
                !all(c("tickers", "date_last_quote") %in% colnames(date_last_quote))) {
              stop("date_first_quote must have columns 'tickers' and 'date_first_quote', and date_last_quote must have 'tickers' and 'date_last_quote'.")
            }

            ##Check tickers
            ###Convert date columns to Date type
            date_first_quote$date_first_quote <- as.Date(date_first_quote$date_first_quote)
            date_last_quote$date_last_quote <- as.Date(date_last_quote$date_last_quote)

            ###Check that there are no duplicate tickers in date_first_quote or date_last_quote
            if (nrow(date_first_quote) != nrow(unique(date_first_quote)) || nrow(date_last_quote) != nrow(unique(date_last_quote))) {
              stop("Duplicate tickers found in date_first_quote or date_last_quote.")
            }

            ###Check if all are untraded (date_first_quote and date_last_quote are all NAs)
            if (all(is.na(date_first_quote$date_first_quote)) || all(is.na(date_last_quote$date_last_quote))) {
              stop("No tradable stocks identified")
            }

            ###Ensure tickers match exactly across all data sources
            common_tickers <- intersect(intersect(raw_features_m_df@data$tickers %>% unique(), date_first_quote$tickers), date_last_quote$tickers)
            if (length(common_tickers) != length(raw_features_m_df@data$tickers %>% unique())) {
              stop("Mismatch in tickers between raw_features_m_df, date_first_quote, and date_last_quote.")
            }

            ##Check that either both dates are NA or neither is
            if (any(is.na(date_first_quote[order(date_first_quote$tickers), "date_first_quote"]) !=
                    is.na(date_last_quote[order(date_last_quote$tickers), "date_last_quote"]))) {
              stop("date_first_quote and date_last_quote must both be NA or neither.")
            }

            ##Check that a warning is thrown when last_date most common date is not the last date
            if (names(table(date_last_quote$date_last_quote) %>% which.max()) <= #Most common date (mode)
                max(unique(raw_features_m_df@data$dates), na.rm = TRUE) - lubridate::days(n_days_tolerance)) { #Current date
              warning("Most common date in date_last_quote is not the last date in raw_features_m_df - n_days_tolerance. Consider increasing n_days_tolerance")
            }

            ##Check that date_last_quote > date_first_quote (allowing NAs)
            invalid_dates <- date_first_quote %>%
              dplyr::inner_join(date_last_quote, by = "tickers") %>% #Get common tickers between both data_frames
              dplyr::filter(!is.na(date_first_quote) & !is.na(date_last_quote) & date_last_quote < date_first_quote) #Filter invalid dates

            if (nrow(invalid_dates) > 0) {
              stop("date_last_quote must be greater than or equal to date_first_quote for all tickers.")
            }

            ##Check that no date_last_quote is > current_date
            if (any(date_last_quote$date_last_quote[which(!is.na(date_last_quote$date_last_quote))] > raw_features_m_df@current_date)){
              warning("Some date_last_quote values are greater than the current_date. This may indicate future dates or errors in the data.")
            }

            #############

            #Create object
            ###############
            ## Extract tickers and initialize object
            tickers_catalog <- raw_features_m_df@data %>%
              dplyr::select(tickers) %>% #Select only tickers
              dplyr::distinct() %>%  #Remove duplicates
              dplyr::left_join(date_first_quote, by = "tickers") %>% #Join date_first_quote
              dplyr::left_join(date_last_quote, by = "tickers") %>% #Join date_last_quote
              dplyr::rename(tickers_first_quote = date_first_quote, tickers_last_quote = date_last_quote) #Rename columns



            ##Generate perm_id
            ###Define function to generate perm_id
            generate_perm_id <- function(ticker, tickers_first_quote) {
              date_str <- ifelse(is.na(tickers_first_quote), "NA", format(tickers_first_quote, "%Y%m%d"))
              short_hash <- paste0("h", substr(digest::digest(paste0(ticker, "_", date_str), algo = "md5"), 1, 9))
              return(short_hash)
            }

            ###Generate perm_id
            tickers_catalog <- tickers_catalog %>%
              dplyr::mutate(perm_id = mapply(generate_perm_id, tickers, tickers_first_quote)) %>%
              dplyr::mutate(perm_id = unname(perm_id)) %>%
              dplyr::arrange(perm_id) #Arrange by perm_id

            ###############

            #Classify stocks
            ################
            ##Get current_date (max of unique dates in raw_features_m_df)
            current_date <- max(unique(raw_features_m_df@data$dates), na.rm = TRUE)

            ##Classify private and delisted stocks
            tickers_catalog <- tickers_catalog %>%
              dplyr::mutate(
                untraded = is.na(tickers_first_quote) & is.na(tickers_last_quote),
                delisted = !is.na(tickers_last_quote) & tickers_last_quote < (current_date - lubridate::days(n_days_tolerance)),
                listed = !is.na(tickers_last_quote) & tickers_last_quote >= (current_date - lubridate::days(n_days_tolerance)),
                old = FALSE
              )

            ################

            #Prepare slots
            ##############
            tickers_catalog_slots <- prepare_tickers_catalog_slots(tickers_catalog)
            ################

            ## Create tickers_catalog object
            tickers_catalog_obj <-  methods::new(
              "tickers_catalog",
              catalog = tickers_catalog,
              tickers = tickers_catalog_slots$tickers,
              perm_id = tickers_catalog_slots$perm_id,
              tickers_first_quote = tickers_catalog_slots$tickers_first_quote,
              tickers_last_quote = tickers_catalog_slots$tickers_last_quote,
              untraded = tickers_catalog_slots$untraded,
              delisted = tickers_catalog_slots$delisted,
              listed = tickers_catalog_slots$listed,
              old = tickers_catalog_slots$old,
              current_date = current_date,
              meta_dataframe_name = raw_features_m_df@meta_dataframe_name,
              n_days_tolerance = n_days_tolerance,
              ticker_change_history = NULL
            )

            return(tickers_catalog_obj)
          })

#' @title Prepare Slots for tickers_catalog Object
#' @description
#' Internal helper that extracts and prepares structured components from a `tickers_catalog` object.
#' Used by constructors like `create_tickers_catalog()` and `update_tickers_catalog()`.
#'
#' @param tickers_catalog A `tickers_catalog` S4 object containing ticker classification columns
#' such as `tickers`, `perm_id`, `tickers_first_quote`, `tickers_last_quote`,
#' and classification flags like `untraded`, `delisted`, `listed`, and `old`.
#'
#' @return A named list with the following elements:
#' \itemize{
#'   \item `tickers_catalog`: The original input.
#'   \item `tickers`: Vector of ticker symbols.
#'   \item `perm_id`: Named vector of permanent IDs.
#'   \item `tickers_first_quote`: First quote dates.
#'   \item `tickers_last_quote`: Last quote dates.
#'   \item `untraded`: Tickrs classified as untraded.
#'   \item `delisted`: Tickers classified as delisted.
#'   \item `listed`: Tickers currently listed.
#'   \item `old`: Tickers marked as old due to ticker changes.
#' }
#'
#' @keywords internal
#' @noRd
prepare_tickers_catalog_slots <- function(tickers_catalog){

  ##Create subsets for slots
  untraded_df <- tickers_catalog %>% dplyr::filter(untraded == TRUE)
  delisted_df <- tickers_catalog %>% dplyr::filter(delisted == TRUE)
  listed_df <- tickers_catalog %>% dplyr::filter(listed == TRUE)
  old_df <- tickers_catalog %>% dplyr::filter(old == TRUE)


  ##Get perm_id and name it
  perm_id <- tickers_catalog$perm_id
  names(perm_id) <- tickers_catalog$tickers

  ##Results
  return(list(
    tickers_catalog = tickers_catalog,
    tickers = tickers_catalog$tickers,
    perm_id = perm_id,
    tickers_first_quote = tickers_catalog$tickers_first_quote,
    tickers_last_quote = tickers_catalog$tickers_last_quote,
    untraded = untraded_df$tickers,
    delisted = delisted_df$tickers,
    listed = listed_df$tickers,
    old = old_df$tickers
  ))
}



#' Update a `tickers_catalog` Object
#'
#' @description
#' Updates a `tickers_catalog` object with a newer `tickers_catalog`, reconciling
#' ticker classifications (`listed`, `delisted`, `untraded`, `old`) and resolving
#' ticker changes so that renamed tickers keep a single `perm_id` across time.
#'
#' @param old_tickers_catalog A `tickers_catalog` S4 object representing the
#'   previously stored catalog (the state as of the last update).
#' @param new_tickers_catalog A `tickers_catalog` S4 object containing the latest
#'   batch of stock data. Its `current_date` must equal
#'   `old_tickers_catalog@current_date` plus one month, and its maximum
#'   `tickers_last_quote` must be strictly greater than the old catalog's.
#' @param ticker_changes A `data.frame` mapping tickers renamed between
#'   `old_tickers_catalog` and `new_tickers_catalog`, with columns:
#'   \describe{
#'     \item{\code{new_tickers}}{Character. The new ticker symbol observed in `new_tickers_catalog`.}
#'     \item{\code{old_tickers}}{Character. The corresponding previous ticker symbol in `old_tickers_catalog`.}
#'     \item{\code{change_date}}{Date. The date on which the ticker change occurred.}
#'   }
#'   Must contain no `NA`s and no duplicated `new_tickers`/`old_tickers`. Defaults
#'   to `NULL`, treated as an empty (zero-row) `data.frame`, i.e. no ticker changes
#'   between updates. Any ticker newly observed in `new_tickers_catalog` and not
#'   listed here is treated as a new IPO; any ticker missing from
#'   `old_tickers_catalog`'s successor without a matching entry here triggers an
#'   error.
#' @param ... Additional arguments (currently unused; reserved for future methods).
#'
#' @return A new `tickers_catalog` S4 object combining `old_tickers_catalog` and
#'   `new_tickers_catalog`:
#'   \itemize{
#'     \item Renamed tickers keep the `perm_id` of their pre-change ticker (looked
#'       up through the full ticker-change history, not just the current call's
#'       `ticker_changes`).
#'     \item Genuinely new tickers keep the hash-based `perm_id` assigned by
#'       `create_tickers_catalog()`.
#'     \item At a rename boundary, `tickers_last_quote` of the old ticker and
#'       `tickers_first_quote` of the new ticker are both set to `change_date`.
#'     \item Pre-rename ticker rows are retained with `old = TRUE` and
#'       `listed = delisted = untraded = FALSE`, so history is never dropped.
#'     \item `current_date` is taken from `new_tickers_catalog`; `meta_dataframe_name`
#'       and `n_days_tolerance` are taken from `old_tickers_catalog`.
#'     \item `ticker_change_history` accumulates `ticker_changes` across calls.
#'   }
#'
#' @details
#' The update proceeds in three stages:
#' 1. **Validation** of `ticker_changes` structure/types/uniqueness, of the
#'    partition of newly added tickers into IPOs vs. renames, of classification
#'    transitions, and of date consistency between the two catalogs.
#' 2. **`perm_id` reassignment**, inheriting the predecessor's `perm_id` for
#'    renamed tickers.
#' 3. **Recombination**, re-appending old (pre-rename) rows and merging
#'    `ticker_change_history`.
#'
#' Validation failures (`stop()`) include: malformed `ticker_changes` (missing
#' columns, wrong types, `NA`s, duplicates); newly added tickers that cannot be
#' decomposed into IPOs and renamed old tickers; a `delisted` ticker being
#' renamed, or becoming `listed`/`untraded`; an `untraded` ticker becoming
#' `listed` (or vice versa), with or without a rename; a `tickers_first_quote`/
#' `tickers_last_quote` mismatch for tickers common to both catalogs (delisted
#' tickers' `tickers_last_quote` must not change); an IPO's `tickers_first_quote`
#' earlier than `old_tickers_catalog@current_date`; `new_tickers_catalog`'s
#' maximum `tickers_last_quote` not exceeding the old catalog's; and
#' `new_tickers_catalog@current_date` not exactly one month after
#' `old_tickers_catalog@current_date`. A mismatched `n_days_tolerance` between
#' catalogs produces a `warning()` rather than a `stop()`.
#'
#' @examples
#' \dontrun{
#' updated_catalog <- update_tickers_catalog(
#'   old_tickers_catalog = old_catalog,
#'   new_tickers_catalog = new_catalog,
#'   ticker_changes = data.frame(
#'     new_tickers = "BRAV3",
#'     old_tickers = "RRRP3",
#'     change_date = as.Date("2001-06-02")
#'   )
#' )
#' }
#'
#' @seealso \code{\link{create_tickers_catalog}}, \code{\link{read_tickers_catalog}}, \code{\link{tickers_catalog-class}}
#'
#' @export
setGeneric("update_tickers_catalog", function(old_tickers_catalog, new_tickers_catalog, ...) {
  standardGeneric("update_tickers_catalog")
})

#' @rdname update_tickers_catalog
setMethod(
  "update_tickers_catalog",
  signature(
    old_tickers_catalog = "tickers_catalog",
    new_tickers_catalog = "tickers_catalog"
  ),
  function(old_tickers_catalog, new_tickers_catalog, ticker_changes = NULL) {

    # Default `ticker_changes` to an empty history
    ####################
    ## Callers that have no renames to report can omit `ticker_changes`; treat
    ## NULL as "no renames this period" rather than requiring an empty data.frame.
    if (is.null(ticker_changes)){
      ticker_changes <- data.frame(new_tickers = character(0), old_tickers = character(0), change_date = as.Date(character(0)))
    }
    ####################

    # Extract slots for comparison between old and new catalogs
    ####################
    ##ticker_change-history
    old_ticker_change_history <- old_tickers_catalog@ticker_change_history

    ##metadata
    ###Get new listed
    new_listed <- new_tickers_catalog@listed
    old_listed <- old_tickers_catalog@listed
    ###Get new and old delisted
    new_delisted <- new_tickers_catalog@delisted
    old_delisted <- old_tickers_catalog@delisted
    ###Get new and old untraded
    new_untraded <- new_tickers_catalog@untraded
    old_untraded <- old_tickers_catalog@untraded
    ###Get new and old current_date
    new_current_date <- new_tickers_catalog@current_date
    old_current_date <- old_tickers_catalog@current_date
    ###Get new and old meta_dataframe_name
    new_meta_dataframe_name <- new_tickers_catalog@meta_dataframe_name
    old_meta_dataframe_name <- old_tickers_catalog@meta_dataframe_name
    ###Get new and old n_days_tolerance
    new_n_days_tolerance <- new_tickers_catalog@n_days_tolerance
    old_n_days_tolerance <- old_tickers_catalog@n_days_tolerance

    ##catalog
    ###Get new and old catalogs
    new_tickers_catalog <- new_tickers_catalog@catalog
    old_tickers_catalog <- old_tickers_catalog@catalog
    ###Extract tickers from new and old catalogs
    new_tickers <- new_tickers_catalog$tickers
    old_tickers <- old_tickers_catalog$tickers

    ## Classify tickers moving between catalogs
    ### A ticker is either a brand-new IPO or a rename (accounted for in
    ### `ticker_changes`); anything in `old_tickers` that isn't in `new_tickers`
    ### and isn't a known rename is treated as missing and must be explained below.
    ###Identify new and missing tickers
    newly_added_tickers <- setdiff(new_tickers, old_tickers) #New tickers (whether IPOs or ticker change)
    ####New tickers that are IPOs (not ticker changes)
    ipos_tickers <- setdiff(newly_added_tickers, ticker_changes$new_tickers)
    ####Old tickers that are not in new tickers and are not old ticker changes
    missing_old_tickers <- setdiff(old_tickers, c(new_tickers, #Tickers that changed names from last period to current (old_tickers_catalog that are missing)
                                                  old_ticker_change_history$old_tickers)) #This allow one to not consider older ticker changes
    ####newly_delisted_tickers
    newly_delisted_tickers <- setdiff(new_delisted, old_delisted) #Delisted tickers


    ####Message
    if (length(newly_added_tickers) > 0){
      crayon::green(message(paste0("Newly added tickers: ", paste(newly_added_tickers, collapse = ", "))))
    }
    if (length(ipos_tickers) > 0){
      crayon::green(message(paste0("Newly added IPOs: ", paste(ipos_tickers, collapse = ", "))))
    }
    if (length(newly_delisted_tickers) > 0){
      crayon::red(message(paste0("Newly delisted tickers: ", paste(newly_delisted_tickers, collapse = ", "))))
    }
    if (length(missing_old_tickers) > 0){
      crayon::yellow(message(paste0("Missing old tickers: ", paste(missing_old_tickers, collapse = ", "))))
    }


    ####################

    # Validate Inputs
    ####################
    ## `ticker_changes` structure and types
    ###structure
    if (!all(c("new_tickers", "old_tickers", "change_date") %in% colnames(ticker_changes))) {
      stop("ticker_changes must contain columns: 'new_tickers', 'old_tickers', and 'change_date'.")
    }
    ###new_ticker and old_ticker columns are character type
    if (!all(sapply(ticker_changes[, c("new_tickers", "old_tickers")], is.character))) {
      stop("Columns 'new_tickers' and 'old_tickers' in ticker_changes must be character type.")
    }
    ###change_date column is Date type
    if (!lubridate::is.Date(ticker_changes$change_date)) {
      stop("Column 'change_date' in ticker_changes must be of Date type.")
    }
    ###can't have NAs
    if (any(is.na(ticker_changes))) {
      stop("ticker_changes can't have NAs.")
    }
    ###can't have duplicate columns
    if (any(duplicated(ticker_changes$new_tickers)) || any(duplicated(ticker_changes$old_tickers))) {
      stop("ticker_changes can't have duplicate columns.")
    }


    ## Validate ticker partition (IPOs vs. renames)
    ### Every newly added ticker must resolve to exactly one bucket -- a fresh
    ### IPO or the renamed successor of a missing old ticker -- otherwise a
    ### ticker appeared without an explanation and the catalog would silently
    ### lose lineage.
    ####Check if newly_added_tickers can't be decomposed into ipos_tickers and missing_old_tickers
    if (!identical(sort(newly_added_tickers), sort(c(ipos_tickers, #completely new
                                                     ticker_changes %>% #ticker changes
                                                     dplyr::filter(old_tickers %in% missing_old_tickers) %>% #old tickers that are missing
                                                     dplyr::pull(new_tickers))))){ #new tickers
      stop("Newly added tickers can't be decomposed into IPOs and missing old tickers")
    }

    ###Validate that all `new_tickers` tickers are accounted for in `ticker_changes`
    if (!all(ticker_changes$new_tickers %in% newly_added_tickers)) {
      stop("All new_tickers in ticker_changes must be present in raw_features_m_df")
    }
    ###Check that the number of rows of ticker_changes is equal to missing_old_tickers length
    if (length(ticker_changes$old_tickers) != length(missing_old_tickers)) {
      stop("Mismatch between new_tickers and missing old tickers in ticker_changes")
    }
    ###Check that delisted tickers are changing ticker
    if (any(ticker_changes$old_tickers %in% old_delisted)) {
      stop("Delisted tickers are changing ticker in ticker_changes. For relistings, treat it as a completely new ticker")
    }
    ###Validate that all `missing_old_tickers` tickers are accounted for in `ticker_changes`
    if (!all(missing_old_tickers %in% ticker_changes$old_tickers) || !all(ticker_changes$old_tickers %in% missing_old_tickers)) {
      stop("Mismatch between missing old tickers in ticker_changes and old tickers present in old_tickers_catalog")
    }
    ## Validate classification-transition business rules
    ### Encodes the only classification transitions considered valid between
    ### snapshots: listed -> {delisted, untraded} and untraded -> delisted are
    ### allowed; skipping straight to/from `listed` without going through the
    ### expected state (or reviving a `delisted` ticker) is not, since it would
    ### indicate an unreported corporate action or a data error upstream.
    ###Check that all new tickers are classified as listed
    if (!all(newly_added_tickers %in% new_listed)) {
      warning("New tickers are not classified as 'listed' in new_tickers_catalog.")
    }
    ###Check that delisted tickers from history are not listed
    if (any(old_delisted %in% new_listed)) {
      stop("Delisted tickers from old_tickers_catalog are now listed in new_tickers_catalog.")
    }
    ###Check that delisted tickers from history are not untraded
    if (any(old_delisted %in% new_untraded)) {
      stop("Delisted tickers from old_tickers_catalog are now untraded in new_tickers_catalog.")
    }
    ###Check that untraded tickers from history are not listed
    if (any(old_untraded %in% new_listed)) {
      stop("Untraded tickers from old_tickers_catalog are now listed in new_tickers_catalog.")
    }
    ###Check that untrade tickers from history are now delisted
    if (any(old_untraded %in% new_delisted)) {
      stop("Untraded tickers from old_tickers_catalog are now delisted in new_tickers_catalog.")
    }
    ###Check that old_listed tickers from history are now untraded
    if (any(old_listed %in% new_untraded)) {
      stop("Listed tickers from old_tickers_catalog are now untraded in new_tickers_catalog.")
    }

    ## Validate date consistency between catalogs
    ### Guards against look-ahead bias and silent restatement: `tickers_first_quote`
    ### must be stable for tickers common to both catalogs, `tickers_last_quote`
    ### must not move for already-delisted tickers, and an IPO's first quote
    ### can't predate the *previous* snapshot's `current_date` (net of
    ### tolerance) -- otherwise the update would be injecting information the
    ### old catalog couldn't have known about.
    ###tickers_first_quote (it should not change, as each tickers_first_quote will keep delisted tickers)
    common_tickers_first_quote <- dplyr::inner_join(
      old_tickers_catalog %>% dplyr::select(tickers, tickers_first_quote),
      new_tickers_catalog %>% dplyr::select(tickers, tickers_first_quote),
      by = "tickers"
    ) %>%
      dplyr::filter(!tickers %in% old_ticker_change_history$new_tickers) #Do not consider ticker changes

    ####Check equality considering NAs
    date_mismatch <- !(
      (is.na(common_tickers_first_quote$tickers_first_quote.x) & is.na(common_tickers_first_quote$tickers_first_quote.y)) |
        (common_tickers_first_quote$tickers_first_quote.x == common_tickers_first_quote$tickers_first_quote.y)
    )
    if (any(date_mismatch, na.rm = TRUE)) {
      stop("Mismatch in tickers_first_quote between common tickers in old and new catalogs.",
           "For relistings, use 'old_ticker'_n, where n is relisting_number")
    }
    ###ticker_last_quote should not change for delisted tickers
    delisted_tickers_last_quote <- dplyr::inner_join(
      old_tickers_catalog %>% dplyr::filter(tickers %in% old_delisted) %>% dplyr::select(tickers, tickers_last_quote),
      new_tickers_catalog %>% dplyr::filter(tickers %in% old_delisted) %>% dplyr::select(tickers, tickers_last_quote),
      by = "tickers"
    )
    ####Check equality
    date_mismatch <- !(
      (delisted_tickers_last_quote$tickers_last_quote.x == delisted_tickers_last_quote$tickers_last_quote.y)
    )
    if (any(date_mismatch, na.rm = TRUE)) {
      stop("Mismatch in tickers_last_quote between delisted tickers in old and new catalogs.",
           "For relistings, use 'old_ticker'_n, where n is relisting_number")
    }


    ###tickers_first_quote of new tickers should be higher than old_current_date
    if (length(ipos_tickers) > 0){
      ####Get tickers_first_quote of new tickers
      ipos_tickers_tickers_first_quote <- new_tickers_catalog %>%
        dplyr::filter(tickers %in% ipos_tickers) %>%
        dplyr::pull(tickers_first_quote)

      ####Check if tickers_first_quote of ipo tickers is either >= old_current_date or NA
      if (!all(is.na(ipos_tickers_tickers_first_quote) |
               ipos_tickers_tickers_first_quote >= (old_current_date - new_n_days_tolerance))) {
        stop("tickers_first_quote of IPOs in new_tickers_catalog should be either >= old_current_date or NA.")
      }
    }

    ###max tickers_last_quote in new_tickers_catalog should be higher than in old_catalog
    if (max(new_tickers_catalog$tickers_last_quote, na.rm = TRUE) <= max(old_tickers_catalog$tickers_last_quote, na.rm = TRUE)) {
      stop("tickers_last_quote in new_tickers_catalog should be higher than in old_tickers_catalog.")
    }
    ###current_date in new_tickers_catalog = current_date in old_tickers_catalog + 1
    if (new_current_date != lubridate::add_with_rollback(old_current_date, months(1))) {
      stop("current_date in new_tickers_catalog should be equal to current_date in old_tickers_catalog plus 1")
    }
    ## Validate object-level metadata consistency
    ### `meta_dataframe_name` must show lineage (new name contains the old one)
    ### and `n_days_tolerance` should not silently drift between updates.
    ###check that meta_dataframe_name in new_tickers_catalog is contained in meta_dataframe_name of oold_tickers_catalog
    if (!grepl(old_meta_dataframe_name, new_meta_dataframe_name)) {
      stop("meta_dataframe_name in new_tickers_catalog should contain meta_dataframe_name in old_tickers_catalog.")
    }
    ##n_days_tolerance
    ###n_days_tolerance should be the same in old_tickers_catalog and new_tickers_catalog
    if (old_n_days_tolerance != new_n_days_tolerance) {
      warning("n_days_tolerance has changed from old_tickers_catalog and new_tickers_catalog.")
    }

    ## Validate `ticker_changes` against catalog dates (rename-boundary consistency)
    ### For each rename: the new ticker's first quote can't precede
    ### `change_date`; the old ticker's recorded first/last quote can't be
    ### tightened by the rename; a ticker that was `listed` pre-rename must
    ### still have non-NA quote dates post-rename, one that was `untraded`
    ### must still be NA, and a `delisted` ticker must not be renamed at all
    ### (relistings are new tickers).
    if (nrow(ticker_changes) >= 0) {
      ###ticker_first_quote < change_date
      only_newly_changed_tickers_catalog <- new_tickers_catalog %>%
        dplyr::filter(tickers %in% ticker_changes$new_tickers) %>% #Filter only new tickers
        dplyr::select(tickers, tickers_first_quote, tickers_last_quote) %>% #Get their date first quote
        dplyr::left_join(ticker_changes, by = c("tickers" = "new_tickers")) #Join with ticker_changes to get change_date

      ####Check if tickers_first_quote is different to change_date
      if (!all(is.na(only_newly_changed_tickers_catalog$tickers_first_quote) |
               only_newly_changed_tickers_catalog$tickers_first_quote <= only_newly_changed_tickers_catalog$change_date)) {
        stop("tickers_first_quote in new_tickers_catalog should be <= than change_date in ticker_changes for new tickers.")
      }

      ###ticker_first_quote <= old_first_quote (< when there has been more than one change in history)
      ###tickers_last_quote >= old_tickers_last_quote
      only_newly_changed_tickers_catalog <- only_newly_changed_tickers_catalog %>%
        dplyr::right_join(old_tickers_catalog %>%  #Right join with old catalog to get original first dates
                            dplyr::right_join(ticker_changes, by = c("tickers" = "old_tickers")) %>%
                            dplyr::rename(old_tickers_first_quote = tickers_first_quote,
                                          old_tickers_last_quote = tickers_last_quote) %>%
                            dplyr::select(tickers, old_tickers_first_quote, old_tickers_last_quote), #Rename to facilitate comparison
                          by = c("old_tickers" = "tickers"))

      ####Check if tickers_first_quote is <= to tickers_first_quote of old_tickers_catalog (remember: first_quote <- change_date)
      if (any(
        !is.na(only_newly_changed_tickers_catalog$tickers_first_quote) &
        !is.na(only_newly_changed_tickers_catalog$old_tickers_first_quote) &
        only_newly_changed_tickers_catalog$tickers_first_quote > only_newly_changed_tickers_catalog$old_tickers_first_quote)){
        stop("tickers_first_quote in new_tickers_catalog should be <= tickers_first_quote in old_tickers_catalog for new tickers.")
      }
      ####Check if tickers_last_quote is >= to tickers_old_quote of old_tickers_catalog
      if (any(
        !is.na(only_newly_changed_tickers_catalog$tickers_last_quote) &
        !is.na(only_newly_changed_tickers_catalog$old_tickers_last_quote) &
        only_newly_changed_tickers_catalog$tickers_last_quote < only_newly_changed_tickers_catalog$old_tickers_last_quote)){
        stop("tickers_last_quote in new_tickers_catalog should be >= tickers_last_quote in old_tickers_catalog for new tickers.")
      }

      ###old_listed changing to NA
      old_listed_changed_tickers_catalog <- only_newly_changed_tickers_catalog %>% dplyr::filter(old_tickers %in% old_listed)
      ####Check that tickers_first_quote and tickers_last_quote are not NA for tickers that were listed in old_tickers_catalog
      if (nrow(old_listed_changed_tickers_catalog) > 0 &&
          any(is.na(old_listed_changed_tickers_catalog %>% dplyr::pull(tickers_first_quote)) |
              is.na(old_listed_changed_tickers_catalog %>% dplyr::pull(tickers_last_quote)))
      ){
        stop("tickers_first_quote and tickers_last_quote should not be NA for tickers that were listed in old_tickers_catalog.")
      }
      ###untraded changing from NA
      old_untraded_changed_tickers_catalog <- only_newly_changed_tickers_catalog %>% dplyr::filter(old_tickers %in% old_untraded)
      ####Check that tickers_first_quote and tickers_last_quote are NA for tickers that were untraded in old_tickers_catalog
      if (nrow(old_untraded_changed_tickers_catalog) > 0 &&
          any(!is.na(old_untraded_changed_tickers_catalog %>% dplyr::pull(tickers_first_quote)) |
              !is.na(old_untraded_changed_tickers_catalog %>% dplyr::pull(tickers_last_quote)))
      ){
        stop("tickers_first_quote and tickers_last_quote should be NA for tickers that were untraded in old_tickers_catalog.")
      }
      ###delisted having any change
      if (length(only_newly_changed_tickers_catalog %>% dplyr::filter(tickers %in% old_delisted) %>% dplyr::pull(tickers)) > 0){
        stop("tickers that were delisted in old_tickers_catalog should not change tickers.")
      }
    }

    ####################

    # Reassign `perm_id` across renames
    ####################
    ## A renamed ticker keeps its predecessor's `perm_id` (looked up through
    ## the full history, not just this call's `ticker_changes`) so a single
    ## company keeps one identity across ticker symbol changes -- this is what
    ## lets downstream joins key on `perm_id` instead of a mutable ticker string.
    ##Bind tickers_changes to history, so as to keep perm_id for tickers that once changed
    ticker_changes_full_history <- dplyr::bind_rows(old_ticker_change_history, ticker_changes)

    ##Map old tickers to perm_id
    perm_id_map <- old_tickers_catalog %>%
      dplyr::select(tickers, perm_id) %>%
      dplyr::right_join(ticker_changes_full_history, by = c("tickers" = "old_tickers"))

    ##Change perm_id for renamed tickers
    new_tickers_catalog <- new_tickers_catalog %>%
      dplyr::left_join( #Join with perm_id_map
        perm_id_map %>%
          dplyr::select(new_tickers, perm_id, change_date) %>% #Select only new_ticker and perm_id
          dplyr::rename(old_perm_id = perm_id), #Rename old_perm_id to perm_id
        by = c("tickers" = "new_tickers")) %>%
      dplyr::mutate(perm_id = dplyr::coalesce(old_perm_id, perm_id)) %>% #Get perm_id from old_perm_id if it exists, otherwise, get perm_id from perm_id
      dplyr::mutate(tickers_first_quote = dplyr::coalesce(change_date, tickers_first_quote)) %>% #Get change_date if it exists, otherwise, get tickers_first_quote
      dplyr::select(-old_perm_id, -change_date) #Remove old_perm_id and change_date

    ####################

    # Preserve pre-rename ticker rows as historical ("old") entries
    ####################
    ## Old tickers that were renamed are kept in the catalog (old = TRUE,
    ## listed/delisted/untraded = FALSE) instead of being dropped, so ticker
    ## history remains queryable without duplicating the live row under the new symbol.
    ##Get old entries, set them as either delisted or untraded
    old_tickers_entries <- old_tickers_catalog %>%
      dplyr::select(-dplyr::any_of("change_date")) %>% #Remove change_date columns if it exists
      dplyr::left_join(ticker_changes_full_history, by = c("tickers" = "old_tickers")) %>% #Join with ticker_changes
      ###Consider only entries that have changed names
      dplyr::filter(!is.na(new_tickers)) %>%
      dplyr::mutate(tickers_last_quote = change_date) %>% #Set tickers_last_quote to change_date
      dplyr::left_join(new_tickers_catalog %>%
                         dplyr::right_join(ticker_changes_full_history, by = c("tickers" = "new_tickers")) %>% #Join with ticker_changes
                         dplyr::rename(new_tickers_last_quote = tickers_last_quote) %>%
                         dplyr::select(tickers, new_tickers_last_quote), #Select perm_id, tickers, and new_date_last_quote
                       by = c("new_tickers" = "tickers")) %>%
      dplyr::mutate(
        listed = FALSE, #Set listed to FALSE
        delisted = FALSE, #Set delisted to FALSE
        untraded = FALSE, #Set untraded to FALSE
        old = TRUE #Set old to TRUE
      ) %>%
      dplyr::select(-new_tickers, -new_tickers_last_quote, -change_date)
    ##Bind
    new_tickers_catalog <- dplyr::bind_rows(new_tickers_catalog, old_tickers_entries) %>% dplyr::arrange(perm_id)
    ####################

    # Merge and deduplicate ticker-change history
    #####################
    if (!is.null(old_ticker_change_history) && nrow(old_ticker_change_history) > 0) {
      ##Add general ticker_changes
      updated_ticker_changes <- dplyr::bind_rows(old_ticker_change_history, ticker_changes) %>%
        dplyr::arrange(change_date) %>% # Keep chronological order
        dplyr::distinct(new_tickers, old_tickers, change_date, .keep_all = TRUE) # Ensure unique entries
    } else {
      updated_ticker_changes <- ticker_changes
    }
    #####################

    # Assemble and return the updated `tickers_catalog`
    ####################
    ##Prepare tickers_catalog_slots
    tickers_catalog_slots <- prepare_tickers_catalog_slots(new_tickers_catalog)

    updated_catalog <-  methods::new("tickers_catalog",
                                     catalog = tickers_catalog_slots$tickers_catalog,
                                     tickers = tickers_catalog_slots$tickers,
                                     perm_id = tickers_catalog_slots$perm_id,
                                     tickers_first_quote = tickers_catalog_slots$tickers_first_quote,
                                     tickers_last_quote = tickers_catalog_slots$tickers_last_quote,
                                     untraded = tickers_catalog_slots$untraded,
                                     delisted = tickers_catalog_slots$delisted,
                                     listed = tickers_catalog_slots$listed,
                                     old = tickers_catalog_slots$old,
                                     current_date = new_current_date,
                                     meta_dataframe_name = old_meta_dataframe_name,
                                     n_days_tolerance = old_n_days_tolerance,
                                     ticker_change_history = updated_ticker_changes)

    return(updated_catalog)
  }
)






# meta_xts----------------------------------------------------------------

#' Create a `meta_xts` Object (`returns_meta_xts` or `metrics_meta_xts`)
#'
#' @description
#' Constructs a \code{\link{returns_meta_xts-class}} or \code{\link{metrics_meta_xts-class}}
#' object from an \code{xts} object, a \code{data.frame} (wide or long format), or a
#' \code{meta_dataframe}, auto-detecting frequency and filling in metadata slots.
#'
#' @param data An \code{xts} object, a \code{data.frame}, or a \code{meta_dataframe}
#'   containing the time series data.
#' @param type Character. Either \code{"returns"} (built as \code{returns_meta_xts},
#'   no holes allowed) or \code{"metrics"} (built as \code{metrics_meta_xts}, holes
#'   allowed). Defaults to \code{"returns"} (the first value of
#'   \code{c("returns", "metrics")}, resolved via \code{match.arg()}).
#' @param asset_type Character. Type of asset for \code{returns_meta_xts} objects
#'   (e.g. \code{"stock"}, \code{"ports"}). Defaults to \code{"not_identified"},
#'   which emits a \code{message()} when \code{type = "returns"}.
#' @param meta_xts_name Character. A label for the resulting object(s). Defaults to
#'   \code{"not_identified"}.
#' @param metric_name Character. Name of the metric/return series. If \code{NULL},
#'   defaults to \code{"returns"}/\code{"metrics"} for \code{xts}/wide-\code{data.frame}
#'   input, or to the feature column name(s) for long-\code{data.frame} input. A
#'   vector supplied for long input must match the number of feature columns in length.
#' @param workflow An ANY object recording processing history. Defaults to \code{NULL}.
#'   For \code{meta_dataframe} input, the source object's own \code{workflow} is
#'   carried over with a coercion entry appended.
#' @param source A character vector indicating data origin for each column.
#'   If \code{NULL}, defaults to \code{"not_identified"} repeated for each column.
#' @param data_format Character. \code{data.frame} input only: \code{"wide"}
#'   (columns are already assets/metrics) or \code{"long"} (requires \code{tickers}
#'   and \code{dates} columns; each non-id column becomes a separate result).
#'   Defaults to \code{"wide"}.
#' @param dates An optional vector of dates, sorted ascending. For wide input it
#'   supplies the time index directly; for long input it overrides the pivoted
#'   \code{dates} column. Unsorted input is an error rather than being silently
#'   re-sorted, so data the caller believes is already aligned is never reordered
#'   without their knowledge.
#' @param ... Additional arguments passed to the underlying method.
#'
#' @return
#' A single \code{returns_meta_xts} or \code{metrics_meta_xts} object, **except**
#' when \code{data} is a \code{data.frame} in long format with more than one
#' feature column: then a named \code{list} of such objects is returned, one per
#' feature column, named after the column.
#'
#' @seealso \code{\link{meta_xts-class}}, \code{\link{returns_meta_xts-class}}, \code{\link{metrics_meta_xts-class}}, \code{\link{create_meta_dataframe}}
#'
#' @export
setGeneric(
  "create_meta_xts",
  function(data, type = c("returns", "metrics"), asset_type = "not_identified", meta_xts_name = "not_identified",
           metric_name = NULL, workflow = NULL, source = NULL, ...) {
    standardGeneric("create_meta_xts")
  }
)


#' @rdname create_meta_xts
# Dispatch: 'data' is an 'xts' object -- the base case the other methods delegate to
setMethod(
  "create_meta_xts",
  signature(data = "xts"),
  function(data,
           type = c("returns", "metrics"),
           asset_type = "not_identified",
           meta_xts_name = "not_identified",
           metric_name = NULL,
           workflow = NULL,
           source = NULL) {

    # Match 'type' argument
    type <- match.arg(type)

    # If source is NULL, default to a vector of "not_identified"
    # repeated for each column in 'data'.
    if (is.null(source)) {
      source <- rep("not_identified", ncol(data))
    }

    # Detect frequency automatically
    freq_info <- suppressWarnings(xts::periodicity(data))
    discovered_scale <- if (nrow(data) == 1) "not_available" else freq_info$scale

    #Current date
    current_date <- zoo::index(data)[length(zoo::index(data))]

    # Common slots for the parent class
    common_slots <- list(
      data          = data,
      meta_xts_name = meta_xts_name,
      workflow      = workflow,
      n_dates       = nrow(data),
      source        = source,
      frequency     = discovered_scale,
      current_date  = current_date
    )

    # 5) Depending on 'type', build the appropriate subclass
    if (type == "returns") {
      # For assets_meta_xts, we fill the specialized slots:
      if (asset_type == "not_identified") message("Asset_type not identified for 'returns_meta_xts' subclass")

      obj <- methods::new(
        "returns_meta_xts",
        data = common_slots$data,
        asset_type = asset_type,
        meta_xts_name = common_slots$meta_xts_name,
        metric_name = if (is.null(metric_name)) "returns" else metric_name,
        workflow = common_slots$workflow,
        n_dates = common_slots$n_dates,
        source = common_slots$source,
        frequency = common_slots$frequency,
        assets = colnames(data),
        n_assets = ncol(data),
        current_date = current_date
      )
    } else { # type == "metrics"
      # For metrics_meta_xts, we fill the specialized slots:
      obj <- methods::new(
        "metrics_meta_xts",
        data = common_slots$data,
        meta_xts_name = common_slots$meta_xts_name,
        metric_name = if (is.null(metric_name)) "metrics" else metric_name,
        workflow = common_slots$workflow,
        n_dates = common_slots$n_dates,
        source = common_slots$source,
        frequency = common_slots$frequency,
        series = colnames(data),
        n_series = ncol(data),
        current_date = current_date
      )
    }

    # Return the newly created object
    return(obj)
  })

#' @rdname create_meta_xts
# Define the method for when 'data' is a data.frame
setMethod(
  "create_meta_xts",
  signature(data = "data.frame"),
  function(data,
           type = c("returns", "metrics"),
           asset_type = "not_identified",
           meta_xts_name = "not_identified",
           metric_name = NULL,
           workflow = NULL,
           source = NULL,
           data_format = c("wide", "long"),
           dates = NULL) {

    data_format <- match.arg(data_format)
    type <- match.arg(type)

    if (data_format == "wide") {
      # For wide format, first check if 'dates' argument is provided.
      if (!is.null(dates)) {
        date_vec <- as.Date(dates)
        # Enforce that user-supplied dates are sorted
        if (is.unsorted(date_vec)) {
          stop("Error: 'dates' vector provided by the user must be sorted in ascending order.")
        }
        # Do NOT reorder data here — assume the user supplied aligned dates
      } else if ("dates" %in% colnames(data)) {
        # Sort data based on dates column, and extract date vector
        data <- data[order(as.Date(data$dates)), , drop = FALSE]
        date_vec <- as.Date(data[["dates"]])
        data <- data[, setdiff(colnames(data), "dates"), drop = FALSE]
      } else {
        stop("Error: No 'dates' column found. Please provide a 'dates' column or pass a 'dates' argument.")
      }

      # Error check: ensure we have valid dates
      if (length(date_vec) == 0 || all(is.na(date_vec))) {
        stop("Error: No valid dates found. Please provide a 'dates' column, valid rownames, or pass a 'dates' argument.")
      }

      #Create xts
      data_xts <- xts::as.xts(data, order.by = date_vec)

      # Delegate to the xts method
      return(create_meta_xts(data = data_xts,
                             type = type,
                             asset_type = asset_type,
                             meta_xts_name = meta_xts_name,
                             metric_name = metric_name,
                             workflow = workflow,
                             source = source))

    } else { # data_format == "long"
      # For long format, verify required columns exist.
      if (!all(c("tickers", "dates") %in% colnames(data))) {
        stop("Error: For long format, the data.frame must contain 'tickers' and 'dates' columns.")
      }
      # Identify feature columns (excluding 'id', tickers' and 'dates')
      feature_cols <- setdiff(colnames(data), c("id", "tickers", "dates"))

      ##Check that metric_name legnth is equal to feature_cols length
      if (!is.null(metric_name) && length(metric_name) != length(feature_cols)) {
        stop("Error: When data_format is 'long' and metric_name is provided as a vector, its length must equal the number of feature columns.")
      }
      result_list <- list()

      for (feat in feature_cols) {

        # If user did not provide dates, sort data first by 'dates'
        if (is.null(dates)) {
          data <- data[order(as.Date(data$dates)), , drop = FALSE]
        }

        # Pivot the data: tickers become columns and dates are the id column
        wide_df <- tidyr::pivot_wider(data,
                                      id_cols = dates,
                                      names_from = tickers,
                                      values_from =  dplyr::all_of(feat))
        # Use provided dates if available; otherwise use the pivoted 'dates' column.
        if (!is.null(dates)) {
          date_vec <- as.Date(dates)

          # Enforce that user-supplied dates are sorted
          if (is.unsorted(date_vec)) {
            stop("Error: 'dates' vector provided by the user must be sorted in ascending order.")
          }
          # No reordering done — assume user gave aligned data
        } else {
          date_vec <- as.Date(wide_df$dates)
          wide_df <- wide_df[order(date_vec), , drop = FALSE]
          date_vec <- date_vec[order(date_vec)]
        }

        # Error check for valid dates in the long branch
        if (length(date_vec) == 0 || all(is.na(date_vec))) {
          stop("Error: No valid dates found in pivoted data. Please check your 'dates' column or provide a 'dates' argument.")
        }

        # Remove the 'dates' column from the wide_df
        wide_data <- wide_df[, setdiff(colnames(wide_df), "dates"), drop = FALSE]

        # Create xts object
        data_xts <- xts::as.xts(wide_data, order.by = date_vec)

        # Set the metric name: use the feature name if metric_name is NULL,
        # or append the feature name to the provided metric_name.
        current_metric_name <- if (is.null(metric_name)) feat else paste(metric_name, feat, sep = "_")

        # Create the meta_xts object using the xts method.
        obj <- create_meta_xts(data = data_xts,
                               type = type,
                               asset_type = asset_type,
                               meta_xts_name = meta_xts_name,
                               metric_name = current_metric_name,
                               workflow = workflow,
                               source = source)
        result_list[[feat]] <- obj
      }

      # Return a single object if only one feature is present; otherwise, return the list.
      if (length(result_list) == 1) {
        return(result_list[[1]])
      } else {
        return(result_list)
      }
    }
  }
)

#' @rdname create_meta_xts
# Define the method for when 'data' is a meta_dataframe
setMethod(
  "create_meta_xts",
  signature(data = "meta_dataframe"),
  function(data,
           type = c("returns", "metrics"),
           asset_type = "not_identified",
           meta_xts_name = "not_identified",
           metric_name = NULL,
           source = NULL) {

    #Retrieve data.frame
    meta_dataframe <- data@data

    #Create new workflow entry
    new_entry <- list(
      list(
        current_date = data@current_date, #Current date
        timestamp = Sys.time() #Timestamp
      )
    )

    updated_workflow <- c(data@workflow, new_entry)
    names(updated_workflow)[length(updated_workflow)] <- paste0("meta_xts_coercion", data@current_date)

    #Pass to 'data.frame' method
    results <- create_meta_xts(data = meta_dataframe,
                               type = type,
                               asset_type = asset_type,
                               meta_xts_name = meta_xts_name,
                               metric_name = metric_name,
                               workflow = updated_workflow,
                               data_format = "long",
                               source = source,
                               dates = NULL)

    return(results)

  }
)


# ss_backtest------------------------------------------------------------

#' @title Create an ss_backtest_config Object
#' @description This function constructs an object of class `ss_backtest_config`, ensuring the proper initialization
#' and validation of its slots.
#' @param initial_sample_size A numeric indicating the minimum number of observations required to begin the backtest.
#' @param rebalancing_months A numeric vector of calendar months (each in 1–12) at which signal selection is executed during the walk-forward backtest.
#' @param active_returns Logical, whether to calculate active returns when calculating performance metrics, except for CAPM (default is TRUE).
#' @param split_method A character string specifying the splitting method, either "expanding" (default) or "rolling".
#' @param alpha_test_strategy An `alpha_test_strategy` object — a `frequentist_alpha_test_strategy` or `bayesian_alpha_test_strategy` built with `create_alpha_test_strategy()` — defining the alpha test configuration. May be `NULL`.
#' @param config_name A character string naming the configuration.
#' @param chosen_signals_and_positions A character vector specifying the chosen signals and positions. If set to "all", all signals in `signals_m_df` will be used, and a long position will be assumed for all.
#' @return An object of class `ss_backtest_config`.
#' @examples
#' \dontrun{
#' alpha_strategy <- create_alpha_test_strategy(
#'   model_structure = "no_pooled", p_correction_method = "holm",
#'   market_factor_proxy = "IBOV"
#' )
#' ss_config <- create_ss_backtest_config(
#'   initial_sample_size = 36, rebalancing_months = 1:12,
#'   alpha_test_strategy = alpha_strategy, config_name = "ss_holm_nopool"
#' )
#' }
#' @export
create_ss_backtest_config <- function(
    initial_sample_size,
    rebalancing_months,
    active_returns = TRUE,
    split_method = "expanding",
    alpha_test_strategy = NULL,
    config_name = "not_identified",
    chosen_signals_and_positions = "all") {
  # Message
  if (length(chosen_signals_and_positions) == 1 && chosen_signals_and_positions == "all") {
    message("chosen_signals_and_positions set as 'all'. All signals in signals_m_df will be used and a long position will be assumed to all.")
  }
  # Input validation
  if (initial_sample_size < 0) {
    stop("initial_sample_size cannot be negative.")
  }

  if (!split_method %in% c("expanding", "rolling")) {
    stop("split_method must be either 'expanding' or 'rolling'.")
  }

  # Create and return the object
  methods::new("ss_backtest_config",
               chosen_signals_and_positions = chosen_signals_and_positions,
               initial_sample_size = initial_sample_size,
               rebalancing_months = rebalancing_months,
               active_returns = active_returns,
               split_method = split_method,
               alpha_test_strategy = alpha_test_strategy,
               config_name = config_name
  )
}


# alpha_test_strategy----------------------------------------------------
#' @title Create an alpha_test_strategy object
#' @description
#' Constructor for objects of class `alpha_test_strategy` or its subclasses
#' (`frequentist_alpha_test_strategy`, `bayesian_alpha_test_strategy`).
#' The function validates the model configuration and constructs the appropriate strategy object
#' based on the hypothesis testing methodology and hierarchical model structure.
#'
#' @param model_structure Character string specifying the hierarchical model structure.
#'   Must be one of:
#'   \itemize{
#'     \item `"partial_pooled"`: Uses theme-level effects with partial pooling.
#'     \item `"no_pooled"`: No pooling; estimates signal-level parameters independently.
#'   }
#'
#' @param theme_level_intercept Character string indicating how intercepts are modeled at the theme level
#'   (only used when `model_structure = "partial_pooled"`). Valid options are:
#'   \itemize{
#'     \item `"fixed"`: A common intercept across themes.
#'     \item `"random"`: Intercepts modeled as random effects.
#'     \item `"theme_specific"`: Each theme has its own fixed intercept.
#'   }
#'   Must be `NULL` if `model_structure = "no_pooled"`.
#'
#' @param theme_level_slope Character string indicating how slopes are modeled at the theme level
#'   (only used when `model_structure = "partial_pooled"`). Valid options are:
#'   \itemize{
#'     \item `"fixed"`: A common slope across themes.
#'     \item `"theme_specific"`: Each theme has its own slope.
#'   }
#'   Must be `NULL` if `model_structure = "no_pooled"`.
#'
#' @param signal_significance_threshold Numeric. Significance level for alpha tests (e.g., 0.05).
#'   Determines the rejection region for the null hypothesis of zero alpha. Must be between 0 and 1.
#'
#' @param p_correction_method Character string specifying the p-value adjustment method used
#'   in multiple hypothesis testing correction. Options include:
#'   \itemize{
#'     \item `"none"`: No correction.
#'     \item `"bonferroni"`, `"holm"`, `"hochberg"`, `"hommel"`: Classical FWER control methods.
#'     \item `"BH"`, `"fdr"`, `"BY"`: FDR control methods.
#'     \item `"bayesian"`: Use Bayesian hypothesis testing instead of p-values.
#'   }
#'
#' @param market_factor_proxy Character string specifying the identifier of the market factor proxy.
#'   This variable is typically used as the market return in a CAPM-style alpha test model.
#'
#' @param bayesian_model_parameters An optional object of class `bayesian_model_parameters`.
#'   If `p_correction_method = "bayesian"`, this must either be provided explicitly or will be initialized
#'   with default (uninformative) prior settings. Ignored for frequentist strategies.
#'
#' @param enable_theme_representativeness Logical. If `TRUE`, enables extra diagnostics and modeling
#'   logic to account for representativeness at the theme level. Useful when themes are considered
#'   key grouping variables for inference.
#'
#' @param lmer_control Optional. A list of control parameters passed to `lme4::lmer()` (frequentist)
#'   or `brms::brm()` (Bayesian) for customizing model fitting behavior. Can include convergence
#'   tolerances, optimizers, or verbosity flags.
#'
#' @return An object of class:
#'   \itemize{
#'     \item \code{frequentist_alpha_test_strategy}, if a frequentist method is selected;
#'     \item \code{bayesian_alpha_test_strategy}, if Bayesian inference is selected.
#'   }
#'   The returned object can then be passed to downstream workflows that evaluate and classify alpha signals.
#'
#' @export
create_alpha_test_strategy <- function(
    model_structure = "no_pooled",
    theme_level_intercept = NULL,
    theme_level_slope = NULL,
    signal_significance_threshold = 0.05,
    p_correction_method = "none",
    market_factor_proxy,
    bayesian_model_parameters = NULL,
    enable_theme_representativeness = TRUE,
    lmer_control = NULL) {
  # Validate input arguments
  if (!p_correction_method %in% c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "fdr", "BY", "bayesian")) {
    stop("Invalid p_correction_method. Must be one of: 'none', 'bonferroni', 'holm', 'hochberg', 'hommel', 'BH', 'fdr', 'BY', 'bayesian'.")
  }
  if (signal_significance_threshold < 0 || signal_significance_threshold > 1) {
    stop("signal_significance_threshold must be between 0 and 1.")
  }
  if (missing(market_factor_proxy) || !is.character(market_factor_proxy) || length(market_factor_proxy) != 1) {
    stop("market_factor_proxy must be a single character string.")
  }
  if (!model_structure %in% c("partial_pooled", "no_pooled")) {
    stop("Currently, model_structure must be one of partial_pooled or no_pooled")
  }
  if (model_structure == "partial_pooled") {
    if (is.null(theme_level_intercept) || !theme_level_intercept %in% c("fixed", "random", "theme_specific")) {
      stop("theme_level_intercept must be 'fixed', 'random' or 'theme_specific'")
    }
    if (is.null(theme_level_slope) || !theme_level_slope %in% c("fixed", "theme_specific")) {
      stop("Currently, theme_level_slope can only be 'fixed' or 'theme_specific'")
    }
    avaiable_combinations <- c(
      c("random_intercept_fixed_slope"), # old random_intercept
      c("theme_specific_intercept_fixed_slope"), # old fixed_intercepts
      c("theme_specific_intercept_theme_specific_slope"), # old fixed_intercepts_fixed_slopes
      c("fixed_intercept_fixed_slope")
    ) # one none
    chosen_combination <- paste0(theme_level_intercept, "_intercept_", theme_level_slope, "_slope")

    if (!chosen_combination %in% avaiable_combinations) {
      stop("Chosen combination of theme_level_intercept and theme_level_slope is currently not supported.")
    }
  } else {
    if (any(!is.null(theme_level_intercept), !is.null(theme_level_slope))) {
      stop("Theme-level parameters are only avaiable for partial pooled models.")
    }
  }

  # Handle Bayesian subclass creation
  if (p_correction_method == "bayesian") {
    if (model_structure != "partial_pooled") {
      stop("Currently, only the 'partial_pooled' model structure is supported for Bayesian alpha testing.")
    }
    if (!is.null(bayesian_model_parameters) && !inherits(bayesian_model_parameters, "bayesian_model_parameters")) {
      stop("When p_correction_method is 'bayesian', bayesian_model_parameters must be a bayesian_model_parameters object.")
    }

    # Check if a bayesian_model_parametesr is being provided
    if (is.null(bayesian_model_parameters)) {
      # If not create a generic one
      bayesian_model_parameters <-  methods::new("bayesian_model_parameters",
                                                 user_priors = NULL,
                                                 prior_derivation_control = NULL,
                                                 brms_control = NULL
      )
    }

    return( methods::new("bayesian_alpha_test_strategy",
                         signal_significance_threshold = signal_significance_threshold,
                         p_correction_method = p_correction_method,
                         model_structure = model_structure,
                         theme_level_intercept = theme_level_intercept,
                         theme_level_slope = theme_level_slope,
                         market_factor_proxy = market_factor_proxy, # For a new bayesian class, create an uniformative bayesian_model_parameters
                         enable_theme_representativeness = enable_theme_representativeness,
                         bayesian_model_parameters = bayesian_model_parameters,
                         lmer_control = lmer_control
    ))
  }

  # Handle Frequentist subclass creation
  if (p_correction_method %in% c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "fdr", "BY")) {
    return(methods::new("frequentist_alpha_test_strategy",
                        signal_significance_threshold = signal_significance_threshold,
                        model_structure = model_structure,
                        theme_level_intercept = theme_level_intercept,
                        theme_level_slope = theme_level_slope,
                        p_correction_method = p_correction_method,
                        enable_theme_representativeness = enable_theme_representativeness,
                        market_factor_proxy = market_factor_proxy,
                        lmer_control = lmer_control
    ))
  }

  # Default fallback (should not reach here due to prior validation)
  stop("Unexpected error in create_alpha_test_strategy. Check input parameters.")
}


#' @title Add Alpha Test Strategy to ss_backtest_config
#' @description This method allows you to add an `alpha_test_strategy` object to an `ss_backtest_config` object.
#' @param object An `ss_backtest_config` object.
#' @param alpha_test_strategy An `alpha_test_strategy` object to be added.
#' @param model_structure A character describing the model structure.
#' @param signal_significance_threshold A decimal indicating the hypothesis testing negative-alpha null-hypothesis rejection criteria. If one wants to select all chosen_signals,
#' provide 1. In any case, a signal being selected demands a significant CAPM alpha.
#' @param p_correction_method The method for p-value correction. Possible options are:
#'\itemize{
#'  \item{"none"}: No correction.
#'  \item{"bayesian"}: When bayesian is set, a hierarchical mixed-effects bayesian linear model is fitted to the data, using the `brms` package,
#'  which is an interface to the `Stan` probabilistic programming language.
#'  The user can also choose one of the following frequentist methods, which will control Family-Wise Error Rate (FWER) or the False Discovery Rate (FDR).
#'  FDR is less stringent than FWER.
#'  For FWER, possible options are:
#'  \item{"bonferroni"}: Bonferroni correction, which is dominated by Holm's method.
#'  \item{"holm"}: Holm's (1979) method.
#'  \item{"hochberg"}: Hochberg's (1988) method, valid when hypothesis tests are independent or non-negatively associated. Less powerful than Hommel's (1988) method, but
#'  faster to compute.
#'  \item{"hommel"}: Hommel's (1988) method, also valid when hypothesis tests are independent or non-negatively associated, but is more powerful than Hochberg (1988).
#'  For FDR, possible options are:
#'  \item{"BH" or "fdr"}: Benjamini-Hochberg (1995) procedure.
#'  \item{"BY"}: Benjamini-Yekutieli (2001) procedure.
#'  }
#' @param market_factor_proxy A character string indicating the market factor proxy to be used in the CAPM model.
#' Should correspond to one of the columns in `benchmark_returns_df`.
#' @param bayesian_model_parameters An object of class `bayesian_model_parameters`, containing the
#' parameters needed to build the hierarhicical bayesian model and specify its priors.
#' @param enable_theme_representativeness A logical indicating whether, if a given theme in `signal_themes_m_df` does not have any eligible signal, the signal
#' with highest alpha t-stat should be elected.
#' @param lmer_control A list containing control parameters for the `lmer` function.
#' @param theme_level_intercept A character indicating the specification for theme level intercept
#' @param theme_level_slope A character indicating the specification for theme level slope
#' @param ... Additional arguments to be passed to the method.
#' @return The updated `ss_backtest_config` object with the specified `alpha_test_strategy` added.
#' @export
setGeneric("add_alpha_test_strategy", function(object, alpha_test_strategy, ...) {
  standardGeneric("add_alpha_test_strategy")
})

#' @rdname add_alpha_test_strategy
#' @export
setMethod(
  "add_alpha_test_strategy",
  signature(object = "ss_backtest_config", alpha_test_strategy = "alpha_test_strategy"),
  function(object, alpha_test_strategy) {
    # Set the alpha_test_strategy slot
    object@alpha_test_strategy <- alpha_test_strategy

    # Return the updated object
    return(object)
  }
)

#' @rdname add_alpha_test_strategy
#' @export
setMethod(
  "add_alpha_test_strategy",
  signature(object = "ss_backtest_config", alpha_test_strategy = "missing"),
  function(object, signal_significance_threshold = 0.05, p_correction_method = "none", market_factor_proxy,
           model_structure = "partial_pooled", theme_level_intercept = NULL, theme_level_slope = NULL,
           enable_theme_representativeness = TRUE, bayesian_model_parameters = NULL, lmer_control = NULL) {
    alpha_test_strategy <- create_alpha_test_strategy(
      signal_significance_threshold = signal_significance_threshold,
      p_correction_method = p_correction_method,
      market_factor_proxy = market_factor_proxy,
      model_structure = model_structure,
      theme_level_intercept = theme_level_intercept,
      theme_level_slope = theme_level_slope,
      enable_theme_representativeness = enable_theme_representativeness,
      bayesian_model_parameters = bayesian_model_parameters,
      lmer_control = lmer_control
    )

    # Set the alpha_test_strategy slot
    object@alpha_test_strategy <- alpha_test_strategy

    # Return the updated object
    return(object)
  }
)



# bayesian_model_parameters----------------------------------------------
#' @title Create Bayesian Model Parameters
#' @description Constructor for an S4 object of class \code{bayesian_model_parameters}.
#'
#' @param user_priors An object of class \code{brmsprior}, or \code{NULL}.
#'   Structured according to the \code{model_spec_theme_level}.
#' @param prior_derivation_control A list of additional parameters for deriving priors when \code{priors_type} is
#'   \code{"informative_exogenous_dataset"}. Must be a list with the following elements:
#'   \itemize{
#'     \item \code{half_t_df}: Degrees of freedom for the half-t distribution applied to sd priors.
#'     \item \code{lmer_optimizer}: Optimizer to be used in \code{lme4::lmer} for deriving priors.
#'     \item \code{lmer_optimization_objective}: Criteria to be optimized in \code{lme4::lmer} for deriving priors,
#'           e.g. \code{"likelihood"} or \code{"REML"}.
#'   }
#' @param brms_control A list of parameters to be passed to \code{brms::brm} for MCMC sampling, including:
#'   \itemize{
#'     \item \code{chains}: Number of Markov chains (default is 4).
#'     \item \code{iter}: Number of iterations per chain (default is 2000).
#'     \item \code{warmup}: Number of warmup iterations per chain (default is \code{floor(iter / 2)}).
#'     \item \code{thin}: Thinning interval (default is 1).
#'     \item \code{seed}: Seed for reproducibility (default is \code{NA}).
#'     \item \code{adapt_delta}: Target acceptance probability for HMC (default is 0.80).
#'   }
#'
#' @return An S4 object of class \code{bayesian_model_parameters}.
#' @export
create_bayesian_model_parameters <- function(
    user_priors = NULL,
    prior_derivation_control = NULL,
    brms_control = list(
      chains = 4,
      iter = 2000,
      warmup = 1000,
      thin = 1,
      seed = NA,
      adapt_delta = 0.80
    )) {
  # Build an S4 object with the supplied arguments.
  bayes_obj <- methods::new(
    "bayesian_model_parameters",
    user_priors = user_priors,
    prior_derivation_control = prior_derivation_control,
    brms_control = brms_control
  )

  # Trigger validation checks (as defined in the class).
  methods::validObject(bayes_obj)

  return(bayes_obj)
}


#' @title Add Bayesian Model Parameters
#' @description Generic function to add Bayesian model parameters.
#' @param object The object to which Bayesian model parameters will be added.
#' @param user_priors An optional object of class `brmsprior`.
#' @param prior_derivation_control An optional list containing prior derivation control parameters.
#' @param brms_control An optional list of parameters for `brms::brm`.
#' @return The updated object with the `bayesian_model_parameters` added.
#' @export
setGeneric("add_bayesian_model_parameters", function(object, user_priors = NULL, prior_derivation_control = NULL, brms_control = NULL) {
  standardGeneric("add_bayesian_model_parameters")
})

#' @rdname add_bayesian_model_parameters
#' @export
setMethod(
  "add_bayesian_model_parameters",
  signature(object = "bayesian_alpha_test_strategy"),
  function(object, user_priors = NULL, prior_derivation_control = NULL, brms_control = NULL) {
    # Ensure only one of `user_priors` or `prior_derivation_control` is provided
    if (!is.null(user_priors) && !is.null(prior_derivation_control)) {
      stop("Only one of 'user_priors' or 'prior_derivation_control' can be provided, not both.")
    }

    # Check `user_priors` argument
    if (!is.null(user_priors) && !inherits(user_priors, "brmsprior")) {
      stop("When provided, 'user_priors' must be an object of class 'brmsprior'.")
    }

    # Create `bayesian_model_parameters` object
    bayesian_params <-  methods::new("bayesian_model_parameters",
                                     user_priors = user_priors,
                                     prior_derivation_control = prior_derivation_control,
                                     brms_control = brms_control
    )

    # Add `bayesian_model_parameters` to `bayesian_alpha_test_strategy`
    object@bayesian_model_parameters <- bayesian_params
    return(object)
  }
)

#' @rdname add_bayesian_model_parameters
#' @export
setMethod(
  "add_bayesian_model_parameters",
  signature(object = "ss_backtest_config"),
  function(object,
           user_priors = NULL, prior_derivation_control = NULL, brms_control = NULL) {
    # Check if `alpha_test_strategy` is set
    if (is.null(object@alpha_test_strategy)) {
      stop("The 'alpha_test_strategy' slot is not set in the ss_backtest_config object.")
    }

    # Ensure the `alpha_test_strategy` is of class `bayesian_alpha_test_strategy`
    if (!methods::is(object@alpha_test_strategy, "bayesian_alpha_test_strategy")) {
      stop("The 'alpha_test_strategy' in the ss_backtest_config object is not of class 'bayesian_alpha_test_strategy'.")
    }

    # Call the method for 'bayesian_alpha_test_strategy' to add 'bayesian_model_parameters'
    object@alpha_test_strategy <- add_bayesian_model_parameters(
      object@alpha_test_strategy,
      user_priors = user_priors,
      prior_derivation_control = prior_derivation_control,
      brms_control = brms_control
    )

    # Return the updated 'ss_backtest_config' object
    return(object)
  }
)

#' @title Add Prior to Bayesian Alpha Test Strategy
#' @description Adds a prior to a `bayesian_alpha_test_strategy` object based on the specified effect, type, distribution, and parameters.
#' @param object An object of class `bayesian_alpha_test_strategy`.
#' @param effect A character string specifying the effect of the prior. Must be one of "fixed" or "random".
#' @param type A character string specifying the type of prior. Options: "intercept", "slope", "sigma", or "cor".
#' @param theme A character vector specifying themes (e.g., "value", "momentum"). Applicable for fixed effects.
#' @param distribution_choice A character vector specifying the distribution of the prior (e.g., "normal", "student_t").
#' @param pars A list of named numeric vectors specifying parameters for the chosen distribution.
#' @param level A character string specifying the hierarchical level for random effects. Options: "signals" (default), "tickers", or "theme".
#' @param ... Additional arguments.
#' @return An updated `bayesian_alpha_test_strategy` object with the added prior.
#' @details
#' - For `effect = "fixed"`, you can specify global intercepts or slopes, or theme-specific priors using the `theme` argument.
#' - For `effect = "random"`, you can define hierarchical priors with `level` specifying the grouping structure.
#'
#' Supported `distribution_choice` options include:
#' - `"normal"`: Requires `pars` with `mean` and `sd`.
#' - `"student_t"`: Requires `pars` with `df`, `mean`, and `sd`.
#' - `"lkj"`: Requires `pars` with `eta` (only for `type = "cor"`).
#'
#' @export
setGeneric("add_brms_prior", function(object, ...) standardGeneric("add_brms_prior"))

#' @rdname add_brms_prior
#' @export
setMethod(
  "add_brms_prior",
  signature(object = "bayesian_alpha_test_strategy"),
  function(object, effect, type, theme = NULL, distribution_choice, pars, level = "signals") {
    # Input validation
    effect <- match.arg(effect, choices = c("fixed", "random"))
    type <- match.arg(type, choices = c("intercept", "slope", "sigma", "cor"))
    level <- match.arg(level, choices = c("signals", "tickers", "theme"))

    # Ensure pars and distribution_choice have consistent lengths
    n <- length(distribution_choice)
    if (length(pars) != n) {
      stop("The lengths of `pars` and `distribution_choice` must match.")
    }

    # Handle fixed effects
    if (effect == "fixed") {
      priors <- lapply(seq_along(distribution_choice), function(i) {
        coef_name <- if (!is.null(theme)) {
          paste0("theme", theme[i], if (type == "slope") ":market_factor_proxy" else "")
        } else if (type == "slope") "market_factor_proxy" else ""
        brms::set_prior(
          paste0(distribution_choice[i], "(", paste(pars[[i]], collapse = ", "), ")"),
          class = "b", # Correctly setting class to 'b'
          coef = coef_name
        )
      })
    }

    # Handle random effects
    if (effect == "random") {
      if (type %in% c("intercept", "slope")) {
        coef_name <- if (type == "intercept") "Intercept" else "market_factor_proxy"
        group_name <- if (level == "signals") "theme:tickers" else level
        priors <- list(brms::set_prior(
          paste0(distribution_choice[1], "(", paste(pars[[1]], collapse = ", "), ")"),
          class = "sd",
          group = group_name,
          coef = coef_name
        ))
      } else if (type == "sigma") {
        priors <- list(brms::set_prior(
          paste0(distribution_choice[1], "(", paste(pars[[1]], collapse = ", "), ")"),
          class = "sigma"
        ))
      } else if (type == "cor") {
        priors <- list(brms::set_prior(
          paste0(distribution_choice[1], "(", paste(pars[[1]], collapse = ", "), ")"),
          class = "cor"
        ))
      }
    }

    # Add priors to the object
    if (is.null(object@bayesian_model_parameters@user_priors)) {
      object@bayesian_model_parameters@user_priors <- do.call(rbind, priors)
    } else {
      object@bayesian_model_parameters@user_priors <- rbind(
        object@bayesian_model_parameters@user_priors,
        do.call(rbind, priors)
      )
    }

    # Validate the object and return it
    methods::validObject(object)
    return(object)
  }
)


#' @rdname add_brms_prior
#' @export
setMethod(
  "add_brms_prior",
  signature(object = "ss_backtest_config"),
  function(object, effect, type, theme = NULL, distribution_choice, pars, level = "signals") {
    # Check if `alpha_test_strategy` is set
    if (is.null(object@alpha_test_strategy)) {
      stop("The 'alpha_test_strategy' slot is not set in the ss_backtest_config object.")
    }

    # Ensure the `alpha_test_strategy` is of class `bayesian_alpha_test_strategy`
    if (!methods::is(object@alpha_test_strategy, "bayesian_alpha_test_strategy")) {
      stop("The 'alpha_test_strategy' in the ss_backtest_config object is not of class 'bayesian_alpha_test_strategy'.")
    }

    # Delegate the call to the 'alpha_test_strategy' object's method
    object@alpha_test_strategy <- add_brms_prior(
      object = object@alpha_test_strategy,
      effect = effect,
      type = type,
      theme = theme,
      distribution_choice = distribution_choice,
      pars = pars,
      level = level
    )

    # Return the updated 'ss_backtest_config' object
    return(object)
  }
)






# tuning_strategy--------------------------------------------------------
#' Hyperparameter Tuning Strategy Constructor
#'
#' @description
#' Builds a \code{\link{grid_search_strategy-class}},
#' \code{\link{random_search_strategy-class}}, or
#' \code{\link{bayesian_opt_strategy-class}} object, depending on
#' \code{tuning_method}.
#'
#' @param tuning_method Character. One of \code{"grid_search"},
#'   \code{"random_search"}, or \code{"bayesian_opt"}.
#' @param validation_sample_size Numeric. Size of the validation sample. A
#'   value in \verb{(0, 1)} is later treated as a training-sample proportion
#'   by \code{\link{add_tuning_strategy}()}; this constructor stores it as-is.
#' @param chosen_eval_metric Character. Evaluation metric to optimize; must
#'   be one of \code{"rss"}, \code{"rmse"}, \code{"cp"}, \code{"mae"},
#'   \code{"mphe"}, \code{"mpe"}, \code{"mape"}, \code{"hr"}, \code{"mb"}.
#'   Required -- the resulting object's validity rejects a missing value.
#' @param hyper_grid_domain A \code{\link{hyper_grid_domain-class}} object.
#'   If \code{NULL}, an empty one is created; populate it via
#'   \code{\link{add_hyperparameter}()} before use.
#' @param early_stop Numeric or \code{NULL}. Epochs with no improvement
#'   before stopping early; only meaningful for \code{xgb}/\code{nn}.
#' @param n_iter Numeric. Required for \code{"random_search"} (draws per
#'   hyperparameter) and \code{"bayesian_opt"} (evaluations after
#'   initialization); ignored for \code{"grid_search"}.
#' @param acq Character. Acquisition function; required when
#'   \code{tuning_method = "bayesian_opt"}, unused otherwise.
#' @param init_points Numeric. Required when \code{tuning_method = "bayesian_opt"}, unused otherwise.
#' @param k_iter Numeric. Required when \code{tuning_method = "bayesian_opt"}, unused otherwise.
#'
#' @return An object of class \code{grid_search_strategy},
#'   \code{random_search_strategy}, or \code{bayesian_opt_strategy}.
#'
#' @export
create_tuning_strategy <- function(tuning_method, validation_sample_size, chosen_eval_metric, hyper_grid_domain = NULL, early_stop = NULL,
                                   n_iter = NULL, acq = NULL, init_points = NULL, k_iter = NULL) {
  # Check if hyper_grid_domain is provided; if not, create an empty one
  if (!tuning_method %in% c("grid_search", "random_search", "bayesian_opt")) {
    stop("tuning_method must be one of grid_search, random_search or bayesian_opt")
  }
  if (is.null(hyper_grid_domain)) {
    hyper_grid_domain <-  methods::new("hyper_grid_domain", hyperparameter_list = list())
  }

  # Check the value of tuning_method and create the appropriate subclass
  if (tuning_method == "grid_search") {
    # Create a grid_search_strategy object
    return( methods::new("grid_search_strategy",
                         tuning_method = "grid_search",
                         validation_sample_size = validation_sample_size,
                         chosen_eval_metric = chosen_eval_metric,
                         hyper_grid_domain = hyper_grid_domain,
                         early_stop = early_stop
    ))
  } else if (tuning_method == "random_search") {
    # Create a random_search_strategy object
    if (is.null(n_iter)) {
      stop("n_iter must be provided for random_search.")
    }
    return( methods::new("random_search_strategy",
                         tuning_method = "random_search",
                         validation_sample_size = validation_sample_size,
                         chosen_eval_metric = chosen_eval_metric,
                         hyper_grid_domain = hyper_grid_domain,
                         early_stop = early_stop,
                         n_iter = n_iter
    ))
  } else if (tuning_method == "bayesian_opt") {
    # Create a bayesian_opt_strategy object
    if (is.null(n_iter) || is.null(acq) || is.null(init_points) || is.null(k_iter)) {
      stop("n_iter, acq, init_points, and k_iter must be provided for bayesian_opt.")
    }
    return( methods::new("bayesian_opt_strategy",
                         tuning_method = "bayesian_opt",
                         validation_sample_size = validation_sample_size,
                         chosen_eval_metric = chosen_eval_metric,
                         hyper_grid_domain = hyper_grid_domain,
                         early_stop = early_stop,
                         n_iter = n_iter,
                         acq = acq,
                         init_points = init_points,
                         k_iter = k_iter
    ))
  } else {
    stop("Invalid tuning_method. Choose from 'grid_search', 'random_search', or 'bayesian_opt'.")
  }
}

#' Add a `tuning_strategy` to an Existing `sb_backtest_config`
#'
#' @description
#' Attaches a hyperparameter tuning strategy to a `sb_backtest_config`,
#' either by supplying an existing `tuning_strategy` object or by supplying
#' the parameters needed to build one on the fly.
#'
#' @param object A `sb_backtest_config` object to which a `tuning_strategy` will be added.
#' @param tuning_strategy An object of class `tuning_strategy`, or missing to build one from `...`.
#' @param ... Parameters forwarded to `create_tuning_strategy()` when `tuning_strategy` is missing
#'   (`tuning_method`, `validation_sample_size`, `chosen_eval_metric`, `hyper_grid_domain`,
#'   `early_stop`, `n_iter`, `acq`, `init_points`, `k_iter`).
#' @return An updated `sb_backtest_config` object with the specified or newly created `tuning_strategy`.
#' @export
setGeneric("add_tuning_strategy", function(object, tuning_strategy, ...) {
  standardGeneric("add_tuning_strategy")
})

#' @describeIn add_tuning_strategy Add an existing `tuning_strategy` to the `sb_backtest_config`.
#'
#' Replaces any existing tuning strategy. If `tuning_strategy@validation_sample_size`
#' is in \verb{(0, 1)}, it is rescaled to an absolute count as
#' `round(validation_sample_size * object@training_sample_size)` before being stored.
#' Only blocks `sb_algorithm == "ols"`; unlike the sibling method below, it does not
#' check for other heuristic (non-ML) algorithms.
#'
#' @param object A `sb_backtest_config` object.
#' @param tuning_strategy An object of class `tuning_strategy` to be added.
#' @return The updated `sb_backtest_config` object with the provided `tuning_strategy`.
#' @export
setMethod(
  "add_tuning_strategy", signature(object = "sb_backtest_config", tuning_strategy = "tuning_strategy"),
  function(object, tuning_strategy) {
    # Adjust validation sample size
    if (tuning_strategy@validation_sample_size < 1) {
      tuning_strategy@validation_sample_size <- round(tuning_strategy@validation_sample_size * object@training_sample_size)
    }

    if (object@sb_algorithm != "ols") {
      object@tuning_strategy <- tuning_strategy
    } else {
      stop("OLS does not require tuning.")
    }

    # Give warning if validation_sample size is bigger than training sample size
    if (tuning_strategy@validation_sample_size > object@training_sample_size) {
      message("Validation sample size is bigger than training sample size.")
    }

    # Validate the object explicitly
    methods::validObject(object)

    return(object)
  }
)



#' @describeIn add_tuning_strategy Create and add a new `tuning_strategy` to the `sb_backtest_config`.
#'
#' Builds a `tuning_strategy` via `create_tuning_strategy()` and attaches it. If
#' `chosen_eval_metric` is `NULL`, it is inferred from `object@custom_objective`
#' (`"pseudo_huber_error"` -> `"mphe"`, `"quantile_error"` -> `"quantile_loss"`,
#' `"absolute_error"` -> `"mae"`, otherwise `"rmse"`). `validation_sample_size`
#' in \verb{(0, 1)} is rescaled the same way as in the `tuning_strategy`-signature method.
#' Errors if `object@sb_algorithm` is a heuristic (non-ML) algorithm that does not require tuning.
#'
#' @param object A `sb_backtest_config` object.
#' @param tuning_strategy `NULL`; a new `tuning_strategy` is created from the remaining arguments.
#' @param tuning_method Character string indicating the hyperparameter tuning method. Must be one of 'grid_search', 'random_search', or 'bayesian_opt'.
#' @param validation_sample_size Numeric value representing the size of the validation sample.
#' @param chosen_eval_metric Character or `NULL`; see Details. If provided, must be one of
#'   `"rss"`, `"rmse"`, `"cp"`, `"mae"`, `"mphe"`, `"mpe"`, `"mape"`, `"hr"`, `"mb"`.
#' @param hyper_grid_domain An object of class `hyper_grid_domain`, or `NULL` to start with an empty one.
#' @param early_stop Optional, stopping criteria for early termination. Can be of any type.
#' @param n_iter Numeric, number of iterations for 'random_search' or 'bayesian_opt'.
#' @param acq Character string specifying the acquisition function for Bayesian optimization (for 'bayesian_opt' only). Defaults to `"ucb"`.
#' @param init_points Numeric, number of initial random points for Bayesian optimization (for 'bayesian_opt' only).
#' @param k_iter Numeric, number of samples to evaluate during Bayesian optimization (for 'bayesian_opt' only).
#' @return An updated `sb_backtest_config` object with a newly created `grid_search_strategy`, `random_search_strategy`, or `bayesian_opt_strategy`, depending on the selected `tuning_method`.
#' @export
setMethod(
  "add_tuning_strategy", signature(object = "sb_backtest_config", tuning_strategy = "missing"),
  function(object, tuning_strategy = NULL, tuning_method, validation_sample_size, chosen_eval_metric = NULL, hyper_grid_domain = NULL, early_stop = NULL,
           n_iter = NULL, acq = "ucb", init_points = NULL, k_iter = NULL) {
    # Custom fill of chosen eval metric in case of null
    if (is.null(chosen_eval_metric)) {
      chosen_eval_metric <- switch(object@custom_objective,
                                   "pseudo_huber_error" = "mphe",
                                   "quantile_error" = "quantile_loss",
                                   "absolute_error" = "mae",
                                   "rmse"
      )
      message(paste("chosen_eval_metric set to", chosen_eval_metric, "according to custom_objective.\n"))
    }

    # Adjust validation sample size if decimal
    if (validation_sample_size < 1) {
      validation_sample_size <- round(validation_sample_size * object@training_sample_size)
    }

    # Give warning if validation_sample size is bigger than training sample size
    if (validation_sample_size > object@training_sample_size) {
      message("Validation sample size is bigger than training sample size.")
    }

    if (!object@sb_algorithm %in% c("ols", "sw", "ew", "rp", "hrp", "mvo", "mmaf")) {
      # Create a new tuning_strategy object
      object@tuning_strategy <- create_tuning_strategy(
        tuning_method = tuning_method, validation_sample_size = validation_sample_size,
        chosen_eval_metric = chosen_eval_metric, early_stop = early_stop, n_iter = n_iter, acq = acq,
        init_points = init_points, k_iter = k_iter
      )
    } else {
      stop("ols, sw, ew, rp, hrp, mvo and mmaf do not require tuning.")
    }


    # Validate the object explicitly
    methods::validObject(object)

    return(object)
  }
)



# hyperparameters--------------------------------------------------------
#' Add a Hyperparameter to a `hyper_grid_domain`, whether inside a `sb_backtest_config`, a `tuning_strategy` or on its own.
#'
#' @description
#' Adds (or, for a name already present, upserts) one or more hyperparameters
#' in a `hyper_grid_domain`, dispatching on where it lives: a bare
#' `hyper_grid_domain`, a `tuning_strategy` subclass (`grid_search_strategy`,
#' `random_search_strategy`, `bayesian_opt_strategy`), or a `sb_backtest_config`.
#'
#' @param object A `hyper_grid_domain`, a `tuning_strategy` (or subclass:
#'   `grid_search_strategy`, `random_search_strategy`, `bayesian_opt_strategy`),
#'   or a `sb_backtest_config` object.
#' @param hyperparameter A character vector naming the hyperparameter(s) to add. Options are:
#' \itemize{
#'  \item \strong{glmnet}: alpha, lambda.min.ratio
#'  \item \strong{rf}: mtry, num.trees, max.depth, min.bucket
#'  \item \strong{xgb}: min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds
#'  \item \strong{nn}: regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs
#' }
#' @param ... Method-specific arguments: `grid` (grid search), `distribution_choice`/`pars`
#'   (random search), `bounds` (Bayesian optimization), or `new_hyperparameter_list`
#'   (bare `hyper_grid_domain`) -- see the individual methods below.
#'
#' @return The input object with `hyperparameter_list` (nested inside `hyper_grid_domain`
#'   for the strategy/config methods) updated: names shared with the existing list are
#'   overwritten, others are added, and everything already present under an unrelated
#'   name is left untouched.
#' @export
setGeneric("add_hyperparameter", function(object, hyperparameter, ...) {
  standardGeneric("add_hyperparameter")
})

#' @describeIn add_hyperparameter Upsert entries directly into a `hyper_grid_domain`'s `hyperparameter_list`.
#'
#' This is the merge primitive the `grid_search_strategy`/`random_search_strategy`/
#' `bayesian_opt_strategy` methods below delegate to after building a properly
#' shaped `new_hyperparameter_list` from their own `hyperparameter`/`grid`/
#' `distribution_choice`/`pars`/`bounds` arguments. It can also be called
#' directly if you already have the hyperparameter list in the shape
#' \code{\link{tuning_strategy-class}} expects for the target `tuning_method`.
#'
#' @param new_hyperparameter_list A named list of already-shaped hyperparameter
#'   entries. Names matching existing entries in `object@hyperparameter_list`
#'   overwrite them; other existing names are preserved.
#' @export
setMethod(
  "add_hyperparameter",
  signature(object = "hyper_grid_domain"),
  function(object, new_hyperparameter_list) {
    # Get names stored in new_hyperparameter_list
    hyperparameter <- names(new_hyperparameter_list)

    # Merge with existing hyperparameters in object
    if (length(object@hyperparameter_list) != 0) {
      old_hyperparameter_list <- object@hyperparameter_list

      # Take only those that have no substitute in new hyperparameters
      if (any(!names(old_hyperparameter_list) %in% hyperparameter)) {
        old_hyperparameter_list_disjoint <- old_hyperparameter_list[which(!names(old_hyperparameter_list) %in% hyperparameter)] # Info
        old_hyperparameter_list_disjoint_names <- names(old_hyperparameter_list)[which(!names(old_hyperparameter_list) %in% hyperparameter)] # Names

        # Re-combine
        for (hyper in old_hyperparameter_list_disjoint_names) {
          new_hyperparameter_list[hyper] <- old_hyperparameter_list[hyper]
        }

        # Re-order
        new_hyperparameter_list <- new_hyperparameter_list[c(old_hyperparameter_list_disjoint_names, hyperparameter)]
      }
    }

    # Update the object
    object@hyperparameter_list <- new_hyperparameter_list

    # Validate the object explicitly
    methods::validObject(object)

    return(object)
  }
)


#' @describeIn add_hyperparameter Add/upsert hyperparameter(s) in a `grid_search_strategy`'s `hyper_grid_domain`.
#' @param grid A numeric vector (single hyperparameter) or list of numeric vectors
#'   (one per name in `hyperparameter`, same order) giving the exact values to try.
#' @export
setMethod(
  "add_hyperparameter",
  signature(object = "grid_search_strategy"),
  function(object, hyperparameter, grid, ...) {
    if (!is.list(grid)) {
      grid <- list(grid)
    }

    if (!all(sapply(grid, function(x) is.numeric(x) && is.vector(x)))) {
      stop("Grid should contain only numeric data.")
    }

    if (length(hyperparameter) != length(grid)) {
      stop("All hyperparameters should have a grid.")
    }

    new_hyperparameter_list <- grid
    names(new_hyperparameter_list) <- hyperparameter


    # Extract the current object
    current_hyper_grid_domain <- object@hyper_grid_domain
    updated_hyper_grid_domain <- add_hyperparameter(current_hyper_grid_domain, new_hyperparameter_list = new_hyperparameter_list)

    # Update the object
    object@hyper_grid_domain <- updated_hyper_grid_domain

    # Validate the object explicitly
    methods::validObject(object)

    return(object)
  }
)



#' @describeIn add_hyperparameter Add/upsert hyperparameter(s) in a `random_search_strategy`'s `hyper_grid_domain`.
#'
#' There is no separate `value` argument for `distribution_choice = "constant"`:
#' pass the constant through `pars` (it is stored internally as `value`).
#'
#' @param distribution_choice Character vector, one of `"uniform"`, `"normal"`,
#'   `"lognormal"`, or `"constant"` per hyperparameter in `hyperparameter`.
#' @param pars A named numeric vector (or list of them, one per hyperparameter)
#'   with the parameters for the chosen distribution (`c(min, max)`, `c(mean, sd)`,
#'   `c(meanlog, sdlog)`), or the constant value itself when `distribution_choice = "constant"`.
#' @export
setMethod(
  "add_hyperparameter",
  signature(object = "random_search_strategy"),
  function(object, hyperparameter, distribution_choice, pars, ...) {
    # Logic for random_search
    if (!is.list(distribution_choice)) {
      distribution_choice <- as.list(distribution_choice) # As list
    }
    if (!is.list(pars)) {
      pars <- list(pars) # list
    }

    if (length(hyperparameter) != length(distribution_choice) || length(hyperparameter) != length(pars)) {
      stop("All hyperparameters should have matching distribution_choice and pars.")
    }

    valid_distributions <- c("uniform", "normal", "lognormal", "constant")
    if (!all(distribution_choice %in% valid_distributions)) {
      stop("Invalid distribution_choice. Choose from 'uniform', 'normal', 'lognormal', or 'constant'.")
    }

    for (i in seq_along(distribution_choice)) {
      dist <- distribution_choice[[i]]
      param <- pars[[i]]

      if (dist == "uniform" && (!all(c("min", "max") %in% names(param)))) {
        stop("For 'uniform', pars must contain 'min' and 'max'.")
      }
      if (dist == "normal" && (!all(c("mean", "sd") %in% names(param)))) {
        stop("For 'normal', pars must contain 'mean' and 'sd'.")
      }
      if (dist == "lognormal" && (!all(c("meanlog", "sdlog") %in% names(param)))) {
        stop("For 'lognormal', pars must contain 'meanlog' and 'sdlog'.")
      }
    }

    new_hyperparameter_list <- mapply(function(hp, dist, param) {
      if (dist == "constant") {
        list(distribution_choice = dist, value = unname(param))
      } else {
        list(distribution_choice = dist, pars = param)
      }
    }, hyperparameter, distribution_choice, pars, SIMPLIFY = FALSE)

    names(new_hyperparameter_list) <- hyperparameter


    # Extract the current object
    current_hyper_grid_domain <- object@hyper_grid_domain
    updated_hyper_grid_domain <- add_hyperparameter(current_hyper_grid_domain,
                                                    new_hyperparameter_list = new_hyperparameter_list
    )

    # Update the object
    object@hyper_grid_domain <- updated_hyper_grid_domain

    # Validate the object explicitly
    methods::validObject(object)

    return(object)
  }
)


#' @describeIn add_hyperparameter Add/upsert hyperparameter(s) in a `bayesian_opt_strategy`'s `hyper_grid_domain`.
#' @param bounds A numeric vector of length 2 (`c(lower, upper)`), or a list of such
#'   vectors (one per hyperparameter in `hyperparameter`, same order).
#' @export
setMethod(
  "add_hyperparameter",
  signature(object = "bayesian_opt_strategy"),
  function(object, hyperparameter, bounds, ...) {
    # Logic for bayesian_opt
    if (!is.list(bounds)) {
      bounds <- list(bounds)
    }

    if (!all(sapply(bounds, function(x) is.numeric(x) && length(x) == 2))) {
      stop("Each hyperparameter must have a bounds vector of length 2 (min and max).")
    }

    if (length(hyperparameter) != length(bounds)) {
      stop("All hyperparameters should have corresponding bounds.")
    }

    new_hyperparameter_list <- bounds
    names(new_hyperparameter_list) <- hyperparameter



    # Extract the current object
    current_hyper_grid_domain <- object@hyper_grid_domain
    updated_hyper_grid_domain <- add_hyperparameter(current_hyper_grid_domain, new_hyperparameter_list = new_hyperparameter_list)

    # Update the object
    object@hyper_grid_domain <- updated_hyper_grid_domain

    # Validate the object explicitly
    methods::validObject(object)

    return(object)
  }
)

#' @describeIn add_hyperparameter Add/upsert hyperparameter(s) via a `sb_backtest_config`'s attached `tuning_strategy`.
#'
#' Delegates to whichever strategy-specific method matches `object@tuning_strategy`'s
#' class; supply only the argument(s) relevant to that strategy's `tuning_method`
#' (`grid`, `distribution_choice`/`pars`, or `bounds`) and leave the rest `NULL`.
#' Requires `object@tuning_strategy` to already be set (see `add_tuning_strategy()`).
#'
#' @export
setMethod(
  "add_hyperparameter",
  signature(object = "sb_backtest_config"),
  function(object, hyperparameter, grid = NULL, distribution_choice = NULL, pars = NULL, bounds = NULL) {
    # Extract object
    tuning_strategy <- object@tuning_strategy


    # Add hyperparamete
    updated_tuning_strategy <- add_hyperparameter(tuning_strategy,
                                                  hyperparameter = hyperparameter,
                                                  grid = grid, distribution_choice = distribution_choice, pars = pars, bounds = bounds
    )

    # Update the object
    object@tuning_strategy <- updated_tuning_strategy

    # Validate the object explicitly
    methods::validObject(object)


    return(object)
  }
)


#' Add a `hyper_grid_domain` Object
#'
#' This function adds a `hyper_grid_domain` S4 class to a `tuning_strategy` or a `sb_backtest_config`.
#' It allows users to add a `hyper_grid_domain` already built or extracted from other objects.
#'
#' @param object An object of class `tuning_strategy` or a `sb_backtest_config`.
#' @param hyper_grid_domain An object of class `hyper_grid_domain`.
#'
#' @return The appropriate object with the added `hyper_grid_domain`.
#'
#' @export
setGeneric("add_hyper_grid_domain", function(object, hyper_grid_domain) {
  standardGeneric("add_hyper_grid_domain")
})


#' @describeIn add_hyper_grid_domain Add `hyper_grid_domain` to `tuning_strategy` object
#' @param object An object of class `tuning_strategy`.
#' @param hyper_grid_domain An object of class `hyper_grid_domain`.
#' @export
setMethod(
  "add_hyper_grid_domain",
  signature(object = "tuning_strategy", hyper_grid_domain = "hyper_grid_domain"),
  function(object, hyper_grid_domain) {
    # Add hyper_grid_domain
    object@hyper_grid_domain <- hyper_grid_domain

    # Validate the object explicitly
    methods::validObject(object)

    return(object)
  }
)


#' @describeIn add_hyper_grid_domain Add `hyper_grid_domain` to `sb_backtest_config` object
#' @param object An object of class `sb_backtest_config`. Must already have a
#'   `tuning_strategy` attached (via `add_tuning_strategy()`) -- this method writes
#'   into `object@tuning_strategy@hyper_grid_domain` and will error on a `NULL`/unset strategy.
#' @param hyper_grid_domain An object of class `hyper_grid_domain`.
#' @export
setMethod(
  "add_hyper_grid_domain",
  signature(object = "sb_backtest_config", hyper_grid_domain = "hyper_grid_domain"),
  function(object, hyper_grid_domain) {
    # Add hyper_grid_domain
    object@tuning_strategy@hyper_grid_domain <- hyper_grid_domain

    # Validate the object explicitly
    methods::validObject(object)

    return(object)
  }
)





# keras_architecture----------------------------------------------------
#' @title Create Keras Architecture
#' @description Constructor for creating an instance of `keras_architecture_parameters`.
#'
#' @param nn_optimizer A character string specifying the optimizer to use (e.g., "adam").
#' @param units A numeric value for the number of units in the new layer.
#' @param activation A character string specifying the activation function for the new layer (e.g., "relu").
#' @param batch_norm_option A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'
#' @return An object of class `keras_architecture_parameters`.
#'
#' @export
create_keras_architecture <- function(nn_optimizer, units = NULL, activation = NULL, batch_norm_option = NULL) {
  # Check nn_optimizer is valid
  valid_optimizers <- c("Adam", "RMSProp")
  if (!nn_optimizer %in% valid_optimizers) {
    stop("nn_optimizer should be Adam or RMSProp.")
  }

  # Check format
  if (!is.numeric(units)) {
    stop("units should be a numeric value.")
  }
  if (!all(activation %in% c("relu", "sigmoid", "tanh", "softmax"))) {
    stop("activation should be relu, sigmoid, tanh, or softmax.")
  }
  if (!all(is.logical(batch_norm_option))) {
    stop("batch_norm_option should be a logical value.")
  }

  # Create new_keras_architecture
  new_keras_architecture_parameters <-
    methods::new("keras_architecture_parameters",
                 units = units,
                 n_layers = length(units),
                 activation = activation,
                 nn_optimizer = nn_optimizer,
                 batch_norm_option = batch_norm_option
    )


  return(new_keras_architecture_parameters)
}


#' @title Add Layer to Keras Architecture
#' @description Method to add a new layer to the Keras architecture.
#'
#' @param object An object of class `keras_architecture_parameters` or `sb_backtest_config`
#' @param units A numeric value for the number of units in the new layer.
#' @param activation A character string specifying the activation function for the new layer (e.g., "relu").
#' @param batch_norm_option A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'
#' @return An updated object of class `keras_architecture_parameters`.
#'
#' @export
setGeneric("add_keras_layer", function(object, units, activation, batch_norm_option) {
  standardGeneric("add_keras_layer")
})

#' @describeIn add_keras_layer Add a keras layer to an object of class `keras_architecture_parameters`
#' @param object An object of class `keras_architecture_parameters`
#' @export
setMethod(
  "add_keras_layer", "keras_architecture_parameters", function(object, units, activation, batch_norm_option) {
    # Check that units are numeric
    if (!all(is.numeric(units))) {
      stop("units should be numeric")
    }

    # Check activation functions
    valid_activations <- c("relu", "sigmoid", "softmax", "softplus", "tanh", "leaky_relu")
    if (!all(activation %in% valid_activations)) {
      stop("activation should be one of relu, sigmoid, softmax, softplus, tanh or leaky_relu.")
    }

    # Check batch_norm_option is logical
    if (!all(batch_norm_option %in% c(TRUE, FALSE))) {
      stop("batch_norm_option should be logical (TRUE or FALSE)")
    }

    # Check length
    if (length(units) != length(activation) || length(units) != length(batch_norm_option)) {
      stop("units, activation and batch_norm_option should have matching length.")
    }

    # Update the layers
    object@units <- c(object@units, units)
    object@n_layers <- length(object@units) # Update the number of layers
    object@activation <- c(object@activation, activation)
    object@batch_norm_option <- c(object@batch_norm_option, batch_norm_option)

    if (length(object@units) > 5) {
      warning("factoRverse only supports up to 5 layers currently")
    }

    return(object) # Return the updated object
  }
)


#' @describeIn add_keras_layer Add a keras layer to an object of class `sb_backtest_config`
#' @param object An object of class `sb_backtest_config`
#' @export
setMethod(
  "add_keras_layer", "sb_backtest_config", function(object, units, activation, batch_norm_option) {
    object <- add_keras_layer(object@keras_architecture_parameters, units = units, activation = activation, batch_norm_option)

    return(object) # Return the updated object
  }
)


#' @title Add Keras Architecture
#' @description Method to add a `keras_architecture_parameters` to a `sb_backtest_config`.
#'
#' This function allows you to either directly add a pre-existing `keras_architecture_parameters` object or create one dynamically by passing additional arguments.
#' When `keras_architecture_parameters` is not provided, a new one will be created using the values for `nn_optimizer`, `units`, `activation`, and `batch_norm_option` passed via the `...` argument.
#'
#' @param object An object of class `sb_backtest_config`.
#' @param keras_architecture_parameters An object of class `keras_architecture_parameters` or missing if a new architecture is to be created.
#' @param ... Additional arguments used to create a new `keras_architecture_parameters` when `keras_architecture_parameters` is missing. These arguments must include:
#'   \itemize{
#'     \item \strong{nn_optimizer}: A character string specifying the optimizer to use (e.g., "adam").
#'     \item \strong{units}: A numeric value for the number of units in the new layer.
#'     \item \strong{activation}: A character string specifying the activation function for the new layer (e.g., "relu").
#'     \item \strong{batch_norm_option}: A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'   }
#'
#' @return An updated object of class `sb_backtest_config` with the `keras_architecture_parameters` added.
#' @export
setGeneric("add_keras_architecture", function(object, keras_architecture_parameters, ...) {
  standardGeneric("add_keras_architecture")
})

#' @describeIn add_keras_architecture Add existing `keras_architecture_parameters` object
#'
#' This method allows you to add an already existing `keras_architecture_parameters` object to an `sb_backtest_config`.
#'
#' @param object An object of class `sb_backtest_config`.
#' @param keras_architecture_parameters An existing object of class `keras_architecture_parameters`.
#' @return An updated `sb_backtest_config` object with the provided `keras_architecture_parameters`.
#' @export
setMethod(
  "add_keras_architecture",
  signature(object = "sb_backtest_config", keras_architecture_parameters = "keras_architecture_parameters"),
  function(object, keras_architecture_parameters) {
    object@keras_architecture_parameters <- keras_architecture_parameters

    return(object) # Return the updated object
  }
)

#' @describeIn add_keras_architecture Dynamically create and add `keras_architecture_parameters` object
#'
#' This method creates a new `keras_architecture_parameters` object dynamically when `keras_architecture_parameters` is not provided.
#' The parameters required to create this object must be passed via `...` and include:
#'   \itemize{
#'     \item \strong{nn_optimizer}: A character string specifying the optimizer to use (e.g., "adam").
#'     \item \strong{units}: A numeric value for the number of units in the new layer.
#'     \item \strong{activation}: A character string specifying the activation function for the new layer (e.g., "relu").
#'     \item \strong{batch_norm_option}: A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'   }
#'
#' @param object An object of class `sb_backtest_config`.
#' @param keras_architecture_parameters Should be missing to dynamically create a new architecture.
#' @param ... Additional parameters used to create the `keras_architecture_parameters`, including:
#'   \itemize{
#'     \item \strong{nn_optimizer}: A character string specifying the optimizer to use (e.g., "adam").
#'     \item \strong{units}: A numeric value for the number of units in the new layer.
#'     \item \strong{activation}: A character string specifying the activation function for the new layer (e.g., "relu").
#'     \item \strong{batch_norm_option}: A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'   }
#' @return An updated `sb_backtest_config` object with the newly created `keras_architecture_parameters`.
#' @export
setMethod(
  "add_keras_architecture",
  signature(object = "sb_backtest_config", keras_architecture_parameters = "missing"),
  function(object, keras_architecture_parameters = NULL, ...) {
    # Extract args to build keras
    args <- list(...)

    # Ensure all required parameters are present
    if (!all(c("nn_optimizer", "units", "activation", "batch_norm_option") %in% names(args))) {
      stop("All required parameters (nn_optimizer, units, activation, batch_norm_option) must be provided.")
    }

    # Create the keras architecture parameters using the additional arguments
    keras_architecture_parameters <- create_keras_architecture(
      nn_optimizer = args$nn_optimizer,
      units = args$units, activation = args$activation, batch_norm_option = args$batch_norm_option
    )

    # Assign the created keras architecture to the object
    object@keras_architecture_parameters <- keras_architecture_parameters

    return(object) # Return the updated object
  }
)



# cov_est_method---------------------------------------------------------
#' @title Create Covariance Estimation Method
#' @description Constructor for creating an instance of `cov_est_method`.
#'
#' @param cov_estimation_method A character string representing the covariance estimation method. Must be one of 'sample', 'ewma', 'cc', 'pca1', 'pca2', 'shrink_id' or 'shrink_cc'.
#' @param cov_matrix_sample_size Number of periods to subset return sample when estimating the covariance matrix. A high number will provide
#' higher degrees of freedom, but old returns might not reflect current risk due to parameter shift. A low number will tend to expose estimation
#' to dimensionality curse.
#' @param active_returns logical. If TRUE, the covariance matrix will be estimated using active returns. If FALSE, the covariance matrix will be estimated using raw returns.
#' @param cov_matrix_benchmark A character string representing the benchmark for covariance matrix estimation. If NULL, the covariance matrix will be estimated using the active returns.
#' @export
create_cov_est_method <- function(cov_estimation_method = "sample", cov_matrix_sample_size, active_returns = TRUE, cov_matrix_benchmark = NULL) {
  cov_est_method <-  methods::new("cov_est_method",
                                  cov_estimation_method = cov_estimation_method,
                                  cov_matrix_sample_size = cov_matrix_sample_size,
                                  active_returns = active_returns,
                                  cov_matrix_benchmark = cov_matrix_benchmark
  )

  return(cov_est_method)
}

#' @title Add covariance estimation method to a backtest configuration
#'
#' @description
#' This function allows either directly adding a pre-existing `cov_est_method` object or creating one dynamically by passing additional arguments.
#' When `cov_est_method` is not provided, a new one will be created using the values for `cov_estimation_method`, `cov_matrix_sample_size`, and `active_returns`, passed via the `...` argument.
#'
#' @param object An object of class `sb_backtest_config` or `port_backtest_config`.
#' @param cov_est_method An object of class `cov_est_method`, or missing if a new object is to be created.
#' @param cov_estimation_method A character string representing the covariance estimation method. Must be one of `"sample"`, `"ewma"`, `"cc"`, `"pca1"`, `"pca2"`, `"shrink_id"` or `"shrink_cc"`.
#' @param cov_matrix_sample_size Number of periods to subset return sample when estimating the covariance matrix. A high number provides
#' @param active_returns logical. If `TRUE`, the covariance matrix is estimated using active returns. If `FALSE`, raw returns are used.
#' @param cov_matrix_benchmark A character string representing the benchmark for covariance matrix estimation. This is used when `cov_estimation_method` is `"shrink_id"` or `"shrink_cc"`.
#' @param ... Additional arguments.
#'
#' @return An updated object of class `sb_backtest_config` or `port_backtest_config` with the `cov_est_method` added.
#' @export
setGeneric("add_cov_est_method", function(object, cov_est_method, ...) {
  standardGeneric("add_cov_est_method")
})

#' @describeIn add_cov_est_method Add existing `cov_est_method` object to a `sb_backtest_config` object.
#'
#' This method allows to add an already existing `cov_est_method` object to an `sb_backtest_config`.
#'
#' @param object An object of class `sb_backtest_config`.
#' @param cov_est_method An existing object of class `cov_est_method`.
#' @export
setMethod(
  "add_cov_est_method", signature(object = "sb_backtest_config", cov_est_method = "cov_est_method"),
  function(object, cov_est_method, ...) {
    # Check for sb algo
    if (!(object@sb_algorithm %in% c("rp", "hrp", "mvo", "mmaf"))) {
      stop("Covariance estimation method is only available for 'rp' and 'mvo' strategies.")
    }

    object@signal_port_parameters@cov_est_method <- cov_est_method

    return(object)
  }
)

#' @describeIn add_cov_est_method Create a `cov_est_method` object to a `sb_backtest_config` object.
#'
#' This method allows to dynamically create a `cov_est_method` object and add to `sb_backtest_config`.
#'
#' @param object An object of class `sb_backtest_config`.
#' @param cov_est_method An existing object of class `cov_est_method`.
#' @export
setMethod(
  "add_cov_est_method", signature(object = "sb_backtest_config", cov_est_method = "missing"),
  function(object, cov_est_method, cov_estimation_method = "sample", cov_matrix_sample_size = 36, active_returns = TRUE, cov_matrix_benchmark = NULL, ...) {
    # Check for sb algo
    if (!(object@sb_algorithm %in% c("rp", "hrp", "mvo", "mmaf"))) {
      stop("Covariance estimation method is only available for 'rp' and 'mvo' strategies.")
    }

    object@signal_port_parameters@cov_est_method <- create_cov_est_method(
      cov_estimation_method = cov_estimation_method,
      cov_matrix_sample_size = cov_matrix_sample_size,
      active_returns = active_returns,
      cov_matrix_benchmark = cov_matrix_benchmark
    )

    return(object)
  }
)



#' @describeIn add_cov_est_method Add existing `cov_est_method` object to a `port_backtest_config` object.
#'
#' This method allows to add an already existing `cov_est_method` object to an `port_backtest_config`.
#'
#' @param object An object of class `port_backtest_config`.
#' @param cov_est_method An existing object of class `cov_est_method`.
#' @export
setMethod(
  "add_cov_est_method", signature(object = "port_backtest_config", cov_est_method = "cov_est_method"),
  function(object, cov_est_method, ...) {
    # Check for port construction method
    if (!object@port_construction_method %in% c("rp", "hrp", "mvo", "mmaf")) {
      stop("Covariance estimation method is only available for 'rp', 'hrp', 'mvo' and 'mmaf' strategies.")
    }

    # Check for existence of selected_benchmark
    if (!is.null(object@selected_benchmark) && object@active_returns) {
      message("Using port_backtest_config selected benchmark as active covariance matrix benchmark.")
      cov_est_method@cov_matrix_benchmark <- object@selected_benchmark
    }

    object@cov_est_method <- cov_est_method

    return(object)
  }
)

#' @describeIn add_cov_est_method Create a `cov_est_method` object to a `port_backtest_config` object.
#'
#' This method allows to dynamically create a `cov_est_method` object and add to `port_backtest_config`.
#'
#' @param object An object of class `port_backtest_config`.
#' @param cov_est_method An existing object of class `cov_est_method`.
#' @export
setMethod(
  "add_cov_est_method", signature(object = "port_backtest_config", cov_est_method = "missing"),
  function(object, cov_est_method, cov_estimation_method = "sample", cov_matrix_sample_size = 252, active_returns = TRUE, cov_matrix_benchmark = NULL, ...) {
    # Check for sb algo
    if (!object@port_construction_method %in% c("rp", "hrp", "mvo", "mmaf")) {
      stop("Covariance estimation method is only available for 'rp', 'hrp', 'mvo' and 'mmaf' strategies.")
    }

    # Check for existence of selected_benchmark
    if (!is.null(object@selected_benchmark) && active_returns) {
      message("Using port_backtest_config selected benchmark as active covariance matrix benchmark.")
      cov_matrix_benchmark <- object@selected_benchmark
    }

    object@cov_est_method <- create_cov_est_method(
      cov_estimation_method = cov_estimation_method,
      cov_matrix_sample_size = cov_matrix_sample_size,
      active_returns = active_returns,
      cov_matrix_benchmark = cov_matrix_benchmark
    )

    return(object)
  }
)




# mvo_parameters-------------------------------------------------------
#' @title Create MVO Parameters
#' @description Constructor function for creating an instance of `mvo_parameters`.
#'
#' @param opt_method A character indicating the optimization method.
#'   The only current available method is 'random'. In this case, \code{n_random_ports} are
#'   generated under the constraints defined in the `mvo_parameters` object and the one that
#'   optimizes the \code{opt_objective} will be selected.
#' @param random_ports_method A character string representing the method that will be
#'   passed to \code{PortfolioAnalytics::random_portfolios} to generate random portfolios.
#'   Options are 'sample', 'simplex' or 'grid'.
#' @param n_random_ports Number of random portfolios to generate. Only needed when
#'   \code{opt_method} is 'random'.
#' @param opt_objective A character indicating the optimization objective. Possible options
#'   are 'return', 'risk' or 'sharpe'.
#' @param ridge_pen A numeric value representing the ridge penalty to be used in the optimization.
#' @param n_resamples A numeric value indicating the number of bootstrap resamples to perform
#' @param exp_ret_score_jitter A numeric value indicating the jitter to be applied to the expected return scores
#' @param cov_eigval_jitter A numeric value indicating the jitter to be applied to the covariance matrix eigenvalues
#'
#' @return An S4 object of class `mvo_parameters`.
#' @export
#'
create_mvo_parameters <- function(opt_method = "random",
                                  random_ports_method = "sample",
                                  n_random_ports = 1000,
                                  opt_objective = "sharpe",
                                  ridge_pen = NULL,
                                  n_resamples = 0,
                                  exp_ret_score_jitter = 0,
                                  cov_eigval_jitter = 0
) {

  mvo_params <- methods::new("mvo_parameters",
                             opt_method = opt_method,
                             random_ports_method = random_ports_method,
                             n_random_ports = n_random_ports,
                             opt_objective = opt_objective,
                             ridge_pen = ridge_pen,
                             n_resamples = n_resamples,
                             exp_ret_score_jitter = exp_ret_score_jitter,
                             cov_eigval_jitter = cov_eigval_jitter
  )
  return(mvo_params)
}


#' @title Add mvo_parameters to a backtest config
#'
#' @description
#' This function allows either directly adding a pre-existing `mvo_parameters` object
#' or creating one dynamically by passing additional arguments.
#'
#' @param object An object of class `sb_backtest_config` or `port_backtest_config`.
#' @param mvo_params An object of class `mvo_parameters`, or missing if a new object is to be created.
#' @param opt_method A character indicating the optimization method.
#' @param random_ports_method A character string representing the method to generate random portfolios.
#' @param n_random_ports Number of random portfolios to generate.
#' @param opt_objective A character indicating the optimization objective.
#' @param ridge_pen A numeric value representing the ridge penalty to be used in the optimization.
#' @param n_resamples A numeric value indicating the number of bootstrap resamples to perform
#' @param exp_ret_score_jitter A numeric value indicating the jitter to be applied to the expected return scores
#' @param cov_eigval_jitter A numeric value indicating the jitter to be applied to the covariance matrix eigenvalues
#' @param level A character indicating the level to which the parameters should be applied when using 'mmaf' strategy.
#' @param ... Additional arguments used to create a new `mvo_parameters` object when `mvo_params` is missing.
#'   These arguments must include:
#'   \itemize{
#'     \item \strong{opt_method}: A character indicating the optimization method.
#'           The only current available method is 'random'.
#'     \item \strong{random_ports_method}: A character string representing the method to generate random portfolios.
#'           Options are 'sample', 'simplex' or 'grid'.
#'     \item \strong{n_random_ports}: Number of random portfolios to generate.
#'           Only needed when `opt_method` is 'random'.
#'     \item \strong{opt_objective}: A character indicating the optimization objective.
#'           Possible options are 'return', 'risk' or 'sharpe'.
#'   }
#'
#' @return An updated object of class `sb_backtest_config` or `port_backtest_config` with
#'   the `mvo_parameters` added.
#' @export
setGeneric("add_mvo_parameters", function(object, mvo_params, ...) {
  standardGeneric("add_mvo_parameters")
})


#' @describeIn add_mvo_parameters Add existing `mvo_parameters` object to a `sb_backtest_config` object.
#' @export
setMethod(
  "add_mvo_parameters",
  signature(object = "sb_backtest_config", mvo_params = "mvo_parameters"),
  function(object, mvo_params, level = NULL, ...) {

    # Check for sb
    if (!(object@sb_algorithm %in% c("mvo", "mmaf"))) {
      stop("MVO parameters is only available for 'mvo' strategies.")
    }

    # If sb_algorithm is mmaf, require that level is either micro or macro
    if (object@sb_algorithm == "mmaf") {
      if (is.null(level) || !(level %in% c("micro", "macro"))) {
        stop("When sb_algorithm is 'mmaf', level must be specified as either 'micro' or 'macro'.")
      }
      if (level == "macro") {
        message("Applying MVO parameters to macro portfolio.")
        ## Check if object@signal_port_parameters@mmaf_parameters@macro_port_config@port_construction_method is mvo
        if (object@signal_port_parameters@mmaf_parameters@macro_port_config@port_construction_method != "mvo") {
          stop("macro_port_config port_construction_method is not 'mvo'.")
        }
        object@signal_port_parameters@mmaf_parameters@macro_port_config@mvo_parameters <- mvo_params

      } else {
        message("Applying MVO parameters to micro portfolios.")
        ## Check if object@signal_port_parameters@mmaf_parameters@micro_port_config@port_construction_method is mvo
        if (object@signal_port_parameters@mmaf_parameters@micro_port_config@port_construction_method != "mvo") {
          stop("micro_port_config port_construction_method is not 'mvo'.")
        }
        object@signal_port_parameters@mmaf_parameters@micro_port_config@mvo_parameters <- mvo_params
      }
      return(object)
    }


    # Suppose you store mvo_parameters within signal_port_parameters:
    object@signal_port_parameters@mvo_parameters <- mvo_params

    return(object)
  }
)

#' @describeIn add_mvo_parameters Dynamically create a `mvo_parameters` object and add it to a `sb_backtest_config` object.
#' @export
setMethod(
  "add_mvo_parameters",
  signature(object = "sb_backtest_config", mvo_params = "missing"),
  function(object,
           mvo_params,
           opt_method = "random",
           random_ports_method = "sample",
           n_random_ports = 1000,
           opt_objective = "sharpe",
           ridge_pen = NULL,
           n_resamples = 0,
           exp_ret_score_jitter = 0,
           cov_eigval_jitter = 0,
           level = NULL,
           ...) {

    # Check for sb
    if (!(object@sb_algorithm %in% c("mvo", "mmaf"))) {
      stop("MVO parameters is only available for 'mvo' strategies.")
    }

    # Create mvo params
    mvo_params <- create_mvo_parameters(
      opt_method = opt_method,
      random_ports_method = random_ports_method,
      n_random_ports = n_random_ports,
      opt_objective = opt_objective,
      ridge_pen = ridge_pen,
      n_resamples = n_resamples,
      exp_ret_score_jitter = exp_ret_score_jitter,
      cov_eigval_jitter = cov_eigval_jitter
    )

    # Add it to object
    object <- object %>% add_mvo_parameters(mvo_params = mvo_params, level = level)


    return(object)
  }
)



#' @describeIn add_mvo_parameters Add existing `mvo_parameters` object to a `port_backtest_config` object.
#' @export
setMethod(
  "add_mvo_parameters",
  signature(object = "port_backtest_config", mvo_params = "mvo_parameters"),
  function(object, mvo_params, level = NULL, ...) {

    # Check for port construction method
    if (!(object@port_construction_method %in% c("mvo", "mmaf"))) {
      stop("MVO parameters is only available for 'mvo' strategies.")
    }

    # If port_construction_method is mmaf, require that level is either micro or macro
    if (object@port_construction_method == "mmaf") {
      if (is.null(level) || !(level %in% c("micro", "macro"))) {
        stop("When port_construction_method is 'mmaf', level must be specified as either 'micro' or 'macro'.")
      }
      if (level == "macro") {
        message("Applying MVO parameters to macro portfolio.")
        ## Check if object@signal_port_parameters@mmaf_parameters@macro_port_config@port_construction_method is mvo
        if (object@mmaf_parameters@macro_port_config@port_construction_method != "mvo") {
          stop("macro_port_config port_construction_method is not 'mvo'.")
        }
        object@mmaf_parameters@macro_port_config@mvo_parameters <- mvo_params

      } else {
        message("Applying MVO parameters to micro portfolios.")
        ## Check if object@signal_port_parameters@mmaf_parameters@micro_port_config@port_construction_method is mvo
        if (object@mmaf_parameters@micro_port_config@port_construction_method != "mvo") {
          stop("micro_port_config port_construction_method is not 'mvo'.")
        }
        object@mmaf_parameters@micro_port_config@mvo_parameters <- mvo_params
      }
      return(object)
    }

    object@mvo_parameters <- mvo_params

    return(object)
  }
)

#' @describeIn add_mvo_parameters Dynamically create a `mvo_parameters` object and add it to a `port_backtest_config` object.
#' @export
setMethod(
  "add_mvo_parameters",
  signature(object = "port_backtest_config", mvo_params = "missing"),
  function(object,
           mvo_params,
           opt_method = "random",
           random_ports_method = "sample",
           n_random_ports = 1000,
           opt_objective = "sharpe",
           ridge_pen = NULL,
           n_resamples = 0,
           exp_ret_score_jitter = 0,
           cov_eigval_jitter = 0,
           level = NULL,
           ...) {


    # Check for port construction method
    if (!(object@port_construction_method %in% c("mvo", "mmaf"))) {
      stop("MVO parameters is only available for 'mvo' strategies.")
    }

    # Create mvo params
    mvo_params <- create_mvo_parameters(
      opt_method = opt_method,
      random_ports_method = random_ports_method,
      n_random_ports = n_random_ports,
      opt_objective = opt_objective,
      ridge_pen = ridge_pen,
      n_resamples = n_resamples,
      exp_ret_score_jitter = exp_ret_score_jitter,
      cov_eigval_jitter = cov_eigval_jitter
    )

    # Add it to object
    object <- object %>% add_mvo_parameters(mvo_params = mvo_params, level = level)


    return(object)

  }
)


# rp_parameters--------------------------------------------------------
#' @title Create RP (Risk Parity) Parameters
#' @description Constructor function for creating an instance of `rp_parameters`.
#'
#' @param rp_method A character indicating the method to compute the risk-parity vanilla solution.
#'   It is passed to \code{riskParityPortfolio::riskParityPortfolio()} function as \code{method_init}.
#'   Default is \code{"cyclical-spinu"}.
#' @param exp_ret_score_tilt A character value indicating the tilt to apply to the expected return score.
#'  It is used to compute the expected return score tilting the risk-parity solution
#'  towards higher expected return assets. Default is 'none', meaning no tilt is applied.
#' @param exp_ret_score_tilt_eta A character string indicating the eta to compute the expected return score tilt.
#'
#' @return An S4 object of class `rp_parameters`.
#' @export
create_rp_parameters <- function(rp_method = "cyclical-spinu",
                                 exp_ret_score_tilt = "none", exp_ret_score_tilt_eta = NULL) {

  rp_params <- methods::new("rp_parameters",
                            rp_method = rp_method,
                            exp_ret_score_tilt = exp_ret_score_tilt,
                            exp_ret_score_tilt_eta = exp_ret_score_tilt_eta
  )
  return(rp_params)
}

#' @title Add rp_parameters to a backtest config
#'
#' @description
#' This function allows either directly adding a pre-existing `rp_parameters` object
#' or creating one dynamically by passing additional arguments.
#'
#' @param object An object of class `sb_backtest_config` or `port_backtest_config`.
#' @param rp_params An object of class `rp_parameters`, or missing if a new object is to be created.
#' @param rp_method A character indicating the method to compute the risk-parity solution.
#' @param exp_ret_score_tilt A character value indicating the tilt to apply to the expected return score.
#' @param exp_ret_score_tilt_eta A numeric indicating the tilt intensity to apply to the expected return score.
#' @param level A character indicating the level to which the parameters should be applied when using 'mmaf' strategy.
#' @param ... Additional arguments used to create a new `rp_parameters` object when `rp_params` is missing.
#'   These arguments must include:
#'   \itemize{
#'     \item \strong{rp_method}: A character indicating the method to compute the risk-parity solution.
#'   }
#'
#' @return An updated object of class `sb_backtest_config` or `port_backtest_config` with
#'   the `rp_parameters` added.
#' @export
setGeneric("add_rp_parameters", function(object, rp_params, ...) {
  standardGeneric("add_rp_parameters")
})


#' @describeIn add_rp_parameters Add an existing `rp_parameters` object to a `sb_backtest_config` object.
#' @export
setMethod(
  "add_rp_parameters",
  signature(object = "sb_backtest_config", rp_params = "rp_parameters"),
  function(object, rp_params, level = NULL, ...) {

    # Check for sb
    if (!(object@sb_algorithm %in% c("rp", "mmaf"))) {
      stop("RP parameters is only available for 'rp' strategies.")
    }

    # If sb_algorithm is mmaf, require that level is either micro or macro
    if (object@sb_algorithm == "mmaf") {
      if (is.null(level) || !(level %in% c("micro", "macro"))) {
        stop("When sb_algorithm is 'mmaf', level must be specified as either 'micro' or 'macro'.")
      }
      if (level == "macro") {
        message("Applying RP parameters to macro portfolio.")
        ## Check if object@signal_port_parameters@mmaf_parameters@macro_port_config@port_construction_method is rp
        if (object@signal_port_parameters@mmaf_parameters@macro_port_config@port_construction_method != "rp") {
          stop("macro_port_config port_construction_method is not 'rp'.")
        }
        object@signal_port_parameters@mmaf_parameters@macro_port_config@rp_parameters <- rp_params

      } else {
        message("Applying RP parameters to micro portfolios.")
        ## Check if object@signal_port_parameters@mmaf_parameters@micro_port_config@port_construction_method is rp
        if (object@signal_port_parameters@mmaf_parameters@micro_port_config@port_construction_method != "rp") {
          stop("micro_port_config port_construction_method is not 'rp'.")
        }
        object@signal_port_parameters@mmaf_parameters@micro_port_config@rp_parameters <- rp_params
      }
      return(object)
    }

    # Suppose you store rp_parameters within signal_port_parameters:
    object@signal_port_parameters@rp_parameters <- rp_params

    return(object)
  }
)

#' @describeIn add_rp_parameters Dynamically create a `rp_parameters` object and add it to a `sb_backtest_config` object.
#' @export
setMethod(
  "add_rp_parameters",
  signature(object = "sb_backtest_config", rp_params = "missing"),
  function(object,
           rp_params,
           rp_method = "cyclical-spinu",
           exp_ret_score_tilt = "none",
           exp_ret_score_tilt_eta = NULL,
           level = NULL,
           ...) {

    # Check for sb
    if (!(object@sb_algorithm %in% c("rp", "mmaf"))) {
      stop("RP parameters is only available for 'rp' strategies.")
    }

    # Create rp params
    rp_params <- create_rp_parameters(
      rp_method = rp_method,
      exp_ret_score_tilt = exp_ret_score_tilt,
      exp_ret_score_tilt_eta = exp_ret_score_tilt_eta
    )

    # Add it to object
    object <- object %>% add_rp_parameters(rp_params = rp_params, level = level)


    return(object)
  }
)

#' @describeIn add_rp_parameters Add an existing `rp_parameters` object to a `port_backtest_config` object.
#' @export
setMethod(
  "add_rp_parameters",
  signature(object = "port_backtest_config", rp_params = "rp_parameters"),
  function(object, rp_params, level = NULL, ...) {

    # Check for pcm
    if (!(object@port_construction_method %in% c("rp", "mmaf"))) {
      stop("RP parameters is only available for 'rp' strategies.")
    }

    # If port_construction_method is mmaf, require that level is either micro or macro
    if (object@port_construction_method == "mmaf") {
      if (is.null(level) || !(level %in% c("micro", "macro"))) {
        stop("When port_construction_method is 'mmaf', level must be specified as either 'micro' or 'macro'.")
      }
      if (level == "macro") {
        message("Applying RP parameters to macro portfolio.")
        ## Check if object@mmaf_parameters@macro_port_config@port_construction_method is rp
        if (object@mmaf_parameters@macro_port_config@port_construction_method != "rp") {
          stop("macro_port_config port_construction_method is not 'rp'.")
        }
        object@mmaf_parameters@macro_port_config@rp_parameters <- rp_params

      } else {
        message("Applying RP parameters to micro portfolios.")
        ## Check if object@mmaf_parameters@micro_port_config@port_construction_method is rp
        if (object@mmaf_parameters@micro_port_config@port_construction_method != "rp") {
          stop("micro_port_config port_construction_method is not 'rp'.")
        }
        object@mmaf_parameters@micro_port_config@rp_parameters <- rp_params
      }
      return(object)
    }


    object@rp_parameters <- rp_params

    return(object)
  }
)

#' @describeIn add_rp_parameters Dynamically create a `rp_parameters` object and add it to a `port_backtest_config` object.
#' @export
setMethod(
  "add_rp_parameters",
  signature(object = "port_backtest_config", rp_params = "missing"),
  function(object,
           rp_params,
           rp_method = "cyclical-spinu",
           exp_ret_score_tilt = "none",
           exp_ret_score_tilt_eta = NULL,
           level = NULL,
           ...) {
    # Check for pcm
    if (!(object@port_construction_method %in% c("rp", "mmaf"))) {
      stop("RP parameters is only available for 'rp' strategies.")
    }

    # Create rp params
    rp_params <- create_rp_parameters(
      rp_method = rp_method,
      exp_ret_score_tilt = exp_ret_score_tilt,
      exp_ret_score_tilt_eta = exp_ret_score_tilt_eta
    )

    # Add it to object
    object <- object %>% add_rp_parameters(rp_params = rp_params, level = level)

    return(object)
  }
)



# hrp_parameters--------------------------------------------------------
#' @title Create HRP (Hierarchical Risk Parity) Parameters
#' @description Constructor function for creating an instance of `hrp_parameters`.
#'
#' @param linkage A character indicating the linkage method to use for hierarchical clustering.
#'   Possible options are 'single', 'complete', 'average', 'weighted', 'centroid', 'median', or 'ward.D2'.
#'   Default is 'single.'
#' @param exp_ret_score_tilt A character value indicating the tilt to apply to the expected return score.
#'  It is used to compute the expected return score tilting the risk-parity solution
#'  towards higher expected return assets. Default is 'none', meaning no tilt is applied.
#' @param exp_ret_score_tilt_eta A character string indicating the eta to compute the expected return score tilt.
#'
#' @return An S4 object of class `hrp_parameters`.
#' @export
create_hrp_parameters <- function(linkage = "single",
                                  exp_ret_score_tilt = "none", exp_ret_score_tilt_eta = NULL) {

  hrp_params <- methods::new("hrp_parameters",
                             linkage = linkage,
                             exp_ret_score_tilt = exp_ret_score_tilt,
                             exp_ret_score_tilt_eta = exp_ret_score_tilt_eta
  )
  return(hrp_params)
}

#' @title Add hrp_parameters to a backtest config
#'
#' @description
#' This function allows either directly adding a pre-existing `hrp_parameters` object
#' or creating one dynamically by passing additional arguments.
#'
#' @param object An object of class `sb_backtest_config` or `port_backtest_config`.
#' @param hrp_params An object of class `hrp_parameters`, or missing if a new object is to be created.
#' @param linkage Character indicating the linkage method to use for hierarchical clustering.
#' @param exp_ret_score_tilt A character value indicating the tilt to apply to the expected return score.
#' @param exp_ret_score_tilt_eta A numeric indicating the tilt intensity to apply to the expected return score.
#' @param level A character indicating the level to which the parameters should be applied when using 'mmaf' strategy.
#' @param ... Additional arguments used to create a new `hrp_parameters` object when `hrp_params` is missing.
#'   These arguments must include:
#'   \itemize{
#'     \item \strong{linkage}:  A character indicating the linkage method to use for hierarchical clustering.
#'   }
#'
#' @return An updated object of class `sb_backtest_config` or `port_backtest_config` with
#'   the `hrp_parameters` added.
#' @export
setGeneric("add_hrp_parameters", function(object, hrp_params, ...) {
  standardGeneric("add_hrp_parameters")
})


#' @describeIn add_hrp_parameters Add an existing `hrp_parameters` object to a `sb_backtest_config` object.
#' @export
setMethod(
  "add_hrp_parameters",
  signature(object = "sb_backtest_config", hrp_params = "hrp_parameters"),
  function(object, hrp_params, level = NULL, ...) {

    # Check for sb
    if (!(object@sb_algorithm %in% c("hrp", "mmaf"))) {
      stop("HRP parameters is only available for 'hrp' strategies.")
    }

    # If sb_algorithm is mmaf, require that level is either micro or macro
    if (object@sb_algorithm == "mmaf") {
      if (is.null(level) || !(level %in% c("micro", "macro"))) {
        stop("When sb_algorithm is 'mmaf', level must be specified as either 'micro' or 'macro'.")
      }
      if (level == "macro") {
        message("Applying HRP parameters to macro portfolio.")
        ## Check if object@signal_port_parameters@mmaf_parameters@macro_port_config@port_construction_method is hrp
        if (object@signal_port_parameters@mmaf_parameters@macro_port_config@port_construction_method != "hrp") {
          stop("macro_port_config port_construction_method is not 'hrp'.")
        }
        object@signal_port_parameters@mmaf_parameters@macro_port_config@hrp_parameters <- hrp_params

      } else {
        message("Applying HRP parameters to micro portfolios.")
        ## Check if object@signal_port_parameters@mmaf_parameters@micro_port_config@port_construction_method is hrp
        if (object@signal_port_parameters@mmaf_parameters@micro_port_config@port_construction_method != "hrp") {
          stop("micro_port_config port_construction_method is not 'hrp'.")
        }
        object@signal_port_parameters@mmaf_parameters@micro_port_config@hrp_parameters <- hrp_params
      }
      return(object)
    }


    # Suppose you store hrp_parameters within signal_port_parameters:
    object@signal_port_parameters@hrp_parameters <- hrp_params

    return(object)
  }
)

#' @describeIn add_hrp_parameters Dynamically create a `hrp_parameters` object and add it to a `sb_backtest_config` object.
#' @export
setMethod(
  "add_hrp_parameters",
  signature(object = "sb_backtest_config", hrp_params = "missing"),
  function(object,
           hrp_params,
           linkage = "single",
           exp_ret_score_tilt = "none",
           exp_ret_score_tilt_eta = NULL,
           level = NULL,
           ...) {

    # Check for sb
    if (!(object@sb_algorithm %in% c("hrp", "mmaf"))) {
      stop("HRP parameters is only available for 'hrp' strategies.")
    }

    # Create hrp params
    hrp_params <- create_hrp_parameters(
      linkage = linkage,
      exp_ret_score_tilt = exp_ret_score_tilt,
      exp_ret_score_tilt_eta = exp_ret_score_tilt_eta
    )

    # Add it to object
    object <- object %>% add_hrp_parameters(hrp_params = hrp_params, level = level)


    return(object)
  }
)

#' @describeIn add_hrp_parameters Add an existing `hrp_parameters` object to a `port_backtest_config` object.
#' @export
setMethod(
  "add_hrp_parameters",
  signature(object = "port_backtest_config", hrp_params = "hrp_parameters"),
  function(object, hrp_params, level = NULL, ...) {

    # Check for pcm
    if (!(object@port_construction_method %in% c("hrp", "mmaf"))) {
      stop("HRP parameters is only available for 'hrp' strategies.")
    }

    # If port_construction_method is mmaf, require that level is either micro or macro
    if (object@port_construction_method == "mmaf") {
      if (is.null(level) || !(level %in% c("micro", "macro"))) {
        stop("When port_construction_method is 'mmaf', level must be specified as either 'micro' or 'macro'.")
      }
      if (level == "macro") {
        message("Applying HRP parameters to macro portfolio.")
        ## Check if object@mmaf_parameters@macro_port_config@port_construction_method is hrp
        if (object@mmaf_parameters@macro_port_config@port_construction_method != "hrp") {
          stop("macro_port_config port_construction_method is not 'hrp'.")
        }
        object@mmaf_parameters@macro_port_config@hrp_parameters <- hrp_params

      } else {
        message("Applying HRP parameters to micro portfolios.")
        ## Check if object@mmaf_parameters@micro_port_config@port_construction_method is hrp
        if (object@mmaf_parameters@micro_port_config@port_construction_method != "hrp") {
          stop("micro_port_config port_construction_method is not 'hrp'.")
        }
        object@mmaf_parameters@micro_port_config@hrp_parameters <- hrp_params
      }
      return(object)
    }


    object@hrp_parameters <- hrp_params

    return(object)
  }
)

#' @describeIn add_hrp_parameters Dynamically create a `hrp_parameters` object and add it to a `port_backtest_config` object.
#' @export
setMethod(
  "add_hrp_parameters",
  signature(object = "port_backtest_config", hrp_params = "missing"),
  function(object,
           hrp_params,
           linkage = "single",
           exp_ret_score_tilt = "none",
           exp_ret_score_tilt_eta = NULL,
           level = NULL,
           ...) {
    # Check for pcm
    if (!(object@port_construction_method %in% c("hrp", "mmaf"))) {
      stop("HRP parameters is only available for 'hrp' strategies.")
    }

    # Create hrp params
    hrp_params <- create_hrp_parameters(
      linkage = linkage,
      exp_ret_score_tilt = exp_ret_score_tilt,
      exp_ret_score_tilt_eta = exp_ret_score_tilt_eta
    )

    # Add it to object
    object <- object %>% add_hrp_parameters(hrp_params = hrp_params, level = level)

    return(object)
  }
)


# mmaf_parameters--------------------------------------------------------
#' @title Create MMAF (Micro Macro Allocation Framework) Parameters
#'
#' @description Constructor function for creating an instance of `mmaf_parameters`.
#'
#' @param mmaf_method A character indicating the MMAF method to be used. Must be one of 'top_down' or 'bottom_up'.
#' @param top_down_proxy_port_method A character indicating the method to be used for constructing the top-down proxy portfolio.
#' Must be one of 'ew', 'rp', 'hrp', 'cs' or 'sw' if mmaf_method is 'top_down' and NULL if mmaf_method is 'bottom_up'.
#' @param mmaf_group_col A character string representing the MMAF group to which the assets belong. This is used to group assets when constructing micro and macro portfolios.
#' It must be a length 1 character.
#' @param micro_port_construction_method An object of class `micro_port_construction_method` representing the configuration for constructing micro portfolios.
#' @param macro_port_construction_method An object of class `macro_port_construction_method` representing the configuration for constructing macro portfolios.
#'
#' @return An S4 object of class `mmaf_parameters`.
#' @export
#'
create_mmaf_parameters <- function(mmaf_method = "bottom_up",
                                   top_down_proxy_port_method = "ew",
                                   mmaf_group_col,
                                   micro_port_construction_method,
                                   macro_port_construction_method) {

  # Validate mmaf_method
  if (!(mmaf_method %in% c("top_down", "bottom_up"))) {
    stop("mmaf_method must be either 'top_down' or 'bottom_up'.")
  }

  # Validate top_down_proxy_port_method
  if (mmaf_method == "top_down" && !(top_down_proxy_port_method %in% c("ew", "rp", "hrp", "cs", "sw"))) {
    stop("For 'top_down' mmaf_method, top_down_proxy_port_method must be one of 'ew', 'rp', 'hrp', 'cs', or 'sw'.")
  }
  if (mmaf_method == "bottom_up" && !is.null(top_down_proxy_port_method)) {
    stop("For 'bottom_up' mmaf_method, top_down_proxy_port_method must be NULL.")
  }

  # Validate mmaf_group_col
  if (!is.null(mmaf_group_col) && (!is.character(mmaf_group_col) || length(mmaf_group_col) != 1)) {
    stop("mmaf_group_col must be a length 1 character string.")
  }

  # Create micro_port_config
  micro_port_config <- methods::new("mmaf_sub_port_config",
                                    port_construction_method = micro_port_construction_method,
                                    mvo_parameters = NULL,
                                    rp_parameters = NULL,
                                    hrp_parameters = NULL)

  # Create macro_port_config
  macro_port_config <- methods::new("mmaf_sub_port_config",
                                    port_construction_method = macro_port_construction_method,
                                    mvo_parameters = NULL,
                                    rp_parameters = NULL,
                                    hrp_parameters = NULL)


  # Create the mmaf_parameters object
  mmaf_params <- methods::new("mmaf_parameters",
                              mmaf_method = mmaf_method,
                              top_down_proxy_port_method = top_down_proxy_port_method,
                              mmaf_group_col = mmaf_group_col,
                              micro_port_config = micro_port_config,
                              macro_port_config = macro_port_config
  )

  return(mmaf_params)
}

#' @title Add mmaf_parameters to a backtest config
#'
#' @description
#' This function allows either directly adding a pre-existing `mmaf_parameters` object
#' or creating one dynamically by passing additional arguments.
#'
#' @param object An object of class `sb_backtest_config` or `port_backtest_config`.
#' @param mmaf_params An object of class `mmaf_parameters`, or missing if a new one is to be created.
#' @param mmaf_method A character indicating the MMAF method to be used. Must be one of 'top_down' or 'bottom_up'.
#' @param top_down_proxy_port_method A character indicating the proxy portfolio method for top-down MMAF.
#' @param mmaf_group_col A character string (length 1) with the grouping column name.
#' @param micro_port_construction_method A character indicating the micro portfolio construction method.
#' @param macro_port_construction_method A character indicating the macro portfolio construction method.
#' @param ... Additional arguments used to create a new `mmaf_parameters` object when `mmaf_params` is missing.
#'
#' @return An updated object of class `sb_backtest_config` or `port_backtest_config` with
#'   the `mmaf_parameters` added.
#' @export
setGeneric("add_mmaf_parameters", function(object, mmaf_params, ...) {
  standardGeneric("add_mmaf_parameters")
})


#' @describeIn add_mmaf_parameters Add an existing `mmaf_parameters` object to a `sb_backtest_config` object.
#' @export
setMethod(
  "add_mmaf_parameters",
  signature(object = "sb_backtest_config", mmaf_params = "mmaf_parameters"),
  function(object, mmaf_params, ...) {

    if (object@sb_algorithm != "mmaf") {
      stop("MMAF parameters can only be added when sb_algorithm = 'mmaf'.")
    }

    object@signal_port_parameters@mmaf_parameters <- mmaf_params

    return(object)
  }
)


#' @describeIn add_mmaf_parameters Dynamically create a `mmaf_parameters` object and add it to a `sb_backtest_config` object.
#' @export
setMethod(
  "add_mmaf_parameters",
  signature(object = "sb_backtest_config", mmaf_params = "missing"),
  function(object,
           mmaf_params,
           mmaf_method = "bottom_up",
           top_down_proxy_port_method = if (mmaf_method == "top_down") "ew" else NULL,
           mmaf_group_col,
           micro_port_construction_method,
           macro_port_construction_method,
           ...) {

    if (object@sb_algorithm != "mmaf") {
      stop("MMAF parameters can only be added when sb_algorithm = 'mmaf'.")
    }

    mmaf_params <- create_mmaf_parameters(
      mmaf_method = mmaf_method,
      top_down_proxy_port_method = top_down_proxy_port_method,
      mmaf_group_col = mmaf_group_col,
      micro_port_construction_method = micro_port_construction_method,
      macro_port_construction_method = macro_port_construction_method
    )

    object <- object %>% add_mmaf_parameters(mmaf_params = mmaf_params)

    return(object)
  }
)


#' @describeIn add_mmaf_parameters Add an existing `mmaf_parameters` object to a `port_backtest_config` object.
#' @export
setMethod(
  "add_mmaf_parameters",
  signature(object = "port_backtest_config", mmaf_params = "mmaf_parameters"),
  function(object, mmaf_params, ...) {

    if (object@port_construction_method != "mmaf") {
      stop("MMAF parameters can only be added when port_construction_method = 'mmaf'.")
    }

    object@mmaf_parameters <- mmaf_params

    return(object)
  }
)


#' @describeIn add_mmaf_parameters Dynamically create a `mmaf_parameters` object and add it to a `port_backtest_config` object.
#' @export
setMethod(
  "add_mmaf_parameters",
  signature(object = "port_backtest_config", mmaf_params = "missing"),
  function(object,
           mmaf_params,
           mmaf_method = "bottom_up",
           top_down_proxy_port_method = if (mmaf_method == "top_down") "ew" else NULL,
           mmaf_group_col,
           micro_port_construction_method,
           macro_port_construction_method,
           ...) {

    if (object@port_construction_method != "mmaf") {
      stop("MMAF parameters can only be added when port_construction_method = 'mmaf'.")
    }

    mmaf_params <- create_mmaf_parameters(
      mmaf_method = mmaf_method,
      top_down_proxy_port_method = top_down_proxy_port_method,
      mmaf_group_col = mmaf_group_col,
      micro_port_construction_method = micro_port_construction_method,
      macro_port_construction_method = macro_port_construction_method
    )


    object <- object %>% add_mmaf_parameters(mmaf_params = mmaf_params)

    return(object)
  }
)



# sb_backtest------------------------------------------------------------
#' @title Create sb_backtest_config Object
#' @description Constructs an sb_backtest_config object.
#'
#' @param sb_algorithm Character string specifying the signal-blending algorithm. One of 'ols' (default),
#'   'glmnet', 'rf', 'xgb', 'nn' (ML), or 'ew', 'sw', 'rp', 'hrp', 'mvo', 'mmaf', 'custom_weights' (heuristic/portfolio).
#' @param chosen_signals_and_positions A named vector of chosen signals and their positions ('long'/'short').
#'   Defaults to NULL, which is coerced to 'all' (use every signal in `features_m_df`).
#' @param target_fwd_name Name of the target variable in `target_m_df`.
#' @param training_sample_size Number of observations to include in each training sample.
#' @param rebalancing_months Months (numeric) when model should be rebalanced (refit).
#' @param split_method Character string indicating the data splitting method ('expanding' or 'rolling').
#' @param tuning_strategy An object of class tuning_strategy, specifying the strategy for tuning hyperparameters.
#' @param custom_objective Character string, or NULL to auto-set. For ML algorithms: 'squared_error'
#'   (default), 'pseudo_huber_error' or 'absolute_error' (last two only for 'xgb'/'nn'). For heuristic
#'   portfolio algorithms ('sw', 'rp', 'hrp', 'mvo', 'mmaf'): a 'max_'/'min_' + heuristic-metric string
#'   (defaults to 'max_info_ratio' when NULL). See `display_valid_custom_objectives()`.
#' @param keras_architecture_parameters An object of class `keras_architecture_parameters` providing parameters specific to keras-based neural networks.
#' @param signal_port_parameters An object of class `signal_port_parameters`, specifying the parameters for constructing signal portfolios (portfolio-blending).
#' @param quantile_tau Numeric value indicating the tau parameter used for quantile regression, between 0 and 1.
#' @param huber_delta Numeric value greater than 0, specifying the delta parameter for Huber loss function.
#' @param config_name Name of the backtest configuration.
#'
#' @return An sb_backtest_config object.
#' @seealso [add_tuning_strategy()], [add_hyperparameter()], [add_concentration_constraint_policy()], [run_sb_backtest()]
#' @export
create_sb_backtest_config <- function(sb_algorithm = "ols", target_fwd_name, tuning_strategy = NULL, training_sample_size, rebalancing_months, split_method = "expanding",
                                      chosen_signals_and_positions = NULL,
                                      custom_objective = NULL, keras_architecture_parameters = NULL, signal_port_parameters = NULL, quantile_tau = 0.5, huber_delta = 1,
                                      config_name = "not_identified") {
  ## Give custom warning related to quantile tau and huber delta
  if (!is.null(quantile_tau) && quantile_tau != 0.5) {
    message("changing quantile_tau impacts both chosen_eval_metric and custom_objective.")
  }
  if (!is.null(huber_delta) && huber_delta != 1) {
    message("changing huber_delta impacts both chosen_eval_metric and custom_objective.")
  }

  ## Set custom objective default based on sb_algorithm
  if (is.null(custom_objective)){

    if (sb_algorithm %in% c("rp", "hrp", "mvo", "mmaf", "sw")){
      ### Change custom objective default
      custom_objective <- "max_info_ratio"
      message("custom_objective set as 'max_info_ratio'. It can be changed by the argument custom_objective.",
              "To see complete list of valid heuristic performance metrics, use 'display_valid_custom_objectives()")
    } else {
      custom_objective <- "squared_error"
    }

  }

  ## Chosen_signals_and_positions
  ### Custom weights warning
  if (sb_algorithm == "custom_weights" && is.null(chosen_signals_and_positions)) {
    message(
      "Only positions of chosen_signals_and_positions are used when sb_algorithm is custom_weights, as every non-zero weight",
      "in custom_signal_weights_m_df will be eligible."
    )
  }
  if (is.null(chosen_signals_and_positions)) {
    chosen_signals_and_positions <- "all"
  }

  ### Check if chosen_signals_and_positions length > 1
  if (length(chosen_signals_and_positions) == 1 && chosen_signals_and_positions != "all") {
    stop("More than one signal must be provided in order to run a sb_backtest")
  }
  ### Check if there are repeated signals in chosen_signals
  if (!identical(names(chosen_signals_and_positions), unique(names(chosen_signals_and_positions)))) {
    stop("each signal must be chosen only once")
  }
  ### Check for presence of low_
  if (any(grepl("low_", names(chosen_signals_and_positions)))) {
    stop("chosen_signals_and_positions should not contain 'low_'.")
  }

  # Create default parameters for signal_port_parameters depending on sb_algo
  if (sb_algorithm %in% c("mvo", "rp", "hrp", "mmaf") && is.null(signal_port_parameters)) {
    cov_est_method <- create_cov_est_method(cov_estimation_method = "sample", cov_matrix_sample_size = 36, active_returns = TRUE, cov_matrix_benchmark = "IBOV")
    mvo_parameters <- if (sb_algorithm == "mvo"){
      create_mvo_parameters(opt_method = "random", random_ports_method = "sample", n_random_ports = 1000, opt_objective = "sharpe",
                            ridge_pen = NULL, n_resamples = 0, exp_ret_score_jitter = 0, cov_eigval_jitter = 0)
    } else NULL
    rp_parameters <- if (sb_algorithm == "rp"){
      create_rp_parameters(rp_method = "cyclical-spinu", exp_ret_score_tilt = "none", exp_ret_score_tilt_eta = NULL)
    } else NULL
    hrp_parameters <- if (sb_algorithm == "hrp"){
      create_hrp_parameters(linkage = "single", exp_ret_score_tilt = "none", exp_ret_score_tilt_eta = NULL)
    } else NULL
    mmaf_parameters <- if (sb_algorithm == "mmaf"){
      create_mmaf_parameters(mmaf_method = "bottom_up",
                             top_down_proxy_port_method = NULL,
                             mmaf_group_col = "sector",
                             micro_port_construction_method = "ew",
                             macro_port_construction_method = "ew")
    } else NULL


    signal_port_parameters <- methods::new("signal_port_parameters",
                                           cov_est_method = cov_est_method,
                                           mvo_parameters = mvo_parameters,
                                           rp_parameters = rp_parameters,
                                           hrp_parameters = hrp_parameters,
                                           mmaf_parameters = mmaf_parameters,
                                           concentration_constraint_policy = NULL
    )
  }

  # Create the sb_backtest_config object
  methods::new("sb_backtest_config",
               sb_algorithm = sb_algorithm,
               target_fwd_name = target_fwd_name,
               training_sample_size = training_sample_size,
               chosen_signals_and_positions = chosen_signals_and_positions,
               rebalancing_months = rebalancing_months,
               split_method = split_method,
               tuning_strategy = tuning_strategy,
               custom_objective = custom_objective,
               keras_architecture_parameters = keras_architecture_parameters,
               signal_port_parameters = signal_port_parameters,
               quantile_tau = quantile_tau,
               huber_delta = huber_delta,
               config_name = config_name
  )
}






# sb_metabacktest------------------------------------------------------------
#' Create SB Meta Backtest Configuration
#'
#' The `create_sb_metabacktest_config` function creates an `sb_metabacktest_config` object that configures a
#' meta-learning (stacking) backtest. It wraps a single meta-learner `sb_backtest_config` together with the
#' rules for assembling the meta feature set from base learners' out-of-sample predictions
#' (`features_passthrough`, `normalize_base_predictions`, `winsorize_base_predictions`). The base learners
#' themselves are supplied later, as a list of `sb_backtest_results`, to [run_sb_backtest()].
#'
#' @param meta_sb_backtest_config A `sb_backtest_config` with the configuration for the meta learner.
#' @param features_passthrough A character vector naming features from `features_m_df` to append to the
#'   meta-learner's inputs; or 'all' (all features) or 'none' (none). Default 'none'.
#' @param normalize_base_predictions A logical value indicating whether to normalize the base predictions.
#' @param winsorize_base_predictions A logical value indicating whether to winsorize the base predictions.
#' @param config_name Name of the backtest configuration.
#' @param ... Additional arguments (not used).
#'
#' @return An `sb_metabacktest_config` object.
#'
#' @export
setGeneric("create_sb_metabacktest_config", function(meta_sb_backtest_config, features_passthrough, ...) {
  standardGeneric("create_sb_metabacktest_config")
})


#' @describeIn create_sb_metabacktest_config Create a meta-backtest config from a meta-learner `sb_backtest_config`.
#'
#' @param meta_sb_backtest_config A `sb_backtest_config` with the configuration for the meta learner.
#' @param ... Additional arguments (not used).
#'
#' @return An `sb_metabacktest_config` object.
#' @export
setMethod(
  "create_sb_metabacktest_config",
  signature(meta_sb_backtest_config = "sb_backtest_config", features_passthrough = "character"),
  function(meta_sb_backtest_config, features_passthrough = "none", config_name = "not_identified",
           normalize_base_predictions = TRUE, winsorize_base_predictions = TRUE,
           ...) {


    # Warn about not considering chosen_signals_and_positions at meta-level
    if (length(meta_sb_backtest_config@chosen_signals_and_positions) > 1 || meta_sb_backtest_config@chosen_signals_and_positions != "all") {
      message(
        "chosen_signals_and_positions parameter of the meta-level sb_backtest_config will not be considered.",
        "Selection of features for meta-learner are set via features_passthrough, with positions derived by base-level chosen_signal_and_positions to ensure consistency."
      )
      meta_sb_backtest_config@chosen_signals_and_positions <- "all"
    }

    # Create the sb_metabacktest_config object
    meta_config <- methods::new("sb_metabacktest_config",
                                meta_sb_backtest_config = meta_sb_backtest_config,
                                features_passthrough = features_passthrough,
                                normalize_base_predictions = normalize_base_predictions,
                                winsorize_base_predictions = winsorize_base_predictions,
                                config_name = config_name
    )
    return(meta_config)
  }
)



#' @title Create an sb_metabacktest_results Object
#' @description Constructs an `sb_metabacktest_results` object from a list of `sb_backtest_results` objects for base learners a single
#' `sb_backtest_results` object for the meta learner.
#' It computes consolidated and time series evaluation metrics for machine learning backtests.
#'
#' @param meta_sb_backtest_results  A `sb_backtest_results` object for the meta learner
#' @param base_sb_backtest_results_list A named list of `sb_backtest_results` objects for the base learners.
#' @param oos_predictions_m_df A `meta_dataframe` object containing out-of-sample predictions for the base learners.
#' @param sb_metabacktest_config An `sb_metabacktest_config` object containing the configuration for the meta-learner.
#' @param ... Additional arguments (not used).
#' @return An object of class `sb_metabacktest_results`.
#'
setGeneric(
  name = "create_sb_metabacktest_results",
  def = function(meta_sb_backtest_results, base_sb_backtest_results_list, oos_predictions_m_df,
                 sb_metabacktest_config, ...) {
    standardGeneric("create_sb_metabacktest_results")
  }
)

#' @rdname create_sb_metabacktest_results
#' @aliases create_sb_metabacktest_results,list-method
setMethod(
  f = "create_sb_metabacktest_results",
  signature = signature(
    meta_sb_backtest_results = "sb_backtest_results",
    base_sb_backtest_results_list = "list",
    oos_predictions_m_df = "meta_dataframe",
    sb_metabacktest_config = "sb_metabacktest_config"
  ),
  definition = function(meta_sb_backtest_results, base_sb_backtest_results_list, oos_predictions_m_df, sb_metabacktest_config) {

    # Check that the base_sb_backtest_results input is a list of 'sb_backtest_results' objects
    if (!all(sapply(base_sb_backtest_results_list, function(x) methods::is(x, "sb_backtest_results")))) {
      stop("All elements in 'base_sb_backtest_results_list' must be of class 'sb_backtest_results'")
    }

    # Get all objects
    all_sb_backtest_results <- c(base_sb_backtest_results_list, meta_sb_backtest_results)
    # Get names
    base_sb_names <- names(base_sb_backtest_results_list)
    meta_sb_name <- meta_sb_backtest_results@backtest_identifier


    # Call the helper consolidate function
    results <- consolidate_sb_metabacktest_results(
      all_sb_backtest_results = all_sb_backtest_results,
      meta_sb_name = meta_sb_name,
      base_sb_names = base_sb_names
    )

    # Create the sb_metabacktest_results object
    new_object <- methods::new(
      "sb_metabacktest_results",
      sb_metabacktest_config = sb_metabacktest_config,
      meta_sb_backtest_results = meta_sb_backtest_results,
      base_sb_backtest_results_list = base_sb_backtest_results_list,
      base_learners_oos_predictions_m_df = oos_predictions_m_df,
      combined_oos_testing_metrics = list(
        all_dates_oos_testing_metrics = results$all_dates_oos_testing_metrics,
        common_dates_oos_testing_metrics = results$common_dates_oos_testing_metrics
      ),
      mean_validation_metrics = results$mean_validation_metrics,
      time_series_oos_testing_metrics = results$time_series_oos_testing_metrics,
      time_series_validation_metrics = results$time_series_validation_metrics,
      backtest_identifier = meta_sb_backtest_results@backtest_identifier
    )

    return(new_object)
  }
)




# port_backtest_config---------------------------------------------------
#' @title Create port_backtest_config Object
#' @description Constructs a `port_backtest_config` object containing all necessary parameters for backtesting stock-level portfolios.
#'
#' @param chosen_score_metric_and_position An object (or named vector) specifying the expected return score metric and its associated position. Required if `sb_backtest_results` is not provided.
#' @param eligibility_quantile_range A numeric vector of length 2 (e.g., c(0.9, 1.0)) specifying the quantile range used to determine eligible assets.
#' @param min_eligible_assets_fallback A numeric value indicating the minimum number of eligible assets to include in the portfolio.
#' @param chosen_scaler An object of class `scaler` specifying the scaling variable to be applied to the scores.
#' @param scaler_shrinkage A numeric value between 0 and 1 indicating the shrinkage intensity for the scaler.
#' @param use_raw_for_eligibility A logical value indicating whether to use raw scores for determining eligibility.
#' @param enable_group_representativeness Logical; if TRUE, ensures at least one asset in all groups in groups_m_d_ref
#' @param selected_benchmark A character string indicating the benchmark to use for benchmark-relative backtests.
#' @param initial_buffer_period A numeric value indicating the number of initial dates to skip before starting the backtest.
#' @param rebalancing_months A numeric vector (e.g., c(3,6,9,12)) indicating the months when the portfolio should be rebalanced.
#' @param cov_est_method A `cov_est_method` object specifying the covariance estimation method and its parameters.
#' If not provided, a default is created using method "sample" and sample size 252; `active_returns` is set to `TRUE`
#' (with `cov_matrix_benchmark = selected_benchmark`) when a `selected_benchmark` is supplied, and `FALSE` otherwise.
#' @param port_construction_method A character string representing the portfolio construction method.
#' Must be one of "ew" (equal-weight), "sw" (signal-weight), "cw" (cap-weight), "cs" (cap-scaled), "rp" (risk parity),
#' "hrp" (hierarchical risk parity), "mvo" (mean-variance optimization), or "mmaf" (micro-macro allocation framework).
#' "custom_weights" is not supported through this constructor.
#' @param mvo_parameters An object of class `mvo_parameters` for mean-variance optimization. Only required if `port_construction_method` is "mvo".
#' If missing and port_construction_method is "mvo", a default is created.
#' @param rp_parameters An object of class `rp_parameters` for risk parity portfolios. Only required if `port_construction_method` is "rp".
#' If missing and port_construction_method is "rp", a default is created.
#' @param hrp_parameters An object of class `hrp_parameters` for hierarchical risk parity portfolios. Only required if `port_construction_method` is "hrp".
#' If missing and port_construction_method is "hrp", a default is created.
#' @param mmaf_parameters An object of class `mmaf_parameters` for micro-macro allocation framework portfolios. Only required if `port_construction_method` is "mmaf".
#' If missing and port_construction_method is "mmaf", a default is created (and `enable_group_representativeness` defaults to `TRUE`).
#' @param main_liquidity_metric A character string indicating which liquidity metric (i.e. column in liquidity_m_df) to use.
#' @param liquidity_floor_cutoffs An object (e.g., a data frame) containing liquidity cutoff values.
#' @param liquidity_constraint_policy An object of class `liquidity_constraint_policy` (optional).
#' @param turnover_constraint_policy An object of class `turnover_constraint_policy` (optional).
#' @param concentration_constraint_policy An object of class `concentration_constraint_policy` (optional).
#' @param transaction_costs_parameters An object specifying transaction cost parameters (optional).
#' @param config_name A character string representing the name of the configuration.
#'
#' @return An object of class `port_backtest_config`.
#'
#' @examples
#' # Minimal equal-weighted configuration driven by a single characteristic signal
#' # (a book-yield tilt), rebalanced semi-annually after a 12-period buffer.
#' config <- create_port_backtest_config(
#'   chosen_score_metric_and_position = c(book_yield = "long"),
#'   eligibility_quantile_range = c(0.8, 1.0),
#'   initial_buffer_period = 12,
#'   rebalancing_months = c(6, 12),
#'   main_liquidity_metric = "mean_volfin_3m",
#'   port_construction_method = "ew",
#'   config_name = "ew_book_yield"
#' )
#'
#' # Benchmark-relative risk-parity configuration: supplying a selected_benchmark
#' # makes the default cov_est_method use active returns against that benchmark.
#' rp_config <- create_port_backtest_config(
#'   chosen_score_metric_and_position = c(book_yield = "long"),
#'   selected_benchmark = "ibov",
#'   initial_buffer_period = 12,
#'   rebalancing_months = 12,
#'   main_liquidity_metric = "mean_volfin_3m",
#'   port_construction_method = "rp",
#'   config_name = "rp_book_yield"
#' )
#' @export
create_port_backtest_config <- function(chosen_score_metric_and_position = NULL,
                                        eligibility_quantile_range = c(0.9, 1.0),
                                        min_eligible_assets_fallback = NULL,
                                        chosen_scaler = NULL,
                                        scaler_shrinkage = NULL,
                                        use_raw_for_eligibility = NULL,
                                        enable_group_representativeness = NULL,
                                        selected_benchmark = NULL,
                                        initial_buffer_period,
                                        rebalancing_months,
                                        cov_est_method = NULL,
                                        port_construction_method = "ew",
                                        mvo_parameters = NULL,
                                        rp_parameters = NULL,
                                        hrp_parameters = NULL,
                                        mmaf_parameters = NULL,
                                        main_liquidity_metric,
                                        liquidity_floor_cutoffs = NULL,
                                        liquidity_constraint_policy = NULL,
                                        turnover_constraint_policy = NULL,
                                        concentration_constraint_policy = NULL,
                                        transaction_costs_parameters = NULL,
                                        config_name = "not_identified") {
  # Create a default covariance estimation method if none is provided
  if (is.null(cov_est_method)) {
    if (!is.null(selected_benchmark)) {
      cov_est_method <- create_cov_est_method(
        cov_estimation_method = "sample",
        cov_matrix_sample_size = 252,
        active_returns = TRUE,
        cov_matrix_benchmark = selected_benchmark
      )
    } else {
      cov_est_method <- create_cov_est_method(
        cov_estimation_method = "sample",
        cov_matrix_sample_size = 252,
        active_returns = FALSE,
        cov_matrix_benchmark = NULL
      )
    }
  }

  # If the portfolio construction method is "mvo" but no mvo_parameters provided, create defaults
  if (port_construction_method == "mvo" && is.null(mvo_parameters)) {
    mvo_parameters <- create_mvo_parameters(
      opt_method = "random",
      random_ports_method = "sample",
      n_random_ports = 1000,
      opt_objective = "sharpe",
      ridge_pen = NULL,
      n_resamples = 0,
      exp_ret_score_jitter = 0,
      cov_eigval_jitter = 0
    )
  }

  # Similarly, if the method is "rp" and no rp_parameters are provided, create defaults
  if (port_construction_method == "rp" && is.null(rp_parameters)) {
    rp_parameters <- create_rp_parameters(
      rp_method = "cyclical-spinu",
      exp_ret_score_tilt = "none",
      exp_ret_score_tilt_eta = NULL
    )
  }

  # If method is hrp and no hrp_parameters provided, create defaults
  if (port_construction_method == "hrp" && is.null(hrp_parameters)) {
    hrp_parameters <- create_hrp_parameters(
      linkage = "single",
      exp_ret_score_tilt = "none",
      exp_ret_score_tilt_eta = NULL
    )
  }

  # If method is mmaf and no mmaf_parameters provided, create defaults
  if (port_construction_method == "mmaf" && is.null(mmaf_parameters)) {
    mmaf_parameters <- create_mmaf_parameters(
      mmaf_method = "bottom_up",
      top_down_proxy_port_method = NULL,
      mmaf_group_col = NULL,
      micro_port_construction_method = "ew",
      macro_port_construction_method = "ew"
    )
  }

  # If method is mmaf and enable_group_representativeness is NULL, set it to TRUE and message
  if (port_construction_method == "mmaf" && is.null(enable_group_representativeness)) {
    enable_group_representativeness <- TRUE
    message("enable_group_representativeness set to TRUE for mmaf port_construction_method.")
  }

  # Create and return the new port_backtest_config object
  methods::new("port_backtest_config",
               chosen_score_metric_and_position = chosen_score_metric_and_position,
               min_eligible_assets_fallback = min_eligible_assets_fallback,
               eligibility_quantile_range = eligibility_quantile_range,
               selected_benchmark = selected_benchmark,
    initial_buffer_period = initial_buffer_period,
    rebalancing_months = rebalancing_months,
    chosen_scaler = chosen_scaler,
    scaler_shrinkage = scaler_shrinkage,
    use_raw_for_eligibility = use_raw_for_eligibility,
    enable_group_representativeness = enable_group_representativeness,
    cov_est_method = cov_est_method,
    port_construction_method = port_construction_method,
    mvo_parameters = mvo_parameters,
    rp_parameters = rp_parameters,
    hrp_parameters = hrp_parameters,
    mmaf_parameters = mmaf_parameters,
    main_liquidity_metric = main_liquidity_metric,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs,
    liquidity_constraint_policy = liquidity_constraint_policy,
    turnover_constraint_policy = turnover_constraint_policy,
    concentration_constraint_policy = concentration_constraint_policy,
    transaction_costs_parameters = transaction_costs_parameters,
    config_name = config_name
  )
}




# concentration_constraint_policy----------------------------------------
#' @title Create Concentration Constraint Policy
#' @description Constructor for a `concentration_constraint_policy` object.
#'
#' @param benchmark A character vector (can be empty if no benchmark specified).
#' @param max_abs_active_individual_weight A numeric indicating the max
#'   absolute active weight for individual assets.
#' @param max_abs_active_group_weight A named numeric vector for group constraints.
#'
#' @return An S4 object of class `concentration_constraint_policy`.
#' @export
create_concentration_constraint_policy <- function(
    benchmark = character(0),
    max_abs_active_individual_weight = NA_real_,
    max_abs_active_group_weight = numeric(0)) {

  ## If provided, max_abs_active_group_weight must be named
  if (length(max_abs_active_group_weight) > 0 && is.null(names(max_abs_active_group_weight))) {
    stop("max_abs_active_group_weight must be a named numeric vector.")
  }

  ## If provided, max_abs_active must be a numeric (not named)
  if (length(max_abs_active_individual_weight) > 0 && !is.numeric(max_abs_active_individual_weight)) {
    stop("max_abs_active_individual_weight must be a numeric value.")
  }

  ## Benchmark must be always provided and should be a character
  if (!is.character(benchmark)) {
    stop("benchmark must be a character vector (can be empty if no benchmark specified).")
  }

  obj <- methods::new(
    "concentration_constraint_policy",
    benchmark = benchmark,
    max_abs_active_individual_weight = max_abs_active_individual_weight,
    max_abs_active_group_weight = max_abs_active_group_weight
  )
  methods::validObject(obj)
  obj
}


#' @title Add Concentration Constraint Policy
#' @description Either add an existing \code{concentration_constraint_policy} to an object
#' (e.g., \code{port_backtest_config} or \code{sb_backtest_config}), or create one dynamically
#' when \code{policy} is missing.
#'
#' @param object An object of class \code{port_backtest_config} or \code{sb_backtest_config}.
#' @param policy A \code{concentration_constraint_policy} object, or missing if a new one is to be created.
#' @param benchmark A character vector indicating the benchmark to be used.
#' @param max_abs_active_individual_weight A numeric indicating the max absolute active weight for individual assets.
#' @param max_abs_active_group_weight A named numeric vector for group constraints.
#' @param ... Additional arguments used to create a new \code{concentration_constraint_policy}
#'   if \code{policy} is missing. These typically include:
#'   \itemize{
#'     \item \strong{benchmark} (character)
#'     \item \strong{max_abs_active_individual_weight} (numeric)
#'     \item \strong{max_abs_active_group_weight} (named numeric)
#'   }
#'
#' @return The updated \code{object} with the concentration policy added.
#' @export
setGeneric("add_concentration_constraint_policy", function(object, policy, ...) {
  standardGeneric("add_concentration_constraint_policy")
})

#' @describeIn add_concentration_constraint_policy
#'   Add an existing \code{concentration_constraint_policy} to a \code{port_backtest_config}.
#' @export
setMethod(
  "add_concentration_constraint_policy",
  signature(object = "port_backtest_config", policy = "concentration_constraint_policy"),
  function(object, policy, ...) {
    # Check for a selected_benchmark
    if (length(object@selected_benchmark) == 0) {
      stop("A selected_benchmark must be provided to add a concentration constraint policy.")
    }

    # Check if port_construction_method is 'mvo' or 'mmaf'
    if (!(object@port_construction_method %in% c("mvo", "mmaf"))) {
      stop("Concentration constraint policy can only be added to 'mvo' or 'mmaf' port_construction_method.")
    }

    # If group constraints are provided and enable_group_representativeness is NULL, set it to TRUE
    if (length(policy@max_abs_active_group_weight) > 0 && is.null(object@enable_group_representativeness)) {
      object@enable_group_representativeness <- TRUE
      message("enable_group_representativeness set to TRUE due to presence of group constraints in concentration constraint policy.
               Set it manually if different behavior is desired.")
    }

    object@concentration_constraint_policy <- policy
    methods::validObject(object) # optional validity check
    return(object)
  }
)
#' @describeIn add_concentration_constraint_policy
#'   Dynamically create a \code{concentration_constraint_policy} and add it to a \code{port_backtest_config}.
#' @export
setMethod(
  "add_concentration_constraint_policy",
  signature(object = "port_backtest_config", policy = "missing"),
  function(object,
           policy,
           max_abs_active_individual_weight = NULL,
           max_abs_active_group_weight = NULL,
           ...) {
    # Check for a selected_benchmark
    if (length(object@selected_benchmark) == 0) {
      stop("A selected_benchmark must be provided to add a concentration constraint policy.")
    }

    # Get benchmark from object
    selected_benchmark <- object@selected_benchmark

    # Check if port_construction_method is 'mvo' or 'mmaf'
    if (!(object@port_construction_method %in% c("mvo", "mmaf", "rp"))) {
      stop("Concentration constraint policy can only be added to 'rp', 'mvo' or 'mmaf' port_construction_method.")
    }

    # Build a new policy on the fly
    new_policy <- create_concentration_constraint_policy(
      benchmark = selected_benchmark,
      max_abs_active_individual_weight = max_abs_active_individual_weight,
      max_abs_active_group_weight = max_abs_active_group_weight
    )

    object@concentration_constraint_policy <- new_policy

    # If group constraints are provided and enable_group_representativeness is NULL, set it to TRUE
    if (length(max_abs_active_group_weight) > 0 && is.null(object@enable_group_representativeness)) {
      object@enable_group_representativeness <- TRUE
      message("enable_group_representativeness set to TRUE due to presence of group constraints in concentration constraint policy.
               Set it manually if different behavior is desired.")
    }

    methods::validObject(object)
    return(object)
  }
)



#' @describeIn add_concentration_constraint_policy
#'   Add an existing \code{concentration_constraint_policy} to a \code{sb_backtest_config}.
#'   This method will store it inside \code{object@signal_port_parameters}.
#' @export
setMethod(
  "add_concentration_constraint_policy",
  signature(object = "sb_backtest_config", policy = "concentration_constraint_policy"),
  function(object, policy, ...) {
    # Ensure signal_port_parameters is defined
    if (!methods::is(object@signal_port_parameters, "signal_port_parameters")) {
      object@signal_port_parameters <- methods::new("signal_port_parameters")
    }

    object@signal_port_parameters@concentration_constraint_policy <- policy
    methods::validObject(object)
    return(object)
  }
)

#' @describeIn add_concentration_constraint_policy
#'   Dynamically create a \code{concentration_constraint_policy} for \code{sb_backtest_config}.
#' @export
setMethod(
  "add_concentration_constraint_policy",
  signature(object = "sb_backtest_config", policy = "missing"),
  function(object,
           policy,
           benchmark,
           max_abs_active_individual_weight = NULL,
           max_abs_active_group_weight = NULL,
           ...) {
    # Build a new policy
    new_policy <- create_concentration_constraint_policy(
      benchmark = benchmark,
      max_abs_active_individual_weight = max_abs_active_individual_weight,
      max_abs_active_group_weight = max_abs_active_group_weight
    )

    # Ensure signal_port_parameters is defined
    if (!methods::is(object@signal_port_parameters, "signal_port_parameters")) {
      object@signal_port_parameters <- methods::new("signal_port_parameters")
    }

    # Assign
    object@signal_port_parameters@concentration_constraint_policy <- new_policy
    methods::validObject(object)
    return(object)
  }
)



# liquidity_constraint_policy-------------------------------------------
#' @title Create Liquidity Constraint Policy
#' @description Constructor for a `liquidity_constraint_policy` object.
#'
#' @param liquidity_floor_rule A character string indicating the minimum liquidity classification.
#' @param liquidity_cap_rules A named numeric vector indicating liquidity cap rules.
#'
#' @return An S4 object of class `liquidity_constraint_policy`.
#' @export
create_liquidity_constraint_policy <- function(liquidity_floor_rule = NULL,
                                               liquidity_cap_rules = NULL) {
  obj <- methods::new("liquidity_constraint_policy",
    liquidity_floor_rule = liquidity_floor_rule,
    liquidity_cap_rules = liquidity_cap_rules
  )
  methods::validObject(obj)
  obj
}

#' @title Add Liquidity Constraint Policy
#' @description Add an existing or dynamically create a `liquidity_constraint_policy`
#' to a portfolio backtest configuration object.
#'
#' @param object An object of class `port_backtest_config`.
#' @param policy A `liquidity_constraint_policy` object. If missing, a new one is created.
#' @param liquidity_floor_rule A character string (see details) used to create a new policy when `policy` is missing.
#' @param liquidity_cap_rules A named numeric vector used to create a new policy when `policy` is missing.
#' @param ... Additional arguments (currently unused).
#'
#' @return The updated `object` with the liquidity constraint policy added.
#' @export
setGeneric("add_liquidity_constraint_policy", function(object, policy, ...) {
  standardGeneric("add_liquidity_constraint_policy")
})

#' @describeIn add_liquidity_constraint_policy
#'   Add an existing `liquidity_constraint_policy` to a `port_backtest_config`.
#' @export
setMethod(
  "add_liquidity_constraint_policy",
  signature(object = "port_backtest_config", policy = "liquidity_constraint_policy"),
  function(object, policy, ...) {
    object@liquidity_constraint_policy <- policy

    methods::validObject(object)
    return(object)
  }
)

#' @describeIn add_liquidity_constraint_policy
#'   Dynamically create a `liquidity_constraint_policy` and add it to a `port_backtest_config`.
#' @export
setMethod(
  "add_liquidity_constraint_policy",
  signature(object = "port_backtest_config", policy = "missing"),
  function(object, policy, liquidity_floor_rule = NULL, liquidity_cap_rules = NULL, ...) {
    new_policy <- create_liquidity_constraint_policy(
      liquidity_floor_rule = liquidity_floor_rule,
      liquidity_cap_rules = liquidity_cap_rules
    )
    object@liquidity_constraint_policy <- new_policy

    methods::validObject(object)
    return(object)
  }
)



# turnover_constraint_policy--------------------------------------------
#' @title Create Turnover Constraint Policy
#' @description Constructor for a `turnover_constraint_policy` object.
#'
#' @param quantile_range_buffer A numeric value indicating the increase in the quantile range for buffer zones.
#' @param turnover_cap_rules A named numeric vector indicating turnover cap rules.
#'
#' @return An S4 object of class `turnover_constraint_policy`.
#' @export
create_turnover_constraint_policy <- function(quantile_range_buffer,
                                              turnover_cap_rules = NULL) {
  obj <- methods::new("turnover_constraint_policy",
    quantile_range_buffer = quantile_range_buffer,
    turnover_cap_rules = turnover_cap_rules
  )
  methods::validObject(obj)
  obj
}

#' @title Add Turnover Constraint Policy
#' @description Add an existing or dynamically create a `turnover_constraint_policy`
#' to a portfolio backtest configuration object.
#'
#' @param object An object of class `port_backtest_config` or `sb_backtest_config`.
#' @param policy A `turnover_constraint_policy` object. If missing, a new one is created.
#' @param quantile_range_buffer A numeric value used to create a new policy when `policy` is missing.
#' @param turnover_cap_rules A named numeric vector used to create a new policy when `policy` is missing.
#' @param ... Additional arguments (currently unused).
#'
#' @return The updated `object` with the turnover constraint policy added.
#' @export
setGeneric("add_turnover_constraint_policy", function(object, policy, ...) {
  standardGeneric("add_turnover_constraint_policy")
})

#' @describeIn add_turnover_constraint_policy
#'   Add an existing `turnover_constraint_policy` to a `port_backtest_config`.
#' @export
setMethod(
  "add_turnover_constraint_policy",
  signature(object = "port_backtest_config", policy = "turnover_constraint_policy"),
  function(object, policy, ...) {
    # Check if port_construction_method is 'mvo'
    if (object@port_construction_method != "mvo") {
      stop("Concentration constraint policy can only be added to a 'mvo' port_construction_method.")
    }

    object@turnover_constraint_policy <- policy
    methods::validObject(object)
    return(object)
  }
)

#' @describeIn add_turnover_constraint_policy
#'   Dynamically create a `turnover_constraint_policy` and add it to a `port_backtest_config`.
#' @export
setMethod(
  "add_turnover_constraint_policy",
  signature(object = "port_backtest_config", policy = "missing"),
  function(object, policy, quantile_range_buffer, turnover_cap_rules, ...) {
    # Check if port_construction_method is 'mvo'
    if (object@port_construction_method != "mvo") {
      stop("Concentration constraint policy can only be added to a 'mvo' port_construction_method.")
    }

    new_policy <- create_turnover_constraint_policy(
      quantile_range_buffer = quantile_range_buffer,
      turnover_cap_rules = turnover_cap_rules
    )
    object@turnover_constraint_policy <- new_policy
    methods::validObject(object)
    return(object)
  }
)



# transaction_cost_parameters-------------------------------------------
#' Create a New Transaction Cost Parameters Object
#'
#' This function constructs a new \code{transaction_cost_parameters} S4 object.
#'
#' @param direct_transaction_cost A numeric value for the direct transaction cost.
#' @param strategy_aum A numeric value for the strategy's assets under management.
#' @param alpha A numeric value for the alpha parameter.
#' @param lambda A numeric value or the string "dynamic" for the lambda parameter.
#'
#' @return An object of class \code{transaction_cost_parameters}.
#'
#' @export
create_transaction_costs_parameters <- function(direct_transaction_cost, strategy_aum, alpha, lambda) {
  transaction_costs_parameters <- list(
    direct_transaction_cost = direct_transaction_cost,
    strategy_aum = strategy_aum,
    alpha = alpha,
    lambda = lambda
  )

  # Validate the parameters using the validation function
  validate_transaction_costs_parameters(transaction_costs_parameters)

  methods::new("transaction_costs_parameters",
    direct_transaction_cost = direct_transaction_cost,
    strategy_aum = strategy_aum,
    alpha = alpha,
    lambda = lambda
  )
}

#' Add Transaction Cost Parameters to a Portfolio Backtest Configuration
#'
#' This generic function adds an existing or dynamically creates a
#' \code{transaction_cost_parameters} object to a portfolio backtest configuration object.
#'
#' @param object An object of class \code{port_backtest_config}.
#' @param transaction_costs_parameters A \code{transaction_cost_parameters} object. If missing, a new one is created.
#' @param ... Additional arguments used when creating a new transaction cost parameters object.
#'
#' @return The updated \code{object} with the transaction cost parameters added.
#'
#' @export
setGeneric("add_transaction_costs_parameters", function(object, transaction_costs_parameters, ...) {
  standardGeneric("add_transaction_costs_parameters")
})

#' @describeIn add_transaction_costs_parameters
#'   Add an existing \code{transaction_cost_parameters} to a \code{port_backtest_config}.
#' @export
setMethod(
  "add_transaction_costs_parameters",
  signature(object = "port_backtest_config", transaction_costs_parameters = "transaction_costs_parameters"),
  function(object, transaction_costs_parameters, ...) {
    object@transaction_costs_parameters <- transaction_costs_parameters
    methods::validObject(object)
    return(object)
  }
)

#' @describeIn add_transaction_costs_parameters
#'   Dynamically create a \code{transaction_cost_parameters} object and add it to a \code{port_backtest_config}.
#'
#'   Additional arguments (such as \code{direct_transaction_cost}, \code{strategy_aum}, \code{alpha}, and \code{lambda})
#'   are passed to \code{new_transaction_cost_parameters}.
#' @export
setMethod(
  "add_transaction_costs_parameters",
  signature(object = "port_backtest_config", transaction_costs_parameters = "missing"),
  function(object, transaction_costs_parameters, ...) {
    new_tc_params <- create_transaction_costs_parameters(...)
    object@transaction_costs_parameters <- new_tc_params
    methods::validObject(object)
    return(object)
  }
)




# liquidity_floor_cutoffs-----------------------------------------------
#' @title Create Liquidity Floor Cutoffs
#' @description Construct a liquidity_floor_cutoffs data frame from scratch.
#'
#' @param metric_name A character vector of metric names.
#' @param metric_cutoffs A list of named numeric vectors, one for each metric in
#'   metric_name. Each vector must have names exactly equal to
#'   c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps") (order may vary).
#'
#' @return A data.frame with a column `liquidity_classification` and one column per metric.
#'   The rows are ordered (after reordering each metric vector according to the allowed levels)
#'   so that the main liquidity metric (assumed to be the first element of metric_name)
#'   is in non-decreasing order.
#'
#' @details The function enforces that:
#'   \itemize{
#'     \item The returned object is a data.frame.
#'     \item The column names (besides `liquidity_classification`) are numeric with no NAs.
#'     \item The `liquidity_classification` values are exactly the allowed levels:
#'       "micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps".
#'     \item The main liquidity metric (the first metric in metric_name) is in ascending order.
#'     \item The ordering (ranking) of liquidity classifications is consistent across metrics.
#'   }
#'
#' @export
create_liquidity_floor_cutoffs <- function(metric_name, metric_cutoffs) {
  # Allowed liquidity classifications
  allowed_levels <- c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")

  # Check that lengths match
  if (length(metric_name) != length(metric_cutoffs)) {
    stop("Length of metric_name must equal length of metric_cutoffs")
  }

  # Process each metric: check numeric, names, and reorder according to allowed_levels.
  metrics_list <- list()
  for (i in seq_along(metric_name)) {
    vec <- metric_cutoffs[[i]]
    if (!is.numeric(vec)) {
      stop(paste0("Metric '", metric_name[i], "' must be numeric"))
    }
    if (is.null(names(vec))) {
      stop(paste0("Metric '", metric_name[i], "' must be a named vector"))
    }
    if (!setequal(names(vec), allowed_levels)) {
      stop(paste0(
        "Metric '", metric_name[i], "' must have names exactly: ",
        paste(allowed_levels, collapse = ", ")
      ))
    }
    # Reorder the vector so that its values are arranged by the allowed levels.
    vec <- vec[allowed_levels]
    metrics_list[[metric_name[i]]] <- vec
  }

  # Build the data.frame: first column is liquidity_classification.
  df <- data.frame(liquidity_classification = allowed_levels, stringsAsFactors = FALSE)
  for (nm in metric_name) {
    df[[nm]] <- unname(metrics_list[[nm]])
  }

  # Ensure all metric columns are numeric and free of NAs.
  if (!all(sapply(df[, -1, drop = FALSE], is.numeric))) {
    stop("All metric columns must be numeric")
  }
  if (any(is.na(df))) {
    stop("liquidity_floor_cutoffs elements must not have NAs")
  }

  # Check that the main liquidity metric (first metric) is in ascending order.
  main_metric <- metric_name[1]
  if (!all(diff(df[[main_metric]]) >= 0)) {
    stop("liquidity_floor_cutoffs is not in ascending order based on the main liquidity metric")
  }

  # Check that ordering of all metrics is consistent.
  orders_matrix <- sapply(df[, metric_name, drop = FALSE], function(x) order(x))
  for (i in seq_len(nrow(orders_matrix))) {
    if (length(unique(orders_matrix[i, ])) != 1) {
      stop("liquidity metrics orders in liquidity_floor_cutoffs are conflicting")
    }
  }

  return(df)
}

##add_liquidity_floor_cutoffs

#' @title Add Liquidity Floor Cutoffs
#' @description Add or update liquidity floor cutoff values in an existing object.
#'
#' @param object An object of class `port_backtest_config` or `sb_backtest_config`.
#'   The object must have a slot `liquidity_floor_cutoffs` (which may be NULL)
#'   and a character slot `main_liquidity_metric`.
#' @param metric_name A character vector (or a single character) specifying the metric(s)
#'   to add or update.
#' @param metric_cutoffs For each metric, a named numeric vector containing cutoff values.
#'   For a single metric, this can be a named numeric vector.
#'
#' @return The updated `object` with its `liquidity_floor_cutoffs` slot merged with the new values.
#'
#' @details If the object already has a liquidity_floor_cutoffs data.frame, the function:
#'   \itemize{
#'     \item Ensures that rows exist for all allowed liquidity classifications.
#'     \item For each provided metric, if the metric column already exists, it updates the rows
#'       corresponding to the names provided in `metric_cutoffs`; otherwise a new column is added.
#'     \item After merging, the resulting data.frame is validated against the enforced structure:
#'       it must be a data.frame, contain the main liquidity metric, be sorted in ascending order,
#'       have consistent orders across metrics, contain only allowed liquidity classifications,
#'       have numeric columns (except for the classification column) and contain no NAs.
#'   }
#'
#' @export
add_liquidity_floor_cutoffs <- function(object, metric_name, metric_cutoffs) {
  # Allowed liquidity classifications
  allowed_levels <- c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")

  # Ensure metric_cutoffs is a list when multiple metrics are provided.
  if (length(metric_name) > 1 && !is.list(metric_cutoffs)) {
    stop("When multiple metric names are provided, metric_cutoffs must be a list")
  }
  if (length(metric_name) == 1 && !is.list(metric_cutoffs)) {
    metric_cutoffs <- list(metric_cutoffs)
  }

  # If no liquidity_floor_cutoffs exists, create one from scratch.
  if (is.null(object@liquidity_floor_cutoffs)) {
    # For a new liquidity_floor_cutoffs, each metric must provide values for all allowed levels.
    for (i in seq_along(metric_name)) {
      vec <- metric_cutoffs[[i]]
      if (!setequal(names(vec), allowed_levels)) {
        stop(paste0(
          "For new liquidity_floor_cutoffs, metric '", metric_name[i],
          "' must have values for all liquidity classifications: ",
          paste(allowed_levels, collapse = ", ")
        ))
      }
    }
    new_df <- create_liquidity_floor_cutoffs(metric_name, metric_cutoffs)
    object@liquidity_floor_cutoffs <- new_df
    return(object)
  }

  # Otherwise, merge new information with the existing liquidity_floor_cutoffs.
  df <- object@liquidity_floor_cutoffs
  if (!is.data.frame(df)) {
    stop("liquidity_floor_cutoffs must be a data.frame")
  }
  if (!"liquidity_classification" %in% colnames(df)) {
    stop("liquidity_floor_cutoffs must have a 'liquidity_classification' column")
  }

  # Ensure that all allowed liquidity classifications are present.
  missing_levels <- setdiff(allowed_levels, df$liquidity_classification)
  if (length(missing_levels) > 0) {
    new_rows <- data.frame(liquidity_classification = missing_levels, stringsAsFactors = FALSE)
    for (col in setdiff(colnames(df), "liquidity_classification")) {
      new_rows[[col]] <- NA_real_
    }
    df <- rbind(df, new_rows)
  }
  # Reorder rows according to allowed_levels.
  df <- df[match(allowed_levels, df$liquidity_classification), , drop = FALSE]

  # For each provided metric, update or add its column.
  for (i in seq_along(metric_name)) {
    nm <- metric_name[i]
    vec <- metric_cutoffs[[i]]
    if (!is.numeric(vec) || is.null(names(vec))) {
      stop(paste0("Metric '", nm, "' must be a named numeric vector"))
    }
    if (!all(names(vec) %in% allowed_levels)) {
      stop(paste0("Metric '", nm, "' names must be among: ", paste(allowed_levels, collapse = ", ")))
    }
    if (nm %in% colnames(df)) {
      # Update the specified liquidity classifications.
      for (lev in names(vec)) {
        idx <- which(df$liquidity_classification == lev)
        df[idx, nm] <- vec[lev]
      }
    } else {
      # Create a new column (with NA) and update the specified rows.
      df[[nm]] <- NA_real_
      for (lev in names(vec)) {
        idx <- which(df$liquidity_classification == lev)
        df[idx, nm] <- vec[lev]
      }
    }
  }

  # After merging, ensure no NAs remain in any metric column.
  metric_cols <- setdiff(colnames(df), "liquidity_classification")
  if (any(is.na(df[, metric_cols]))) {
    stop("After adding, liquidity_floor_cutoffs elements must not have NAs")
  }

  # Check that the main liquidity metric is present.
  main_metric <- object@main_liquidity_metric
  if (!(main_metric %in% colnames(df))) {
    stop("main_liquidity_metric is not represented in liquidity_floor_cutoffs")
  }

  # Ensure that df is sorted in ascending order by the main liquidity metric.
  df_sorted <- dplyr::arrange(df, !!rlang::sym(main_metric))
  if (!all(df_sorted$liquidity_classification == df$liquidity_classification)) {
    stop("liquidity_floor_cutoffs is not in ascending order based on main_liquidity_metric")
  }

  # Check that the ordering (ranking) is consistent across all metric columns.
  orders_matrix <- sapply(df[, metric_cols, drop = FALSE], function(x) order(x))
  for (i in seq_len(nrow(orders_matrix))) {
    if (length(unique(orders_matrix[i, ])) != 1) {
      stop("liquidity metrics orders in liquidity_floor_cutoffs are conflicting")
    }
  }

  object@liquidity_floor_cutoffs <- df
  return(object)
}



# create_port_backtest_cohort--------------------------------------------
#' Create Portfolio Backtest Cohort
#'
#' This function creates a `port_backtest_cohort` object by merging a list of
#' `port_backtest_results` objects. It checks for compatibility across backtests by
#' verifying that key elements of the `port_backtest_workflow` slot are identical.
#'
#' The function then merges:
#' \itemize{
#'   \item{\strong{port_weights_m_df:}} Merges the underlying meta data.frames (accessed via `@data`) by matching `id`, `tickers` and `dates`.
#'         The individual `eop_port_weights` columns are renamed to the corresponding backtest identifier.
#'         If a benchmark was used (i.e. `selected_benchmark` is not `NULL`), the common `bench_weights` column is
#'         checked for consistency and included only once.
#'   \item{\strong{port_costs_m_xts:}} For each cost type (direct_cost, market_impact_cost, total_cost, turnover),
#'         the function extracts the respective column from each backtest’s `@port_costs_m_xts@data`,
#'         renames it to the backtest identifier, and then merges them (after checking that dates match).
#'   \item{\strong{port_returns_m_xts:}} Separately merges columns for raw_return, net_return, raw_active_return, and net_active_return.
#'         For raw_return and net_return, if a benchmark is present the common `selected_bench_return` column is included
#'         after checking consistency.
#'   \item{\strong{port_metrics_m_xts:}} As the metrics may be built from custom stock metrics,
#'         the function iterates over the primary metric columns (i.e. those not starting with `bench_`)
#'         and merges them across backtests. If a corresponding bench column exists (and benchmarks are used)
#'         and is identical across backtests, it is included once.
#' }
#'
#' After merging, the function uses `create_meta_dataframe` and `create_meta_xts` (from your package)
#' to reconstruct the meta objects. The resulting `port_backtest_cohort` object contains the merged data
#' and the common backtest workflow parameters.
#'
#' @param port_backtest_results_list A list of `port_backtest_results` objects.
#' @param cohort_name A character string representing the name for the merged cohort.
#'
#' @return An object of class `port_backtest_cohort`.
#' @export
create_port_backtest_cohort <- function(port_backtest_results_list, cohort_name) {

  # Input Validation
  ## Check that backtest identifiers are unique
  if (length(unique(sapply(port_backtest_results_list, function(x) x@backtest_identifier))) != length(port_backtest_results_list)) {
    stop("Backtest identifiers must be unique.")
  }
  ## Check that port_backtest_results_list is a non-empty list
  if (!is.list(port_backtest_results_list) || length(port_backtest_results_list) == 0) {
    stop("port_backtest_results_list must be a non-empty list of port_backtest_results objects.")
  }
  ## Check that cohort_name is a single character string
  if (!is.character(cohort_name) || length(cohort_name) != 1) {
    stop("cohort_name must be a single character string.")
  }
  ## Check that all elements are of class 'port_backtest_results'
  if (!all(sapply(port_backtest_results_list, function(x) methods::is(x, "port_backtest_results")))) {
    stop("All elements of port_backtest_results_list must be of class 'port_backtest_results'.")
  }
  ## Ensure that all backtests use the same benchmark
  if (length(unique(sapply(port_backtest_results_list,
                           function(x) x@port_backtest_workflow[[length(x@port_backtest_workflow)]]$selected_benchmark))) > 1) {
    stop("All backtests must use the same benchmark.")
  }
  ## Ensure all backtest identifiers are unique
  if (length(unique(sapply(port_backtest_results_list, function(x) x@backtest_identifier))) != length(port_backtest_results_list)) {
    stop("All backtests must have unique backtest identifiers.")
  }

  # Step 1: Check Compatibility Across port_backtest_workflow Slots
  ## Define required parameters that must match across all backtests
  required_params <- c(
    "selected_benchmark", "dates_covered", "initial_buffer_period", "dates_backtest",
    "signals_object_name", "fwd_return_object_name", "stock_groups_object_name",
    "benchmark_returns_object_name", "daily_stocks_returns_object_name", "daily_bench_returns_object_name",
    "liquidity_object_name", "volatility_object_name", "benchmark_weights_object_name", "current_date"
  )

  ## Extract the workflow list from each backtest object
  workflow_list <- lapply(port_backtest_results_list, function(x) x@port_backtest_workflow[[length(x@port_backtest_workflow)]])
  ## Store the common values from the first backtest for comparison
  common_values <- workflow_list[[1]][required_params]
  ## Loop over each backtest and verify required parameters match
  for (i in seq_along(workflow_list)) {
    current_values <- workflow_list[[i]][required_params]
    for (param in required_params) {
      if (!identical(common_values[[param]], current_values[[param]])) {
        stop(paste("Incompatibility found in parameter:", param, "for backtest result at index", i))
      }
    }
  }
  ## Determine if a benchmark is used based on the selected_benchmark value
  benchmark_used <- !is.null(common_values[["selected_benchmark"]])

  # Step 2: Merge port_weights_m_df
  ## Extract and process the meta data.frames from each backtest
  weights_m_dfs <- lapply(port_backtest_results_list, function(x) {
    m_df <- x@port_weights_m_df@data
    ### Set required columns based on whether benchmark is used
    if (benchmark_used) {
      required_cols <- c("id", "tickers", "dates", "eop_port_weights", "bench_weights")
    } else {
      required_cols <- c("id", "tickers", "dates", "eop_port_weights")
    }
    ### Validate that required columns exist
    if (!all(required_cols %in% names(m_df))) {
      stop("Each port_weights_m_df must contain columns: ", paste(required_cols, collapse = ", "))
    }
    ### Rename 'eop_port_weights' column to the backtest identifier
    colnames(m_df)[colnames(m_df) == "eop_port_weights"] <- x@backtest_identifier
    m_df
  })

  ## Verify that id, tickers, dates (and bench_weights if applicable) match across all data.frames
  base_m_df <- weights_m_dfs[[1]][, c("id", "tickers", "dates", if (benchmark_used) "bench_weights" else NULL)]
  for (i in seq_along(weights_m_dfs)) {
    current_m_df <- weights_m_dfs[[i]][, c("id", "tickers", "dates", if (benchmark_used) "bench_weights" else NULL)]
    if (!all(base_m_df$id == current_m_df$id &
      base_m_df$tickers == current_m_df$tickers &
      base_m_df$dates == current_m_df$dates)) {
      stop("Mismatch in id, tickers, or dates in port_weights_m_df of backtest result at index ", i)
    }
    if (benchmark_used) {
      if (!all(base_m_df$bench_weights == current_m_df$bench_weights)) {
        stop("Mismatch in bench_weights in port_weights_m_df of backtest result at index ", i)
      }
    }
  }

  ## Merge the individual backtest eop_port_weights columns into one meta data.frame
  merged_weights_m_df <- base_m_df
  for (i in seq_along(weights_m_dfs)) {
    current_m_df <- weights_m_dfs[[i]][, c("id", setdiff(names(weights_m_dfs[[i]]), c("id", "tickers", "dates", if (benchmark_used) "bench_weights" else NULL))), drop = FALSE]
    merged_weights_m_df <- dplyr::left_join(merged_weights_m_df, current_m_df, by = "id")
  }
  ## If benchmark is used, ensure bench_weights is the last column in the merged data.frame
  if (benchmark_used) {
    bench_weights <- merged_weights_m_df$bench_weights
    merged_weights_m_df <- merged_weights_m_df %>%
      dplyr::select(-bench_weights) %>%
      dplyr::mutate(bench_weights = bench_weights)
  }

  ## Create the merged meta data.frame using create_meta_dataframe (type = "weights")
  merged_port_weights_m_df <- create_meta_dataframe(
    data = merged_weights_m_df,
    meta_dataframe_name = cohort_name,
    type = "weights"
  )

  # Step 3: Merge port_costs_m_xts
  ## Define the cost types to merge
  cost_types <- c("direct_cost", "market_impact_cost", "total_cost", "turnover")
  ## Initialize a list to store merged meta_xts objects for each cost type
  port_costs_m_xts_list <- list()
  ## Loop over each cost type and merge across backtests
  for (cost in cost_types) {
    cost_m_xts_list <- lapply(port_backtest_results_list, function(x) {
      m_xts <- x@port_costs_m_xts@data[, cost, drop = FALSE]
      colnames(m_xts) <- x@backtest_identifier
      m_xts
    })
    ### Check that dates match across all meta_xts objects for the cost type
    dates_list <- lapply(cost_m_xts_list, function(x) as.character(zoo::index(x)))
    if (!all(sapply(dates_list, function(d) identical(d, dates_list[[1]])))) {
      stop("Dates do not match across port_costs_m_xts for cost type: ", cost)
    }
    ### Merge the cost meta_xts objects
    merged_cost_m_xts <- do.call(xts::merge.xts, cost_m_xts_list)
    ### Create a meta_xts object for the cost type using create_meta_xts
    port_costs_m_xts_list[[paste0(cost, "_m_xts")]] <- create_meta_xts(
      data = merged_cost_m_xts,
      type = "metrics",
      meta_xts_name = cohort_name,
      metric_name = cost,
      source = sapply(port_backtest_results_list, function(x) x@backtest_identifier)
    )
  }

  # Step 4: Merge port_returns_m_xts
  ## If a benchmark is used, extract and verify the "selected_bench_return" column
  if (benchmark_used) {
    bench_returns_list <- lapply(port_backtest_results_list, function(x) {
      m_xts <- x@port_returns_m_xts@data[, "selected_bench_return", drop = FALSE]
      m_xts
    })
    bench_ref <- bench_returns_list[[1]]
    for (i in seq_along(bench_returns_list)) {
      if (!all(bench_returns_list[[i]] == bench_ref)) {
        stop("selected_bench_return mismatch in port_returns_m_xts for backtest result at index ", i)
      }
    }
  } else {
    bench_ref <- NULL
  }

  ## Define a helper function to merge a specified return column across backtests
  merge_return_column <- function(colname, include_bench = FALSE) {
    ret_m_xts_list <- lapply(port_backtest_results_list, function(x) {
      m_xts <- x@port_returns_m_xts@data[, colname, drop = FALSE]
      colnames(m_xts) <- x@backtest_identifier
      m_xts
    })
    ### Verify that dates match for the return column across backtests
    dates_list <- lapply(ret_m_xts_list, function(x) as.character(zoo::index(x)))
    if (!all(sapply(dates_list, function(d) identical(d, dates_list[[1]])))) {
      stop("Dates do not match across port_returns_m_xts for column: ", colname)
    }
    ### Merge the return meta_xts objects
    merged_ret_m_xts <- do.call(xts::merge.xts, ret_m_xts_list)
    ### If including benchmark data, append the benchmark return column
    if (include_bench && !is.null(bench_ref)) {
      merged_ret_m_xts <- cbind(merged_ret_m_xts, bench_ref)
      merged_ret_m_xts <- merged_ret_m_xts[, c(
        colnames(merged_ret_m_xts)[-ncol(merged_ret_m_xts)],
        colnames(merged_ret_m_xts)[ncol(merged_ret_m_xts)]
      )]
    }
    merged_ret_m_xts
  }

  ## Set include_bench flag based on benchmark_used and merge individual return columns using the helper function
  include_bench <- benchmark_used # Only include benchmark returns if benchmark is used
  raw_returns_m_xts <- merge_return_column("raw_return", include_bench = include_bench)
  net_returns_m_xts <- merge_return_column("net_return", include_bench = include_bench)
  raw_active_returns_m_xts <- if (benchmark_used) merge_return_column("raw_active_return", include_bench = FALSE) else NULL
  net_active_returns_m_xts <- if (benchmark_used) merge_return_column("net_active_return", include_bench = FALSE) else NULL
  ## Define the benchmark source (if applicable) and backtest sources
  if (include_bench) {
    bench_source <- port_backtest_results_list[[1]]@port_backtest_workflow[[length(port_backtest_results_list[[1]]@port_backtest_workflow)]]$benchmark_returns_object_name
  }
  backtest_sources <- sapply(port_backtest_results_list, function(x) x@backtest_identifier)

  ## Create the merged port_returns meta_xts objects using create_meta_xts
  port_returns_m_xts_list <- list(
    raw_returns_m_xts = create_meta_xts(
      data = raw_returns_m_xts,
      type = "returns",
      asset_type = "ports",
      meta_xts_name = cohort_name,
      metric_name = "raw_return",
      source = if (include_bench) c(backtest_sources, bench_source) else backtest_sources
    ),
    net_returns_m_xts = create_meta_xts(
      data = net_returns_m_xts,
      type = "returns",
      asset_type = "ports",
      meta_xts_name = cohort_name,
      metric_name = "net_return",
      source = if (include_bench) c(backtest_sources, bench_source) else backtest_sources
    )
  )

  if (benchmark_used) {
    port_returns_m_xts_list <- c(port_returns_m_xts_list,
      raw_active_returns_m_xts = create_meta_xts(
        data = raw_active_returns_m_xts,
        type = "returns",
        asset_type = "ports",
        meta_xts_name = cohort_name,
        metric_name = "raw_active_return",
        source = backtest_sources
      ),
      net_active_returns_m_xts = create_meta_xts(
        data = net_active_returns_m_xts,
        type = "returns",
        asset_type = "ports",
        meta_xts_name = cohort_name,
        metric_name = "net_active_return",
        source = backtest_sources
      )
    )
  }

  # Step 5: Merge port_metrics_m_xts
  ## Consolidate all primary metric names across all backtests (excluding bench_* columns)
  all_primary_metrics_list <- lapply(port_backtest_results_list, function(x) {
    if (is.null(x@port_metrics_m_xts)) {
      return(character(0))
    } else {
      cols <- colnames(x@port_metrics_m_xts@data)
      primary <- cols[!grepl("^bench_", cols)]
      return(primary)
    }
  })
  union_primary_metrics <- sort(unique(unlist(all_primary_metrics_list)))

  ## Initialize list for merged port_metrics meta_xts objects
  port_metrics_m_xts_list <- list()

  ## Loop over each metric in the consolidated union of primary metrics
  for (metric in union_primary_metrics) {
    ### Create a list to hold the metric data from each backtest (if available)
    metric_m_xts_list <- lapply(port_backtest_results_list, function(x) {
      if (!is.null(x@port_metrics_m_xts) && metric %in% colnames(x@port_metrics_m_xts@data)) {
        m_xts <- x@port_metrics_m_xts@data[, metric, drop = FALSE]
        colnames(m_xts) <- x@backtest_identifier
        return(m_xts)
      } else {
        return(NULL)
      }
    })
    ### Remove NULLs for backtests that do not have this metric
    metric_m_xts_list <- Filter(Negate(is.null), metric_m_xts_list)

    ### If no backtest has this metric, skip to the next metric
    if (length(metric_m_xts_list) == 0) next

    ### Check that dates match among the available metric data
    dates_list <- lapply(metric_m_xts_list, function(x) as.character(zoo::index(x)))
    if (length(dates_list) > 1 && !all(sapply(dates_list, function(d) identical(d, dates_list[[1]])))) {
      stop("Dates do not match across port_metrics_m_xts for metric: ", metric)
    }

    ### Merge the available metric meta_xts objects
    merged_metric_m_xts <- do.call(xts::merge.xts, metric_m_xts_list)

    ### Handle the corresponding bench metric column (if benchmark is used)
    bench_col <- paste0("bench_", metric)
    bench_m_xts_list <- lapply(port_backtest_results_list, function(x) {
      if (!is.null(x@port_metrics_m_xts) && benchmark_used && bench_col %in% colnames(x@port_metrics_m_xts@data)) {
        return(x@port_metrics_m_xts@data[, bench_col, drop = FALSE])
      } else {
        return(NULL)
      }
    })
    bench_m_xts_list <- Filter(Negate(is.null), bench_m_xts_list)

    ### Initialize bench_consistent flag as FALSE
    bench_consistent <- FALSE

    ### If bench metric data is available, check for consistency
    if (length(bench_m_xts_list) > 0) {
      bench_ref_metric <- bench_m_xts_list[[1]]
      bench_consistent <- TRUE
      for (bm in bench_m_xts_list) {
        if (!all(bm == bench_ref_metric)) {
          bench_consistent <- FALSE
          break
        }
      }
      #### If bench metric data is consistent, combine it with the merged metric data
      if (bench_consistent) {
        merged_metric_m_xts <- cbind(merged_metric_m_xts, bench_ref_metric)
        merged_metric_m_xts <- merged_metric_m_xts[, c(
          colnames(merged_metric_m_xts)[!grepl("^bench_", colnames(merged_metric_m_xts))],
          bench_col
        )]
      }
    }

    ### Define backtest sources for this metric
    backtest_sources <- sapply(port_backtest_results_list, function(x) {
      if (!is.null(x@port_metrics_m_xts) && metric %in% colnames(x@port_metrics_m_xts@data)) x@backtest_identifier
    }) %>% unlist()

    ### If bench metric was added, include the bench source as well
    if (bench_consistent) {
      bench_source <- port_backtest_results_list[[1]]@port_backtest_workflow[[length(port_backtest_results_list[[1]]@port_backtest_workflow)]]$benchmark_returns_object_name
      source_vector <- c(backtest_sources, bench_source)
    } else {
      source_vector <- backtest_sources
    }

    ### Create the consolidated meta_xts object for this metric using create_meta_xts
    port_metrics_m_xts_list[[paste0(metric, "_m_xts")]] <- create_meta_xts(
      data = merged_metric_m_xts,
      type = "metrics",
      meta_xts_name = cohort_name,
      metric_name = metric,
      source = source_vector
    )
  }

  # Step 6: Merge port_stats_m_df (family-aware: active vs regular)
  stats_m_dfs <- lapply(seq_along(port_backtest_results_list), function(i) {
    x <- port_backtest_results_list[[i]]
    if (is.null(x@port_stats_m_df)) return(NULL)
    m_df <- x@port_stats_m_df@data

    # Key columns
    required_keys <- c("id", "tickers", "dates")
    if (!all(required_keys %in% names(m_df))) {
      stop("Each port_stats_m_df must contain columns: id, tickers, dates.")
    }

    # Partition columns
    stat_cols <- setdiff(names(m_df), required_keys)
    is_act    <- grepl("^act_", stat_cols)
    is_group  <- grepl("^group_", stat_cols)
    is_info   <- stat_cols == "info_ratio"
    is_sharpe <- stat_cols == "sharpe"

    # Family checks
    if (benchmark_used) {
      # Allowed: act_* , info_ratio, group_*
      if (any(is_sharpe)) {
        stop("port_stats_m_df contains 'sharpe' while benchmark is used (expected 'info_ratio' / 'act_*'). Backtest index: ", i)
      }
    } else {
      # Allowed: regular (no act_*), sharpe, group_* ; disallow act_* and info_ratio
      if (any(is_act | is_info)) {
        bad <- paste(stat_cols[is_act | is_info], collapse = ", ")
        stop("Found active/info columns without benchmark (not allowed): ", bad,
             ". Backtest index: ", i)
      }
    }

    m_df
  })

  # Remove NULLs if some backtests don't expose stats
  stats_m_dfs <- Filter(Negate(is.null), stats_m_dfs)

  port_stats_m_df_list <- list()
  port_stats_m_xts_nested_list <- list(raw_return = list(), net_return = list())

  if (length(stats_m_dfs) > 0) {
    # 1) Validate alignment of keys across backtests
    base_keys_df <- stats_m_dfs[[1]][, c("id", "tickers", "dates")]
    for (i in seq_along(stats_m_dfs)) {
      keys_i <- stats_m_dfs[[i]][, c("id", "tickers", "dates")]
      if (!all(base_keys_df$id == keys_i$id &
               base_keys_df$tickers == keys_i$tickers &
               base_keys_df$dates == keys_i$dates)) {
        stop("Mismatch in id, tickers, or dates in port_stats_m_df of backtest result at index ", i)
      }
    }

    # 2) Build union of stat columns, respecting family rules
    union_stat_cols <- sort(unique(unlist(lapply(stats_m_dfs, function(df) {
      setdiff(names(df), c("id", "tickers", "dates"))
    }))))

    # Helper to convert merged (wide) data.frame to xts by return type
    build_stat_xts <- function(merged_df, ret_type) {
      sub <- merged_df[merged_df$tickers == ret_type, , drop = FALSE]
      if (nrow(sub) == 0) return(NULL)
      # Ensure date ordering and build matrix
      sub <- sub[order(sub$dates), ]
      value_cols <- setdiff(names(sub), c("id", "tickers", "dates"))
      if (length(value_cols) == 0) return(NULL)
      mat <- as.matrix(sub[, value_cols, drop = FALSE])
      rownames(mat) <- NULL
      # dates might already be Date; as.Date is safe
      xts::xts(mat, order.by = as.Date(sub$dates))
    }

    # 3) For each stat column, merge across backtests (wide by id),
    #    then (a) create a meta_dataframe and (b) create meta_xts for raw/net branches
    for (stat_col in union_stat_cols) {
      # 3a) Merge to wide (keys + one column per backtest id)
      merged_stat_df <- base_keys_df
      for (i in seq_along(stats_m_dfs)) {
        df_i  <- stats_m_dfs[[i]]
        bt_id <- port_backtest_results_list[[i]]@backtest_identifier

        if (stat_col %in% names(df_i)) {
          attach_i <- df_i[, c("id", stat_col), drop = FALSE]
          names(attach_i)[2] <- bt_id
          merged_stat_df <- dplyr::left_join(merged_stat_df, attach_i, by = "id")
        } else {
          # If a backtest lacks this stat, it simply won't contribute a column
          next
        }
      }


      # 3b) meta_xts (nested) for raw_return
      raw_xts <- build_stat_xts(merged_stat_df, "raw_return")
      if (!is.null(raw_xts)) {
        port_stats_m_xts_nested_list$raw_return[[paste0(stat_col, "_m_xts")]] <- create_meta_xts(
          data = raw_xts,
          type = "metrics",
          meta_xts_name = cohort_name,
          metric_name = stat_col,
          source = colnames(raw_xts)
        )
      }

      # 3c) meta_xts (nested) for net_return
      net_xts <- build_stat_xts(merged_stat_df, "net_return")
      if (!is.null(net_xts)) {
        port_stats_m_xts_nested_list$net_return[[paste0(stat_col, "_m_xts")]] <- create_meta_xts(
          data = net_xts,
          type = "metrics",
          meta_xts_name = cohort_name,
          metric_name = stat_col,
          source = colnames(net_xts)
        )
      }
    }
  }



  # Step 7: Create and Return the port_backtest_cohort Object
  cohort_obj <- methods::new("port_backtest_cohort",
    cohort_name = cohort_name,
    port_backtest_results_list = port_backtest_results_list,
    port_weights_m_df = merged_port_weights_m_df,
    port_costs_m_xts_list = port_costs_m_xts_list,
    port_returns_m_xts_list = port_returns_m_xts_list,
    port_metrics_m_xts_list = port_metrics_m_xts_list,
    port_stats_m_xts_nested_list = port_stats_m_xts_nested_list,
    backtest_workflow_common = common_values
  )

  return(cohort_obj)
}
