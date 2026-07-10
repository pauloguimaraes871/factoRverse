# compute_window ----------------------------------------------------
#' Compute Rolling or Seasonal Calculations for a Given Metric in meta_dataframe or meta_xts
#'
#' This method computes rolling or seasonal statistics for a specified metric in a `meta_dataframe` or `meta_xts` object.
#' The function applies a predefined calculation (`FUN`) to values within a specified time window.
#'
#' The window can be either:
#' - **Rolling:** Includes all dates within the range `[current_date - period months, current_date]`.
#' - **Fwd Seasonal:** Includes only observations from the same ticker whose months match the consecutive months immediately following the current observation's month.
#'
#' @param data A `meta_dataframe` or `meta_xts` object.
#' @param period A `numeric` value indicating the time window:
#' @param window A `character` specifying the window type: either "rolling" (default) or "seasonal".
#' @param FUN A `character` specifying the function to apply. Supported options depend on the input class:
#'   - For a `meta_dataframe`: "sum", "mean", "geom_mean_ret", "median", "sd", "skew", "sur", "cagr",
#'     "signal_to_noise", "res_mom", "idio_vol", "count_if", "max", "min", "lag".
#'   - For a `meta_xts`: "mean", "median", "geom_mean_ret", "max", "min", "sd", "skew", "sur", "cagr",
#'     "signal_to_noise", "alpha", "alpha_tstat", "beta", "correlation", "res_mom", "idio_vol".
#'   The benchmark-based FUNs ("res_mom", "idio_vol", "alpha", "alpha_tstat", "beta", "correlation") require
#'   `benchmark_returns_m_xts` and `selected_bench`.
#' @param signal (For `meta_dataframe`) A `character` specifying the column name on which the rolling function is computed.
#' @param col_name (For `meta_xts`) A `character` specifying the column name.
#' @param benchmark_returns_m_xts A `meta_xts` object. Required for `FUN` "res_mom" and "idio_vol".
#' @param selected_bench A `character` specifying the column name in `benchmark_returns_m_xts@data` to use. Required for `FUN` "res_mom" and "idio_vol".
#' @param na.rm A `logical` indicating whether to remove `NA` values (default: TRUE).
#' @param only_unique A `logical` indicating whether to compute the metric using only unique values (default: FALSE).
#' @param feature_name A `character` specifying the name of the computed feature. If NULL (default), the feature name is set to `"<metric>_<window>_<period>_<FUN>"`.
#' @param min_non_na A `numeric` value specifying the minimum number of non-NA values required to compute the rolling statistic. Default is 0.
#'   - Exception: For `FUN = "cagr"`, the default is `period + 1` to ensure sufficient periods.
#' @param count_condition_fun A function that takes a numeric vector and returns a logical vector.
#' The function should return `TRUE` for elements that should be counted. Only used for FUN = "count_if".
#' @param offset_months A `numeric` value specifying the number of months to offset ahead or backwards for `seasonal` window type.
#' @param specific_dates A `Date` vector specifying specific dates to consider for the rolling calculation.
#' @param mult_last_n A `numeric` value indicating the number of most recent observations to multiply by a factor when computing certain metrics (default: 0).
#' @param mult_by A `numeric` value indicating the multiplication factor for the last `n` observations when computing certain metrics (default: -1 for most metrics, 0 for "geom_mean_ret").
#' @param top_n A `numeric` value indicating the number of top elements to consider when computing the "max" function (default: 1).
#' @param ... Additional arguments passed to the function.
#'
#' @return A modified `meta_dataframe` or `meta_xts` object with an additional column named `"<metric>_<window>_<period>_<FUN>"`,
#' storing the computed values.
#'
#' @details
#' The function filters observations within the specified time window and applies the selected `FUN`. If no matching observations are found, the result is `NA`.
#'
#' Available functions:
#' \itemize{
#'   \item **sum**: `sum(x, na.rm = na.rm)` (meta_dataframe only).
#'   \item **mean**: `mean(x, na.rm = na.rm)`.
#'   \item **geom_mean_ret**: Geometric mean return over the window (percentage-point scale).
#'   \item **median**: `stats::median(x, na.rm = na.rm)`.
#'   \item **sd**: `stats::sd(x, na.rm = na.rm)`.
#'   \item **skew**: Bias-adjusted skewness of the values in the window.
#'   \item **sur**: `(final_value - mean(x, na.rm = na.rm)) / stats::sd(x, na.rm = na.rm)`, where `final_value` is the most recent value.
#'   \item **cagr**: Compound growth rate between the first and last value of the window.
#'   \item **signal_to_noise**: `mean(x) / sd(x)` (mean-to-standard-deviation ratio).
#'   \item **res_mom**: Rolling regression of the metric on the benchmark; returns the standardized sum of residuals (residual momentum).
#'   \item **idio_vol**: Rolling regression against the benchmark; returns `sqrt(sd(metric)^2 - beta^2 * sd(benchmark)^2)`.
#'   \item **alpha**, **alpha_tstat**, **beta**, **correlation**: Rolling CAPM-style statistics against the benchmark (meta_xts only).
#'   \item **count_if**: Counts the number of elements that satisfy `count_condition_fun` (meta_dataframe only).
#'   \item **max**: `max(x, na.rm = na.rm)` for a meta_dataframe; sum of the top `top_n` values for a meta_xts.
#'   \item **min**: `min(x, na.rm = na.rm)`.
#'   \item **lag**: Retrieves the observation that happened `period` months ago (meta_dataframe only).
#' }
#'
#' @export

setGeneric("compute_window", function(data, period, FUN, ...){
  standardGeneric("compute_window")
})

#' @rdname compute_window
setMethod("compute_window",
          signature(data = "meta_dataframe", period = "numeric", FUN = "character"),
          function(data, period, FUN, window = "rolling",
                   signal, benchmark_returns_m_xts = NULL, selected_bench = NULL, na.rm = TRUE,
                   only_unique = FALSE, feature_name = NULL, min_non_na = 0,
                   count_condition_fun = NULL, offset_months = 0) {

            #Extract data
            ############
            ###Meta dataframe
            meta_dataframe_workflow <- data@workflow
            meta_dataframe_name <- data@meta_dataframe_name
            current_date <- data@current_date
            pre_silver_features_m_df <- data@data

            ###Meta xts
            is_returns_meta_xts <- inherits(benchmark_returns_m_xts, "returns_meta_xts")
            benchmark_returns_m_xts_name <- if(is_returns_meta_xts) benchmark_returns_m_xts@meta_xts_name else NULL
            benchmark_returns_m_xts <- if(is_returns_meta_xts) benchmark_returns_m_xts@data else NULL
            ############

            #Initial Checks
            ############
            ##Check if the specified signal column exists in the data frame
            if (!signal %in% names(pre_silver_features_m_df)) {
              stop("The signal column does not exist in the data frame.")
            }
            ##Check if the signal column is numeric
            if (!is.numeric(pre_silver_features_m_df[[signal]])) {
              stop("The signal column must be numeric.")
            }
            ##Check if min_non_na is >= 0
            if (min_non_na < 0) {
              stop("The 'min_non_na' argument must be greater or equal to 0.")
            }
            ##Check if period is >= 0
            if (period < 0) {
              stop("The period must be greater or equal to 0.")
            }
            ##Check if offset_months is provided when window is "seasonal"
            if (window == "seasonal" && is.null(offset_months) || !is.numeric(offset_months)) {
              stop("The 'offset_months' argument must be provided when window is 'seasonal'.")
            }
            ##Check for only unique and FUN
            if (only_unique && FUN %in% c("cagr", "res_mom", "idio_vol", "lag", "geom_mean_ret")){
              stop("The 'only_unique' is not supported for FUN ", FUN)
            }
            ##Check for min_non_na and cagr or lag (If CAGR, only begin and end are needed)
            if (min_non_na > 0 && FUN %in% c("cagr", "lag")) {
              stop("The 'min_non_na' argument is not supported for FUN ", FUN)
            }
            ##Additional Checks for FUN types
            if (FUN %in% c("res_mom", "idio_vol", "lag")) {
              if (window != "rolling"){
                stop("The 'window' argument must be 'rolling' for FUN ", FUN)
              }
            }
            if (FUN %in% c("res_mom", "idio_vol")){
              if (!is_returns_meta_xts) {
                stop("benchmark_returns_m_xts must be provided for FUN ", FUN)
              }
              if (is.null(selected_bench)) {
                stop("The 'selected_bench' argument must be provided for FUN ", FUN)
              }
              ##Check that dates in benchmark_returns_m_xts match those in pre_silver_features_m_df
              if (!all(pre_silver_features_m_df$dates %in% zoo::index(benchmark_returns_m_xts))) {
                stop("The dates in 'benchmark_returns_m_xts' must match those in 'pre_silver_features_m_df'.")
              }
              ##Check that dates in benchmark_returns_m_xts are ascending
              if (!all(diff(zoo::index(benchmark_returns_m_xts)) > 0)) {
                stop("The dates in 'benchmark_returns_m_xts' must be in ascending order.")
              }
            }
            if (FUN != "count_if" && !is.null(count_condition_fun)) {
              stop("The 'count_condition_fun' argument is only supported for FUN 'count_if'.")
            }
            if (FUN == "count_if" && !is.function(count_condition_fun)){
              stop("The 'count_condition_fun' argument must be a function.")
            }


            ############

            #Compute Rolling Calculation
            ############
            rolling_values <- purrr::map_dbl(seq_len(nrow(pre_silver_features_m_df)), function(i) {

              ##Get current row values
                ###Extract from mdf
                current_row <- pre_silver_features_m_df[i, ]
                ticker_i <- current_row$tickers
                current_row_date <- current_row$dates

                ###Compute the lower bound date by subtracting 'period' months
                lower_date <- lubridate::add_with_rollback(current_row_date, -months(period))

                ###Depending on the window type, filter the data accordingly
                if (window == "rolling"){
                  ####Find rows with the same ticker and with dates in the range [lower_date, current_row_date]
                  selected_pre_silver_features_m_df <- pre_silver_features_m_df %>%
                    dplyr::filter(tickers == ticker_i) %>%
                    dplyr::filter(dates >= lower_date & dates <= current_row_date)

                } else if (window == "seasonal"){
                  ####Determine seasonal months: consecutive months following the current month (wrapping around if needed)
                  current_month <- lubridate::month(current_row_date)
                  offset_duration <- months(offset_months)
                  seasonal_months <- lubridate::month(lubridate::add_with_rollback(current_row_date, offset_duration, roll = TRUE))

                  ####Filter
                  selected_pre_silver_features_m_df <- pre_silver_features_m_df %>%
                    dplyr::filter(tickers == ticker_i) %>%
                    dplyr::filter(dates >= lower_date & dates <= current_row_date) %>% #This is crucial to avoid forward looking bias
                    dplyr::filter(lubridate::month(dates) %in% seasonal_months)

                } else {
                  stop("Invalid window type. Must be either 'rolling' or 'seasonal'.")
                }

                ###If no matching observation is found, return NA
                if (nrow(selected_pre_silver_features_m_df) == 0) return(NA_real_)

              ##Get the filtered values for the signal
              values <- selected_pre_silver_features_m_df %>% dplyr::pull(!!rlang::sym(signal))

                ###Get lagged
                lagged_value <- NA_real_
                if (FUN == "lag"){
                  ####Get lagged date
                  lagged_date <- lubridate::add_with_rollback(current_row_date, months(-period), roll = TRUE)
                  ####Check if lagged date is present and return NA if not
                  if (lagged_date %in% selected_pre_silver_features_m_df$dates){
                    lagged_value <- selected_pre_silver_features_m_df %>%
                      dplyr::filter(dates == lagged_date) %>%
                      dplyr::pull(!!rlang::sym(signal))
                  }
                }
                ###Get begin and final (useful for cagr and sur)
                  ####Begin
                  begin_date <- lubridate::add_with_rollback(current_row_date, months(-period), roll = TRUE)
                    ####Check if begin date is present and return NA if not
                    if (!(begin_date %in% selected_pre_silver_features_m_df$dates)){
                      begin_value <- NA_real_
                    } else {
                      begin_value <- selected_pre_silver_features_m_df %>%
                        dplyr::filter(dates == begin_date) %>%
                        dplyr::pull(!!rlang::sym(signal)) %>%
                        as.numeric()
                    }
                  ####Final
                  final_date <- current_row_date
                    ####Check if final date is present and return NA if not
                    if (!(final_date %in% selected_pre_silver_features_m_df$dates)){
                      final_value <- NA_real_
                    } else {
                      final_value <- selected_pre_silver_features_m_df %>%
                        dplyr::filter(dates == final_date) %>%
                        dplyr::pull(!!rlang::sym(signal)) %>%
                        as.numeric()
                    }

              ##Get only unique
              if (only_unique) {
                values <- unique(values) #Get unique values
              }
                ###Do the same for benchmark if it is not NULL
                if (!is.null(benchmark_returns_m_xts)){
                  ####Extract rolling window for benchmark from benchmark_returns_m_xts
                  selected_bench_ret_values <- as.numeric(benchmark_returns_m_xts[zoo::index(benchmark_returns_m_xts) %in% selected_pre_silver_features_m_df$dates, selected_bench])
                }

              ##Returns NA in case of certain conditions
                ###Return NA in case min_non_na is not filled
                if (!FUN %in% c("cagr") && sum(!is.na(values)) < min_non_na) return(NA_real_)

                ###Return NA in case values is not of class numeric
                if (!is.numeric(values)) {
                  return(NA_real_)
                }
                ###Return NA in case selected_bench_ret_values length is 0
                if (!is.null(benchmark_returns_m_xts) && length(selected_bench_ret_values) == 0) {
                  return(NA_real_)
                }


              ##Depending on FUN, compute the rolling metric
              switch(FUN,
                     "sum" = sum(values, na.rm = na.rm),
                     "mean" = mean(values, na.rm = na.rm),
                     "geom_mean_ret" = geometric_mean_return(values, na.rm = na.rm),
                     "median" = stats::median(values, na.rm = na.rm),
                     "sd" = stats::sd(values, na.rm = na.rm),
                     "skew" = skew(values, na.rm = na.rm),
                     "sur" = {
                       sur(final_value = final_value, past_values = values, na.rm = na.rm)
                     },
                     "cagr" = {
                       if (length(values) < 2) return(NA_real_) ##Guarantes NA for less than 2 inputs
                       #Dynamically adjust period for values length
                       cagr(begin = begin_value, final = final_value, period = period)
                     },
                     "signal_to_noise" = signal_to_noise(values, na.rm = na.rm, mult_last_n = 0),
                     "res_mom" = res_mom(ret_values = values, bench_ret_values = selected_bench_ret_values, na.rm = na.rm),
                     "idio_vol" = idio_vol(ret_values = values, bench_ret_values = selected_bench_ret_values, na.rm = na.rm),
                     "count_if" = count_if(values, count_condition_fun = count_condition_fun),
                     "max" = max(values, na.rm = na.rm),
                     "min" = min(values, na.rm = na.rm),
                     "lag" = {
                       if (length(values) < 2) return(NA_real_)
                       #Just provied begin value
                       return(lagged_value)
                     },
                     stop("Unsupported function type")
              )

            })

            ############
            # Create a new column name dynamically if feature_name is NULL
            ##Use short_window_name
            if (window == "rolling") short_window_name <- "roll"
            if (window == "seasonal") short_window_name <- "seas"
            if (is.null(feature_name)){
              new_col_name <- paste0(signal, "_", FUN, "_", short_window_name, "_", period, "m")
            } else {
              new_col_name <- feature_name
            }

            ## Add the computed rolling values to the meta_dataframe
            pre_silver_features_m_df <- pre_silver_features_m_df %>%
              dplyr::mutate(!!rlang::sym(new_col_name) := rolling_values)
            ############

            ############
            ##Finalize with the workflow
            new_workflow <- list(
              list(current_date = current_date,  # Current date
                   timestamp = Sys.time(),        # Timestamp
                   signal = signal,
                   period = period,
                   window = window,
                   feature_name = new_col_name,
                   FUN = FUN,
                   call = match.call(),
                   na.rm = na.rm,
                   only_unique = only_unique,
                   min_non_na = min_non_na,
                   benchmark_returns_m_xts_name = benchmark_returns_m_xts_name,
                   count_condition_fun = count_condition_fun,
                   selected_bench = if(!is.null(selected_bench)) selected_bench else NA_character_
              )
            )

            ##Recreate
            pre_silver_features_m_df <- create_meta_dataframe(pre_silver_features_m_df,
                                                              meta_dataframe_name = meta_dataframe_name,
                                                              workflow = c(meta_dataframe_workflow, new_workflow),
                                                              type = "generic")
            ##Rename
            names(pre_silver_features_m_df@workflow)[length(pre_silver_features_m_df@workflow)] <-
              paste0("compute_", signal, "_", FUN, "_", short_window_name, "_", period, "m", "_", current_date)
            ############

            return(pre_silver_features_m_df)
          })

#' @rdname compute_window
setMethod("compute_window",
          signature(data = "meta_xts", period = "numeric", FUN = "character"),
          function(data, period, FUN, window = "rolling",
                   col_name, benchmark_returns_m_xts = NULL, selected_bench = NULL, na.rm = TRUE,
                   only_unique = FALSE, feature_name = NULL, min_non_na = 0, specific_dates = NULL,
                   mult_last_n = 0, mult_by = if (FUN == "geom_mean_ret") 0 else -1, top_n = 1) {

            #Extract relevant elements
            ###############
            meta_xts_workflow <- data@workflow
            meta_xts_name <- data@meta_xts_name
            if (methods::is(data, "metrics_meta_xts")) {
              meta_xts_type <-  "metrics"
            } else if (methods::is(data, "returns_meta_xts")){
              meta_xts_type <- "returns"
            } else {
              stop("Unsupported meta_xts class. Only 'metrics_meta_xts' and 'returns_meta_xts' are supported.")
            }
            current_date <- data@current_date
            source <- data@source
            pre_silver_m_xts <- data@data

            ###Benchmark
            is_returns_meta_xts <- inherits(benchmark_returns_m_xts, "returns_meta_xts")
            benchmark_returns_m_xts_name <- if(is_returns_meta_xts) benchmark_returns_m_xts@meta_xts_name else NULL
            benchmark_returns_m_xts <- if(is_returns_meta_xts) benchmark_returns_m_xts@data else NULL

            ###############

            #Initial checks
            ###############
            if (!col_name %in% colnames(pre_silver_m_xts)) {
              stop("The col_name column does not exist in the xts object.")
            }
            if (!is.numeric(pre_silver_m_xts[, col_name])) {
              stop("The col_name column must be numeric.")
            }
            if (period < 0) {
              stop("The period must be greater or equal to 0.")
            }
            if (only_unique && FUN %in% c("cagr", "res_mom", "idio_vol", "geom_mean_ret",
                                          "alpha", "alpha_tstat", "beta", "correlation")){
              stop("The 'only_unique' is not supported for FUN ", FUN)
            }
            if (window != "rolling"){
              stop("The window argument must be rolling for meta_xts method")
            }
            if (!is.null(specific_dates) && !inherits(specific_dates, "Date")) {
              stop("The specific_dates argument must be of class 'Date'.")
            }
            ##Additional Checks for FUN types
            if (FUN %in% c("res_mom", "idio_vol", "lag", "correlation", "alpha", "alpha_tstat", "beta")) {
              if (window != "rolling"){
                stop("The 'window' argument must be 'rolling' for FUN ", FUN)
              }
            }

            benchmark_FUNs <- c("res_mom", "idio_vol", "alpha", "beta", "correlation", "alpha_tstat")
            if (FUN %in% benchmark_FUNs){
              if (!is_returns_meta_xts) {
                stop("benchmark_returns_m_xts must be provided for FUN ", FUN)
              }
              if (is.null(selected_bench)) {
                stop("The 'selected_bench' argument must be provided for FUN ", FUN)
              }
              ##Check that dates between benchmark and pre_silver_m_xts match
              if (!identical(zoo::index(pre_silver_m_xts), zoo::index(benchmark_returns_m_xts))) {
                stop("The dates in pre_silver_m_xts do not match the dates in benchmark_returns_m_xts.")
              }
            }
            if (meta_xts_type == "returns" && FUN %in% c("cagr")) {
              stop("The FUN ", FUN, " is not supported for returns_meta_xts. Use metrics_meta_xts instead.")
            }
            if (meta_xts_type == "metrics" && FUN %in% c("geom_mean_ret", "idio_vol", "res_mom", "alpha", "alpha_tstat", "beta")) {
              stop("The FUN ", FUN, " is not supported for metrics_meta_xts. Use returns_meta_xts instead.")
            }


            ###############

            #Compute rolling values
            ###############
              ##Init object
              rolling_values <- xts::xts(rep(NA_real_, nrow(pre_silver_m_xts)), order.by = zoo::index(pre_silver_m_xts))

              ##Get indices
              row_indices <- if (is.null(specific_dates)) {
                seq_len(nrow(pre_silver_m_xts))
              } else {
                which(zoo::index(pre_silver_m_xts) %in% specific_dates)
              }

              ##Loop through xts
              for (i in row_indices) {
                current_row_date <- zoo::index(pre_silver_m_xts)[i]
                lower_date <- lubridate::add_with_rollback(current_row_date, -months(period))

                ###Check if the current row date is in the specific dates
                if (!is.null(specific_dates) && !(current_row_date %in% specific_dates)) {
                  rolling_values[i] <- NA_real_
                  next
                }

                ###Rolling window
                selected_xts <- pre_silver_m_xts[zoo::index(pre_silver_m_xts) >= lower_date &
                                                 zoo::index(pre_silver_m_xts) <= current_row_date, col_name]


                ###If length is 0, skip
                if (length(selected_xts) == 0) return(NA_real_)

                ###Adjust values
                values <- as.numeric(selected_xts)

                ###Get begin and final (useful for cagr and sur)
                ####Begin
                begin_date <- lubridate::add_with_rollback(current_row_date, months(-period), roll = TRUE)
                  ####Check if begin date is present and return NA if not
                  if (!(begin_date %in% zoo::index(selected_xts))){
                    begin_value <- NA_real_
                  } else {
                    begin_value <- as.numeric(selected_xts[zoo::index(selected_xts) == begin_date])
                  }
                ####Final
                final_date <- current_row_date
                  ####Check if final date is present and return NA if not
                  if (!(final_date %in% zoo::index(selected_xts))){
                    final_value <- NA_real_
                  } else {
                    final_value <- as.numeric(selected_xts[zoo::index(selected_xts) == final_date])
                  }

                ###Get only unique
                if (only_unique) {
                  values <- unique(values)
                }
                ###Do the same for benchmark if it is not NULL
                if (!is.null(benchmark_returns_m_xts)){
                  ####Extract rolling window for benchmark from benchmark_returns_m_xts
                  selected_bench_ret_values <- as.numeric(benchmark_returns_m_xts[zoo::index(selected_xts), selected_bench])
                }

                ###Return NA in case min_non_na is not filled
                if (!FUN %in% c("cagr") && sum(!is.na(values)) < min_non_na) {
                  rolling_values[i] <- NA_real_
                  next
                }
                if (!is.numeric(values)) {
                  rolling_values[i] <- NA_real_
                  next
                }
                ###Return NA in case selected_bench_ret_values length is 0
                if (!is.null(benchmark_returns_m_xts) && length(selected_bench_ret_values) == 0) {
                  rolling_values[i] <- NA_real_
                  next
                }

                ###Apply FUN
                rolling_values[i] <- switch(FUN,
                                            "mean" = stats::mean(values, na.rm = na.rm),
                                            "median" = stats::median(values, na.rm = na.rm),
                                            "geom_mean_ret" = geometric_mean_return(values, na.rm = na.rm, mult_by = mult_by, mult_last_n = mult_last_n),
                                            "max" = sum_top_n(values, top_n = top_n, na.rm = na.rm),
                                            "min" = min(values, na.rm = na.rm),
                                            "sd" = stats::sd(values, na.rm = na.rm),
                                            "skew" = skew(values, na.rm = na.rm),
                                            "sur" = sur(final_value = final_value, past_values = values, na.rm = na.rm),
                                            "cagr" = cagr(begin = begin_value, final = final_value, period = period),
                                            "signal_to_noise" = signal_to_noise(values, na.rm = na.rm, mult_last_n = mult_last_n, mult_by = mult_by),
                                            "alpha" = alpha_bench(ret_values = values, bench_ret_values = selected_bench_ret_values, mult_last_n = mult_last_n, mult_by = mult_by, na.rm = na.rm),
                                            "alpha_tstat" = alpha_tstat_bench(ret_values = values, bench_ret_values = selected_bench_ret_values, mult_last_n = mult_last_n, mult_by = mult_by, na.rm = na.rm),
                                            "beta" = beta_bench(ret_values = values, bench_ret_values = selected_bench_ret_values, na.rm = na.rm),
                                            "correlation" = correlation_bench(ret_values = values, bench_ret_values = selected_bench_ret_values, na.rm = na.rm),
                                            "res_mom" = res_mom(ret_values = values, bench_ret_values = selected_bench_ret_values, na.rm = na.rm),
                                            "idio_vol" = idio_vol(ret_values = values, bench_ret_values = selected_bench_ret_values, na.rm = na.rm),
                                            stop("Unsupported function type"))
              }
            ###############

            #Assign computed values to a new column
            ###############
            short_window_name <- if (window == "rolling") "roll" else "seas"
            new_col_name <- if (is.null(feature_name)) paste0(col_name, "_", FUN, "_", short_window_name, "_", period, "m") else feature_name
            pre_silver_m_xts <- cbind(pre_silver_m_xts, rolling_values)
            colnames(pre_silver_m_xts)[ncol(pre_silver_m_xts)] <- new_col_name

            #Update workflow
            new_workflow <- list(
              list(current_date = current_date,
                   timestamp = Sys.time(),
                   col_name = col_name,
                   period = period,
                   window = window,
                   feature_name = new_col_name,
                   FUN = FUN,
                   call = match.call(),
                   na.rm = na.rm,
                   only_unique = only_unique,
                   min_non_na = min_non_na,
                   benchmark_returns_m_xts_name = benchmark_returns_m_xts_name
                   )
            )

            #Recreate the meta_xts object
            pre_silver_m_xts <- create_meta_xts(pre_silver_m_xts,
                                                meta_xts_name = meta_xts_name,
                                                workflow = c(meta_xts_workflow, new_workflow),
                                                type = meta_xts_type,
                                                source = c(source,
                                                           paste0("compute_", col_name, "_", FUN, "_", short_window_name, "_", period, "m", "_", current_date))
            )

            names(pre_silver_m_xts@workflow)[length(pre_silver_m_xts@workflow)] <-
              paste0("compute_", col_name, "_", FUN, "_", short_window_name, "_", period, "m", "_", current_date)


            return(pre_silver_m_xts)

            ###############
          })



#FUNs------------------------------------------------------

## Skewness
#' Calculate Skewness of a Numeric Vector
#'
#' This function computes the skewness (the third standardized moment) of a numeric vector.
#' It handles missing values according to the \code{na.rm} parameter and returns \code{NA} if the standard deviation is zero.
#'
#' @param values A numeric vector whose skewness is to be calculated.
#' @param na.rm A logical value indicating whether NA values should be removed before computation (default is TRUE).
#'
#' @return A numeric value representing the skewness of the input vector. If the standard deviation is zero, \code{NA_real_} is returned.
#'
#' @export
#'
#' @examples
#' skew(c(1, 2, 3, 4, 5))
skew <- function(values, na.rm = TRUE) {

    # Remove NAs if requested and count the non-missing values
    values <- values[if (na.rm) !is.na(values) else TRUE]
    n <- length(values)

    # Calculate the mean and standard deviation using base R functions
    m_val <- mean(values)
    s_val <- stats::sd(values)

    # If standard deviation is 0 or NA, return NA
    if (is.na(s_val) || s_val == 0) return(NA_real_)

    # Calculate the unadjusted skewness
    g1 <- mean((values - m_val)^3) / s_val^3

    # Adjust the skewness for small sample bias
    adjusted_skew <- sqrt(n * (n - 1)) / (n - 2) * g1

    return(adjusted_skew)
}


#' @title Calculate Standardized Unexpected Realization (SUR)
#'
#' @description
#' Computes the Standardized Unexpected Realization (SUR), defined as the standardized deviation of a final observation from past values.
#'
#' @param final_value A numeric value representing the latest observation.
#' @param past_values A numeric vector of historical observations.
#' @param na.rm Logical. Should missing values be removed before computation? Default is \code{TRUE}.
#'
#' @return A numeric SUR value. Returns \code{NA_real_} if the standard deviation is zero or undefined.
#'
#' @export
sur <- function(final_value, past_values, na.rm = TRUE) {

  #Calculate the SUR
  sd_val <- stats::sd(past_values, na.rm = na.rm)
  if (is.na(sd_val) || sd_val == 0) return(NA_real_)
  (final_value - mean(past_values, na.rm = na.rm)) / sd_val

}


#' Geometric Mean Return (percentage-point scale)
#'
#' Computes the geometric mean of a vector of returns expressed **in percentage
#' points** (e.g. `2` for 2 %, `-0.75` for -0.75 %).
#' Internally the function converts the input to decimal form, performs the
#' compounding, and returns the result **in the same percentage-point scale**.
#'
#' @param returns Numeric vector of returns. Each return must be greater than
#'   `-100` (i.e. no return worse than -100 %).
#' @param mult_last_n = Integer >= 0. If > 0, multiply the last n (in time order) by a number.
#' @param mult_by Numeric scalar. The number to multiply the last n values by. Default is 0 (skip).
#' @param na.rm Logical. Should `NA` values be removed?  Default `FALSE`.
#' @param scale Numeric divisor/multiplier used to convert between percentage
#'   points and decimals.  Default `100`.
#'
#' @return A single numeric value (in percentage points) representing the
#'   compounded average return.
#'
#' @details
#' The computation is
#' \deqn{\left(\prod_{i=1}^{n}\left(1 + \frac{r_i}{\text{scale}}\right)\right)^{1/n} - 1}
#' converted back to percentage-point scale by multiplying the decimal result by
#' `scale`.
#'
#' @examples
#' # 2 % , 0.5 % and -1 % expressed as 2, 0.5, -1
#' geometric_mean_return(c(2, 0.5, -1))        # ≈ 0.498 (% points)
#' geometric_mean_return(c(2, NA, 1), na.rm = TRUE)
#'
#' @export
geometric_mean_return <- function(returns, na.rm = FALSE, mult_last_n = 0, mult_by = 0, scale = 100) {

  # NA handling
  if (na.rm) returns <- returns[!is.na(returns)]

  # If no observations, return NA
  if (length(returns) == 0) return(NA_real_)

  # Multiply last n values
  if (!is.null(mult_last_n) && mult_last_n > 0L) {
    n <- length(returns)
    k <- min(mult_last_n, n)
    idx <- (n - k + 1L):n
    returns[idx] <- mult_by * returns[idx]
  }

  if (any(returns <= -scale)) {
    stop("All returns must be greater than -", scale, " (i.e. > -100%).")
  }

  ## convert to decimals
  r_dec <- returns / scale

  ## geometric mean in decimals
  gm_dec <- prod(1 + r_dec)^(1 / length(r_dec)) - 1

  ## return to caller in percentage-point scale
  gm_dec * scale
}




## CAGR
#' Calculate Compound Annual Growth Rate (CAGR)
#'
#' This function computes the Compound Annual Growth Rate (CAGR) given an initial value, a final value,
#' and the number of periods. It handles various scenarios, including cases with negative values or missing data.
#'
#' @param begin A numeric value representing the initial value.
#' @param final A numeric value representing the final value.
#' @param period A numeric value representing the number of periods over which the growth is calculated.
#'
#' @return A numeric value representing the CAGR. If either \code{begin} or \code{final} is \code{NA},
#' or if the period is less than or equal to zero, appropriate errors or \code{NA} are returned.
#'
#' @export
#'
#' @examples
#' cagr(100, 200, 5)
cagr <- function(begin, final, period) {
  if (period <= 0) {
    stop("Period must be greater than zero.")
  }
  if (is.na(begin) || is.na(final)) {
    calculated_cagr <- NA_real_
  } else {
    if (!is.numeric(begin) || !is.numeric(final)) {
      stop("Inputs are not numeric")
    }
    # Calculate CAGR based on the sign of the inputs
    if (final >= 0 && begin >= 0) {
      calculated_cagr <- (final / begin)^(1 / period) - 1
    }
    if (final <= 0 && begin <= 0) {
      calculated_cagr <- -1 * ((abs(final) / abs(begin))^(1 / period) - 1)
    }
    if (final >= 0 && begin <= 0) {
      calculated_cagr <- ( (final + 2 * abs(begin)) / abs(begin) )^(1 / period) - 1
    }
    if (final <= 0 && begin >= 0) {
      calculated_cagr <- -1 * (((abs(final) + 2 * begin) / begin)^(1 / period) - 1)
    }
  }
  return(calculated_cagr)
}



## Mean-to-Std Ratio (Signal-to-Noise Ratio)
#' Calculate Mean-to-Standard Deviation Ratio (Signal-to-Noise Ratio)
#'
#' This function computes the mean-to-standard deviation ratio of values in a numeric vector.
#' This ratio is also known as the inverse of the coefficient of variation or, in finance, as the Sharpe Ratio when the risk-free rate is zero.
#' Optional sign inversion on the last n observations
#'
#'
#' @param values Numeric vector.
#' @param na.rm Logical.
#' @param mult_last_n Integer >= 0. If > 0, multiply the last n (in time order) by a number.
#' @param mult_by Numeric scalar. The number to multiply the last n values by. Default is -1 (inversion).
#'
#' @return Numeric scalar.
signal_to_noise <- function(values, na.rm = TRUE, mult_last_n = 0L, mult_by = -1) {

  if (!is.numeric(values)) stop("values must be numeric.")
  if (is.null(mult_last_n)) mult_last_n <- 0L
  mult_last_n <- as.integer(mult_last_n)
  if (mult_last_n < 0L) stop("mult_last_n must be >= 0.")

  if (na.rm) values <- values[!is.na(values)]
  if (length(values) == 0L) return(NA_real_)

  if (mult_last_n > 0L) {
    n <- length(values)
    k <- min(mult_last_n, length(values))
    idx <- (n - k + 1L):n
    values[idx] <- mult_by * values[idx]
  }

  sd_val <- stats::sd(values, na.rm = FALSE)
  if (is.na(sd_val) || sd_val == 0) return(NA_real_)
  mean(values, na.rm = FALSE) / sd_val
}

#' Sum of the top n maximum values
#'
#' @param values Numeric vector.
#' @param top_n Integer >= 1.
#' @param na.rm Logical.
#'
#' @return Numeric scalar.
sum_top_n <- function(values, top_n = 1L, na.rm = TRUE) {
  if (!is.numeric(values)) stop("values must be numeric.")
  top_n <- as.integer(top_n)
  if (top_n < 1L) stop("top_n must be >= 1.")

  if (na.rm) values <- values[!is.na(values)]
  if (length(values) < top_n) return(NA_real_)

  sum(sort(values, decreasing = TRUE)[seq_len(top_n)])
}


##Regression Helper
#' Validator function for returns and benchmark returns
#' @param ret_values Numeric vector (signal returns).
#' @param bench_ret_values Numeric vector (benchmark returns).
#' @return NA_real_ if ret_values are all NAs or contain Inf/-Inf, else throws error if bench_ret_values contain NA/Inf or lengths differ.
validate_returns_bench <- function(ret_values, bench_ret_values){

  ##If ret_values are all NAs, return NA
  if (all(is.na(ret_values))) return(NA_real_)
  ##If ret_values contain Inf or -Inf, return NA
  if (any(is.infinite(ret_values))) return(NA_real_)
  ##If any NA or Inf in bench_ret_values, return error
  if (any(is.na(bench_ret_values))) stop("NA values in benchmark returns")
  if (any(is.infinite(bench_ret_values))) stop("Infinite values in benchmark returns")
  ##If lenghts differ, throw error
  if (length(ret_values) != length(bench_ret_values)) stop("Lengths of returns and benchmark returns differ")

  TRUE

}




#' Internal helper: fit regression of signal returns on benchmark returns
#'
#' @param ret_values Numeric vector (signal returns).
#' @param bench_ret_values Numeric vector (benchmark returns).
#' @param na.rm Logical. If TRUE, drop NA positions from ret_values (and aligned bench).
#' @param include_intercept Logical. If TRUE, fit y ~ 1 + x, else fit y ~ x (no intercept).
#' @param mult_last_n Integer >= 0. If > 0, multiply the last n (in time order) of ret_values by -1 before fitting the regression. Default is 0.
#' @param mult_by Numeric scalar. The number to multiply the last n values by. Default is -1 (inversion).
#'
#' @return A list with elements: alpha, beta, residuals, residual_sd, n_obs.
bench_regression_fit <- function(
    ret_values,
    bench_ret_values,
    mult_last_n = 0,
    mult_by = -1,
    na.rm = TRUE,
    include_intercept = TRUE
) {

  valid <- validate_returns_bench(ret_values, bench_ret_values)
  if (is.na(valid)) return(NA_real_)

  # apply mult_last_n to ret_values ONLY
  if (!is.null(mult_last_n) && mult_last_n > 0L) {
    n <- length(ret_values)
    k <- min(mult_last_n, n)
    idx <- (n - k + 1L):n
    ret_values[idx] <- mult_by*ret_values[idx]
  }


  # aligned NA removal (only from ret_values, but apply to both)
  if (na.rm && any(is.na(ret_values))) {
    idx_keep <- !is.na(ret_values)
    ret_values <- ret_values[idx_keep]
    bench_ret_values <- bench_ret_values[idx_keep]
  }

  n_obs <- length(ret_values)
  if (n_obs < 2L) {
    return(list(alpha = NA_real_, beta = NA_real_, residuals = numeric(0), residual_sd = NA_real_, n_obs = n_obs))
  }

  # Build design matrix and fit with lm.fit (fast + stable)
  if (isTRUE(include_intercept)) {
    X <- cbind(`(Intercept)` = 1, bench = bench_ret_values)
  } else {
    X <- cbind(bench = bench_ret_values)
  }
  colnames(X) <- NULL

  fit <- stats::lm.fit(x = X, y = ret_values)

  # Remove coefficient names caused by column names in the design matrix
  if (!is.null(fit$coefficients)) {
    names(fit$coefficients) <- NULL
  }

  # rank deficiency -> return NA metrics
  if (is.null(fit$coefficients) || any(is.na(fit$coefficients))) {
    return(list(alpha = NA_real_, beta = NA_real_, residuals = numeric(0), residual_sd = NA_real_, n_obs = n_obs))
  }

  if (isTRUE(include_intercept)) {
    alpha <- fit$coefficients[1]
    beta  <- fit$coefficients[2]
  } else {
    alpha <- 0
    beta  <- fit$coefficients[1]
  }

  residuals <- fit$residuals
  residual_sd <- stats::sd(residuals, na.rm = TRUE)

  list(
    alpha = alpha,
    beta = beta,
    residuals = residuals,
    residual_sd = residual_sd,
    n_obs = n_obs
  )
}


#' Calculate Alpha Relative to a Benchmark
#'
#' This function computes the \emph{alpha} of a return series relative to a benchmark,
#' defined as the intercept of a linear regression of the form:
#'
#' \deqn{
#' r_t = \alpha + \beta b_t + \varepsilon_t
#' }
#'
#' where \eqn{r_t} denotes the asset returns and \eqn{b_t} denotes the benchmark returns.
#' The estimated intercept \eqn{\alpha} measures the average excess return of the asset
#' that is not explained by exposure to the benchmark.
#'
#' The regression is estimated using ordinary least squares (OLS). Missing values in
#' \code{ret_values} may be removed depending on \code{na.rm}, while missing or infinite
#' values in \code{bench_ret_values} are not allowed and result in an error.
#'
#' @param ret_values A numeric vector of asset returns.
#' @param bench_ret_values A numeric vector of benchmark returns aligned in time with
#'   \code{ret_values}.
#' @param mult_last_n Integer >= 0. If > 0, multiply the last n (in time order) of
#'   \code{ret_values} by 'mult_by' before fitting the regression. Default is 0.
#' @param mult_by Numeric scalar. The number to multiply the last n values by. Default is -1 (inversion).
#' @param na.rm Logical. If \code{TRUE}, removes observations where \code{ret_values} is
#'   \code{NA} and drops the corresponding benchmark observations. Default is \code{TRUE}.
#'
#' @return A numeric scalar representing the estimated regression intercept (alpha).
#' Returns \code{NA_real_} if the regression cannot be estimated (e.g., insufficient
#' observations, degenerate design matrix).
#'
#' @details
#' \itemize{
#'   \item If all elements of \code{ret_values} are \code{NA}, \code{NA_real_} is returned.
#'   \item If any element of \code{bench_ret_values} is \code{NA} or infinite, an error is thrown.
#'   \item If fewer than two valid observations are available after NA handling,
#'         \code{NA_real_} is returned.
#' }
#'
#' @seealso
#' \code{\link{beta_bench}}, \code{\link{res_mom}}, \code{\link{idio_vol}}
#'
#' @export
alpha_bench <- function(ret_values, bench_ret_values, mult_last_n = 0,
                        mult_by = -1, na.rm = TRUE) {

  out <- bench_regression_fit(ret_values, bench_ret_values, na.rm = na.rm,
                              mult_last_n = mult_last_n, mult_by = -1,
                              include_intercept = TRUE)
  out$alpha
}

#' Compute the t-Statistic of Alpha Relative to a Benchmark
#'
#' This function calculates the t-statistic associated with the intercept
#' (\eqn{\alpha}) in a linear regression of asset returns on benchmark returns.
#' The model estimated is:
#'
#' \deqn{
#' r_t = \alpha + \beta b_t + \varepsilon_t,
#' }
#'
#' where \eqn{r_t} denotes the asset returns and \eqn{b_t} denotes the benchmark
#' returns over a given rolling window. The regression is estimated using the
#' internal helper \code{bench_regression_fit()}, which is a lightweight wrapper
#' around \code{lm.fit()} designed for high-volume, rolling, and point-in-time
#' computations.
#'
#' The t-statistic is computed as:
#'
#' \deqn{
#' t_{\alpha} = \frac{\alpha}{SE(\alpha)},
#' }
#'
#' where the standard error of the intercept is given by the OLS formula:
#'
#' \deqn{
#' SE(\alpha) =
#' \sqrt{
#'   \sigma^2
#'   \left(
#'     \frac{1}{n}
#'     +
#'     \frac{\bar{b}^2}{\sum (b_t - \bar{b})^2}
#'   \right)
#' }.
#' }
#'
#' @param ret_values Numeric vector of asset returns.
#' @param bench_ret_values Numeric vector of benchmark returns aligned with
#'   \code{ret_values}.
#' @param mult_last_n Integer >= 0. If > 0, multiply the last n (in time order) of
#'   \code{ret_values} by 'mult_by' before fitting the regression. Default is 0.
#' @param mult_by Numeric scalar. The number to multiply the last n values by. Default is -1 (inversion).
#' @param na.rm Logical. If \code{TRUE}, removes rows with NA in
#'   \code{ret_values} and applies the same mask to the benchmark. Default is
#'   \code{TRUE}. Missing or infinite values in the benchmark are not allowed
#'   and produce an error.
#'
#' @return A numeric scalar equal to the t-statistic of the regression intercept.
#' Returns \code{NA_real_} if:
#' \itemize{
#'   \item the regression is not estimable (e.g., insufficient observations),
#'   \item the benchmark has zero variance,
#'   \item the standard error of alpha is zero or undefined,
#'   \item the rolling window contains fewer than three usable observations.
#' }
#'
#' @details
#' This function is designed for use inside rolling computations and has no side
#' effects: all output is numeric and unnamed. It is robust to missing values in
#' the return series and implements strict point-in-time validation. The OLS
#' formulas are computed manually for performance reasons and to ensure that the
#' function behaves predictably under minimal rolling sample sizes.
#'
#' @seealso
#' \code{\link{bench_regression_fit}},
#' \code{\link{alpha_bench}},
#' \code{\link{beta_bench}},
#' \code{\link{correlation_bench}}
#'
#' @export
alpha_tstat_bench <- function(ret_values, bench_ret_values,
                              mult_last_n = 0, mult_by = -1, na.rm = TRUE) {

  out <- bench_regression_fit(
    ret_values = ret_values,
    bench_ret_values = bench_ret_values,
    mult_last_n = mult_last_n,
    mult_by = mult_by,
    na.rm = na.rm,
    include_intercept = TRUE
  )

  # Handle impossible cases
  if (is.na(out$alpha) || out$n_obs < 3L) return(NA_real_)
  if (is.na(out$residual_sd)) return(NA_real_)

  n <- out$n_obs
  b <- bench_ret_values
  p <- 2 #Intercept + beta

  # If NA removal occurred, ret_values is shortened
  if (na.rm) {
    idx_keep <- !is.na(ret_values)
    b <- bench_ret_values[idx_keep]
  } else {
    b <- bench_ret_values
  }

  # Compute Sxx centered
  mean_b <- mean(b)
  Sxx <- sum((b - mean_b)^2)
  if (Sxx <= 0) return(NA_real_)   # no variation -> undefined

  SSR <- sum(out$residuals^2)
  sigma2 <- SSR/(n-p)

  se_alpha <- sqrt( sigma2 * (1/n + mean_b^2 / Sxx) )
  if (se_alpha == 0) return(NA_real_)

  out$alpha / se_alpha
}



#' Calculate Beta Relative to a Benchmark
#'
#' This function computes the \emph{beta} of a return series relative to a benchmark,
#' defined as the slope coefficient in the linear regression:
#'
#' \deqn{
#' r_t = \alpha + \beta b_t + \varepsilon_t
#' }
#'
#' The estimated \eqn{\beta} measures the systematic exposure of the asset returns
#' to movements in the benchmark.
#'
#' The regression is estimated via ordinary least squares (OLS). Missing values in
#' \code{ret_values} may be removed depending on \code{na.rm}, while missing or infinite
#' values in \code{bench_ret_values} are not permitted.
#'
#' @param ret_values A numeric vector of asset returns.
#' @param bench_ret_values A numeric vector of benchmark returns aligned in time with
#'   \code{ret_values}.
#' @param na.rm Logical. If \code{TRUE}, removes observations where \code{ret_values} is
#'   \code{NA} and drops the corresponding benchmark observations. Default is \code{TRUE}.
#'
#' @return A numeric scalar representing the estimated regression slope (beta).
#' Returns \code{NA_real_} if the regression cannot be estimated.
#'
#' @details
#' \itemize{
#'   \item A beta close to zero indicates low systematic exposure to the benchmark.
#'   \item A beta greater than one indicates amplified exposure to benchmark movements.
#'   \item If the benchmark variance is zero or the regression is rank-deficient,
#'         \code{NA_real_} is returned.
#' }
#'
#' @seealso
#' \code{\link{alpha_bench}}, \code{\link{idio_vol}}
#'
#' @export
beta_bench <- function(ret_values, bench_ret_values, na.rm = TRUE) {
  out <- bench_regression_fit(ret_values, bench_ret_values, na.rm = na.rm, include_intercept = TRUE)
  out$beta
}

#' Calculate Correlation with a Benchmark
#'
#' This function computes the Pearson correlation coefficient between a return series
#' and a benchmark return series over a given window.
#'
#' Unlike regression-based measures such as alpha or beta, correlation is a symmetric
#' statistic that captures the strength and direction of linear co-movement between
#' the two series, without attributing causality or directional dependence.
#'
#' @param ret_values A numeric vector of asset returns.
#' @param bench_ret_values A numeric vector of benchmark returns aligned in time with
#'   \code{ret_values}.
#' @param na.rm Logical. If \code{TRUE}, removes observations where \code{ret_values} is
#'   \code{NA} and drops the corresponding benchmark observations. Default is \code{TRUE}.
#'
#' @return A numeric scalar in the interval \eqn{[-1, 1]} representing the Pearson
#' correlation coefficient. Returns \code{NA_real_} if the correlation cannot be computed.
#'
#' @details
#' \itemize{
#'   \item A value of \code{1} indicates perfect positive linear correlation.
#'   \item A value of \code{-1} indicates perfect negative linear correlation.
#'   \item A value of \code{0} indicates no linear correlation.
#'   \item Missing or infinite values in \code{bench_ret_values} result in an error.
#' }
#'
#' @seealso
#' \code{\link{alpha_bench}}, \code{\link{beta_bench}}
#'
#' @export
correlation_bench <- function(ret_values, bench_ret_values, na.rm = TRUE) {
  valid <- validate_returns_bench(ret_values, bench_ret_values)
  if (is.na(valid)) return(NA_real_)

if (na.rm && any(is.na(ret_values))) {
  idx_keep <- !is.na(ret_values)
  ret_values <- ret_values[idx_keep]
  bench_ret_values <- bench_ret_values[idx_keep]
}

if (length(ret_values) < 2L) return(NA_real_)
stats::cor(ret_values, bench_ret_values, use = "everything", method = "pearson")
}

## Residual Momentum
#' Calculate Residual Momentum Score
#'
#' This function computes the residual momentum score by fitting a linear regression model with
#' \code{ret_values} as the dependent variable and \code{bench_ret_values} as the independent variable.
#' The score is defined as the sum of the regression residuals divided by their standard deviation.
#'
#' @param ret_values A numeric vector of returns for the stock.
#' @param bench_ret_values A numeric vector of benchmark returns corresponding to the same periods as \code{ret_values}.
#' @param na.rm A logical value indicating whether to remove NA values before computation (default is TRUE).
#'
#' @return A numeric value representing the residual momentum score. If the standard deviation of residuals is zero,
#' \code{NA_real_} is returned.
#'
#' @export
#'
#' @examples
#' res_mom(c(0.05, 0.06, 0.07), c(0.03, 0.04, 0.05))
res_mom <- function(ret_values, bench_ret_values, na.rm = TRUE) {

  #Initial checks
  ##########
  valid <- validate_returns_bench(ret_values, bench_ret_values)
  if (is.na(valid)) return(NA_real_)
  ##########

  #Treat NAs and all equal
  ###########
    ##If na.rm TRUE, remove NAs from ret_values and remove same indices from bench_ret_values
    if (na.rm && any(is.na(ret_values))) {
      idx_keep <- !is.na(ret_values)
      ret_values <- ret_values[idx_keep]
      bench_ret_values <- bench_ret_values[idx_keep]
    }
    ##If ret_values = bench_values, return 0
    if (length(ret_values) > 0L && all(ret_values == bench_ret_values)) return(NA_real_)
  ###########

  # Fit a linear model: returns ~ benchmark returns
  ###########
  out <- bench_regression_fit(ret_values, bench_ret_values, na.rm = FALSE, include_intercept = FALSE)
  if (length(out$residuals) == 0L) return(NA_real_)
  if (is.na(out$residual_sd) || out$residual_sd < 1e-10) return(NA_real_)

  sum(out$residuals, na.rm = TRUE) / out$residual_sd

  ###########
}


## Idiosyncratic Volatility
#' Calculate Idiosyncratic Volatility
#'
#' This function computes the idiosyncratic volatility of a stock by fitting a linear regression model with
#' \code{ret_values} as the dependent variable and \code{bench_ret_values} as the independent variable.
#' Idiosyncratic volatility is defined as the square root of the difference between the variance of the stock returns
#' and the portion explained by the benchmark (i.e., \code{beta^2} times the variance of the benchmark returns).
#'
#' @param ret_values A numeric vector of stock returns.
#' @param bench_ret_values A numeric vector of benchmark returns.
#' @param na.rm A logical value indicating whether to remove NA values before computation (default is TRUE).
#'
#' @return A numeric value representing the idiosyncratic volatility. If the computed variance is negative,
#' \code{NA_real_} is returned.
#'
#' @export
#'
#' @examples
#' idio_vol(c(0.05, 0.06, 0.07), c(0.03, 0.04, 0.05))
idio_vol <- function(ret_values, bench_ret_values, na.rm = TRUE) {

  #Initial checks
  ##########
  valid <- validate_returns_bench(ret_values, bench_ret_values)
  if (is.na(valid)) return(NA_real_)
  ###########

  #Treat NAs and all equal
  ###########
    ##If na.rm TRUE, remove NAs from ret_values and remove same indices from bench_ret_values
    if (na.rm && any(is.na(ret_values))) {
      idx_keep <- !is.na(ret_values)
      ret_values <- ret_values[idx_keep]
      bench_ret_values <- bench_ret_values[idx_keep]
    }
    ##If ret_values = bench_values, return 0
    if (length(ret_values) > 0L && all(ret_values == bench_ret_values)) return(0)

  ###########


  # Fit a linear model: returns ~ benchmark returns
  ###########
  out <- bench_regression_fit(ret_values, bench_ret_values, na.rm = FALSE, include_intercept = TRUE)
  if (is.na(out$beta)) return(NA_real_)

  sd_signal <- stats::sd(ret_values, na.rm = TRUE) #Get standard deviation of stock returns
  sd_bench  <- stats::sd(bench_ret_values, na.rm = TRUE) #Get standard deviation of benchmark returns
  idio_var <- sd_signal^2 - (out$beta^2) * (sd_bench^2) #Compute idiosyncratic variance

  if (is.na(idio_var) || idio_var < 0) return(NA_real_)
  sqrt(idio_var)
  ###########

}

#' Count elements that satisfy a condition
#'
#' The `count_if` function counts the number of elements in `values`
#' that satisfy the condition specified in `count_condition_fun`.
#'
#' @param values A numeric vector of values to evaluate.
#' @param count_condition_fun A function that takes a numeric vector and returns a logical vector.
#' The function should return `TRUE` for elements that should be counted.
#' @param na.rm Logical. If `TRUE`, ignores `NA` values when counting. Default is `TRUE`.
#'
#' @return An integer representing the count of values satisfying the condition.
#' @export
count_if <- function(values, count_condition_fun, na.rm = TRUE) {

  #Initial checks
  if (!is.numeric(values)) stop("values must be numeric.")
  if (!is.function(count_condition_fun)) stop("count_condition_fun must be a function.")

  #Get vector that satisfies condition
  logical_vector <- count_condition_fun(values)

  if (!is.logical(logical_vector)) {
    stop("count_condition_fun must return a logical vector.")
  }

  #Count
  if (na.rm) {
    logical_vector <- logical_vector & !is.na(logical_vector)
  } else if (any(is.na(logical_vector))) {
    return(NA_integer_) # Return NA if NA values are present and na.rm = FALSE
  }

  sum(logical_vector)
}

