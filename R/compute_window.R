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
#' @param FUN A `character` specifying the function to apply. Supported options:
#'   - "median", "sd", "cagr", "skew", "sur", "mean_std", "res_mom", "idio_vol".
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
#'   \item **sum**: `sum(x, na.rm = na.rm)`
#'   \item **median**: `stats::median(x, na.rm = na.rm)`
#'   \item **sd**: `stats::sd(x, na.rm = na.rm)`
#'   \item **skew**: `mean((x - mean(x))^3, na.rm = na.rm) / stats::sd(x, na.rm = na.rm)^3`
#'   \item **sur**: `(final_value - mean(x, na.rm = na.rm)) / stats::sd(x, na.rm = na.rm)`, where `final_value` is the most recent metric value.
#'   \item **cv**: `stats::mean(unique(x), na.rm = na.rm) / stats::sd(unique(x), na.rm = na.rm)`
#'   \item **res_mom**: Performs rolling regressions between the metric and the benchmark (from `benchmark_returns_m_xts`).
#'         Computes `alpha` and `beta`, then returns `(alpha + beta * current_benchmark) - current_metric`.
#'   \item **idio_vol**: Computes rolling volatility by regressing the metric against the benchmark, obtaining beta, then computing `sqrt(sd(metric)^2 - beta^2 * sd(benchmark)^2)`.
#'   \item **count_if**: Counts the number of elements that satisfy a condition. The condition is specified by the `count_condition_fun` argument.
#'   \item **max**: `max(x, na.rm = na.rm)`
#'   \item **min**: `min(x, na.rm = na.rm)`
#'   \item **lag**: Retrieves the observation that happened period months ago.
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
                     "mean_std" = mean_std(values, na.rm = na.rm),
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
                   only_unique = FALSE, feature_name = NULL, min_non_na = 0, specific_dates = NULL) {

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
            if (only_unique && FUN %in% c("cagr", "res_mom", "idio_vol", "geom_mean_ret")){
              stop("The 'only_unique' is not supported for FUN ", FUN)
            }
            if (window != "rolling"){
              stop("The window argument must be rolling for meta_xts method")
            }
            if (!is.null(specific_dates) && !inherits(specific_dates, "Date")) {
              stop("The specific_dates argument must be of class 'Date'.")
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
              ##Check that dates between benchmark and pre_silver_m_xts match
              if (!identical(zoo::index(pre_silver_m_xts), zoo::index(benchmark_returns_m_xts))) {
                stop("The dates in pre_silver_m_xts do not match the dates in benchmark_returns_m_xts.")
              }
            }
            if (meta_xts_type == "returns" && FUN %in% c("cagr")) {
              stop("The FUN ", FUN, " is not supported for returns_meta_xts. Use metrics_meta_xts instead.")
            }
            if (meta_xts_type == "metrics" && FUN %in% c("geom_mean_ret", "idio_vol", "res_mom")) {
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
                                            "geom_mean_ret" = geometric_mean_return(values, na.rm = na.rm),
                                            "max" = max(values, na.rm = na.rm),
                                            "min" = min(values, na.rm = na.rm),
                                            "sd" = stats::sd(values, na.rm = na.rm),
                                            "skew" = skew(values, na.rm = na.rm),
                                            "sur" = sur(final_value = final_value, past_values = values, na.rm = na.rm),
                                            "cagr" = cagr(begin = begin_value, final = final_value, period = period),
                                            "mean_std" = mean_std(values, na.rm = na.rm),
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
geometric_mean_return <- function(returns, na.rm = FALSE, scale = 100) {

  if (na.rm) returns <- returns[!is.na(returns)]

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
#'
#' @param values A numeric vector.
#' @param na.rm A logical value indicating whether to remove NA values before computation (default is TRUE).
#'
#' @return A numeric value representing the mean-to-std ratio. If the standard deviation is zero,
#' \code{NA_real_} is returned.
#'
#' @export
#'
#' @examples
#' mean_std(c(1, 2, 3, 4, 5))
mean_std <- function(values, na.rm = TRUE) {
  sd_val <- stats::sd(values, na.rm = na.rm)
  if (is.na(sd_val) || sd_val == 0) return(NA_real_)
  mean(values, na.rm = na.rm) / sd_val
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
    ##If ret_values are all NAs, return NA
    if (all(is.na(ret_values))) return(NA_real_)
    ##If ret_values contain Inf or -Inf, return NA
    if (any(is.infinite(ret_values))) return(NA_real_)
    ##If any NA or Inf in bench_ret_values, return error
    if (any(is.na(bench_ret_values))) stop("NA values in benchmark returns")
    if (any(is.infinite(bench_ret_values))) stop("Infinite values in benchmark returns")
    ##If lenghts differ, throw error
    if (length(ret_values) != length(bench_ret_values)) stop("Lengths of returns and benchmark returns differ")
  ##########

  #Treat NAs and all equal
  ###########
    ##If na.rm TRUE, remove NAs from ret_values and remove same indices from bench_ret_values
    if (na.rm && any(is.na(ret_values))) {
      ##Get NAs indexes
      na_index <- which(is.na(ret_values))
      ##Clean
      ret_values <- ret_values[-na_index]
      bench_ret_values <- bench_ret_values[-na_index]
    }
    ##If ret_values = bench_values, return 0
    if (all(ret_values == bench_ret_values)) return(NA_real_)
  ###########

  # Fit a linear model: returns ~ benchmark returns
  ###########
  reg <- stats::lm(ret_values ~ bench_ret_values - 1) # Fit linear regression without intercept
  residuals <- stats::residuals(reg) # Get residuals
  residuals_sum <- sum(residuals, na.rm = na.rm) # Sum of residuals
  residuals_sd  <- stats::sd(residuals, na.rm = na.rm) # Standard deviation of residuals
  if (is.na(residuals_sd) || residuals_sd < 1e-10) return(NA_real_)
  residuals_sum / residuals_sd
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
    ##If ret_values are all NAs, return NA
    if (all(is.na(ret_values))) return(NA_real_)
    ##If ret_values contain Inf or -Inf, return NA
    if (any(is.infinite(ret_values))) return(NA_real_)
    ##If any NA or Inf in bench_ret_values, return error
    if (any(is.na(bench_ret_values))) stop("NA values in benchmark returns")
    if (any(is.infinite(bench_ret_values))) stop("Infinite values in benchmark returns")
    ##If lenghts differ, throw error
    if (length(ret_values) != length(bench_ret_values)) stop("Lengths of returns and benchmark returns differ")
  ###########

  #Treat NAs and all equal
  ###########
    ##If na.rm TRUE, remove NAs from ret_values and remove same indices from bench_ret_values
    if (na.rm && any(is.na(ret_values))) {
      ###Get NAs indexes
      na_index <- which(is.na(ret_values))
      ###Clean
      ret_values <- ret_values[-na_index]
      bench_ret_values <- bench_ret_values[-na_index]
    }
    ##If ret_values = bench_values, return 0
    if (all(ret_values == bench_ret_values)) return(0)

  ###########


  # Fit a linear model: returns ~ benchmark returns
  ###########
  reg <- stats::lm(ret_values ~ bench_ret_values)
  beta_est <- stats::coef(reg)[2] %>% unname() #Get beta
  sd_signal <- stats::sd(ret_values, na.rm = na.rm) #Get standard deviation of stock returns
  sd_bench  <- stats::sd(bench_ret_values, na.rm = na.rm) #Get standard deviation of benchmark returns
  idio_var <- sd_signal^2 - beta_est^2 * sd_bench^2 #Compute idiosyncratic variance

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

