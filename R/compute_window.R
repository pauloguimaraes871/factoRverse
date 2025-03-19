# compute_window ----------------------------------------------------
#' Compute Rolling or Fwd Seasonal Calculations for a Given Metric in meta_dataframe or meta_xts
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
#'   - For `rolling`, the number of months to look back.
#'   - For `fwd_seasonal`, the number of months to look ahead.
#' @param window A `character` specifying the window type: either "rolling" (default) or "fwd_seasonal".
#' @param FUN A `character` specifying the function to apply. Supported options:
#'   - "median", "sd", "cagr", "skew", "sur", "mean_std", "res_mom", "idio_vol".
#' @param signal (For `meta_dataframe`) A `character` specifying the column name on which the rolling function is computed.
#' @param metric (For `meta_xts`) A `character` specifying the metric column name.
#' @param benchmark_returns_m_xts A `meta_xts` object. Required for `FUN` "res_mom" and "idio_vol".
#' @param selected_bench A `character` specifying the column name in `benchmark_returns_m_xts@data` to use. Required for `FUN` "res_mom" and "idio_vol".
#' @param na.rm A `logical` indicating whether to remove `NA` values (default: TRUE).
#' @param only_unique A `logical` indicating whether to compute the metric using only unique values (default: FALSE).
#' @param feature_name A `character` specifying the name of the computed feature. If NULL (default), the feature name is set to `"<metric>_<window>_<period>_<FUN>"`.
#' @param min_non_na A `numeric` value specifying the minimum number of non-NA values required to compute the rolling statistic. Default is 0.
#'   - Exception: For `FUN = "cagr"`, the default is `period + 1` to ensure sufficient periods.
#' @param count_condition_fun A function that takes a numeric vector and returns a logical vector.
#' The function should return `TRUE` for elements that should be counted. Only used for FUN = "count_if".

#'
#' @return A modified `meta_dataframe` or `meta_xts` object with an additional column named `"<metric>_<window>_<period>_<FUN>"`,
#' storing the computed values.
#'
#' @details
#' The function filters observations within the specified time window and applies the selected `FUN`. If no matching observations are found, the result is `NA`.
#'
#' Available functions:
#' \itemize{
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

#' method for meta_dataframe
setMethod("compute_window",
          signature(data = "meta_dataframe", period = "numeric", FUN = "character"),
          function(data, period, FUN, window = "rolling", signal, benchmark_returns_m_xts = NULL, selected_bench = NULL, na.rm = TRUE, only_unique = FALSE,
                   feature_name = NULL, min_non_na = 0, count_condition_fun = NULL) {

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
            ##Check if period is >= 0
            if (FUN != "lag" && period < 0) {
              stop("The period must be greater or equal to 0.")
            }
            ##Check for only unique and FUN
            if (only_unique && FUN %in% c("cagr", "res_mom", "idio_vol", "lag", "geom_mean")){
              stop("The 'only_unique' is not supported for FUN ", FUN)
            }
            ##Additional Checks for FUN types
            if (FUN %in% c("res_mom", "idio_vol")) {
              if (window != "rolling"){
                stop("The 'window' argument must be 'rolling' for FUN ", FUN)
              }
              if (!is_returns_meta_xts) {
                stop("benchmark_returns_m_xts must be provided for FUN ", FUN)
              }
              if (is.null(selected_bench)) {
                stop("The 'selected_bench' argument must be provided for FUN ", FUN)
              }
            }
            if (!FUN == "count_if" && !is.null(count_condition_fun)) {
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
              } else if (window == "fwd_seasonal"){
                ####Determine seasonal months: consecutive months following the current month (wrapping around if needed)
                current_month <- lubridate::month(current_row_date)
                fwd_months <- if (period == 0) months(0) else months(1:period)
                seasonal_months <- lubridate::month(lubridate::add_with_rollback(current_row_date, fwd_months, roll = TRUE))

                ####Filter
                selected_pre_silver_features_m_df <- pre_silver_features_m_df %>%
                  dplyr::filter(tickers == ticker_i) %>%
                  dplyr::filter(dates <= current_row_date) %>% #This is crucial to avoid foward looking bias
                  dplyr::filter(lubridate::month(dates) %in% seasonal_months)
              } else {
                stop("Invalid window type. Must be either 'rolling' or 'fwd_seasonal'.")
              }

              ###If no matching observation is found, return NA
              if (nrow(selected_pre_silver_features_m_df) == 0) return(NA_real_)

              ##Get the filtered values for the signal
              values <- selected_pre_silver_features_m_df %>% dplyr::pull(!!rlang::sym(signal))
              ###Get begin and final (useful for cagr and sur)
              begin_value <- head(values, 1)
              final_value <- tail(values, 1)

              ###Get only unique
              if (only_unique) {
                if (FUN == "sur"){
                  ####For SUR, remove last value first
                  past_values <- values[-length(values)]
                  past_values <- unique(past_values)
                } else {
                  values <- unique(values) #Get unique values
                }
              }
              ##Do the same for benchmark if it is not NULL
              if (!is.null(benchmark_returns_m_xts)){
                ####Extract rolling window for benchmark from benchmark_returns_m_xts
                selected_bench_ret_values <- as.numeric(benchmark_returns_m_xts[zoo::index(benchmark_returns_m_xts) >= lower_date & zoo::index(benchmark_returns_m_xts) <= current_row_date, selected_bench])
              }

              ##Returns NA in case of certain conditions
              ###Return NA in case min_non_na is not filled
              if (FUN %in% c("cagr", "lag", "geom_mean") && length(values) < period + 1){
                if (FUN == "cagr") message("For cagr, the should be at least ", period + 1, " values")
                if (FUN == "lag") message("For lag, the should be at least ", period + 1, " values")
                if (FUN == "geom_mean") message("For geom_mean, the should be at least ", period + 1, " values")
                return(NA_real_)
              }
              if (FUN != "cagr" && length(values) < min_non_na) return(NA_real_)

              ###Return NA in case values is not of class numeric
              if (!is.numeric(values)) {
                return(NA_real_)
              }
              ###Return NA in case selected_bench_ret_values length is 0
              if (!is.null(benchmark_returns_m_xts) && length(selected_bench_ret_values) == 0) {
                NA_real_
              }

              ##Depending on FUN, compute the rolling metric
              switch(FUN,
                     "mean" = mean(values, na.rm = na.rm),
                     "median" = stats::median(values, na.rm = na.rm),
                     "sd" = stats::sd(values, na.rm = na.rm),
                     "skew" = skew(values, na.rm = na.rm),
                     "sur" = {
                       sur(final_value = final_value, past_values = past_values, na.rm = na.rm)
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
                       return(begin_value)
                     },
                     stop("Unsupported function type")
              )

            })

            ############
            # Create a new column name dynamically if feature_name is NULL
            ##Use short_window_name
            if (window == "rolling") short_window_name <- "roll"
            if (window == "fwd_seasonal") short_window_name <- "fwd_seas"
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

#' method for meta_xts
setMethod("compute_window",
          signature(data = "metrics_meta_xts", period = "numeric", FUN = "character"),
          function(data, period, FUN, window = "rolling", metric, na.rm = TRUE, only_unique = FALSE,
                   feature_name = NULL, min_non_na = 0) {

            #Extract relevant elements
            ###############
            meta_xts_workflow <- data@workflow
            meta_xts_name <- data@meta_xts_name
            current_date <- data@current_date
            source <- data@source
            pre_silver_m_xts <- data@data

            ###############

            #Initial checks
            ###############
            if (!metric %in% colnames(pre_silver_m_xts)) {
              stop("The metric column does not exist in the xts object.")
            }
            if (!is.numeric(pre_silver_m_xts[, metric])) {
              stop("The metric column must be numeric.")
            }
            if (period < 0) {
              stop("The period must be greater or equal to 0.")
            }
            if (only_unique && FUN %in% c("cagr")){
              stop("The 'only_unique' is not supported for FUN ", FUN)
            }
            if (FUN %in% c("res_mom", "idio_vol")) {
              stop("The FUN ", FUN, " is not supported for metrics_meta_xts")
            }

            ###############

            #Compute rolling values
            ###############
            rolling_values <- xts::xts(rep(NA_real_, nrow(pre_silver_m_xts)), order.by = zoo::index(pre_silver_m_xts))

              ##Loop through xts
              for (i in seq_len(nrow(pre_silver_m_xts))) {
                current_row_date <- zoo::index(pre_silver_m_xts)[i]
                lower_date <- lubridate::add_with_rollback(current_row_date, -months(period))

                ###Rolling window
                if (window == "rolling") {
                  selected_xts <- pre_silver_m_xts[zoo::index(pre_silver_m_xts) >= lower_date &
                                                 zoo::index(pre_silver_m_xts) <= current_row_date, metric]
                ###Seasonal window
                } else if (window == "fwd_seasonal") {
                  current_month <- lubridate::month(current_row_date)
                  fwd_months <- if (period == 0) months(0) else months(1:period)
                  seasonal_months <- lubridate::month(lubridate::add_with_rollback(current_row_date, fwd_months, roll = TRUE))
                  selected_xts <- pre_silver_m_xts[lubridate::month(zoo::index(pre_silver_m_xts)) %in% seasonal_months &
                                                            zoo::index(pre_silver_m_xts) <= current_row_date, metric]
                } else {
                  stop("Invalid window type. Must be either 'rolling' or 'fwd_seasonal'.")
                }

                ###If length is 0, skip
                if (length(selected_xts) == 0) next

                ###Adjust values
                values <- as.numeric(selected_xts)
                begin_value <- head(values, 1)
                final_value <- tail(values, 1)
                if (only_unique) {
                  values <- unique(values)
                }
                if (FUN == "cagr" && length(values) < period + 1) {
                  rolling_values[i] <- NA_real_
                  next
                }
                if (FUN != "cagr" && length(values) < min_non_na) {
                  rolling_values[i] <- NA_real_
                  next
                }
                if (!is.numeric(values)) {
                  rolling_values[i] <- NA_real_
                  next
                }

                ###Apply FUN
                rolling_values[i] <- switch(FUN,
                                            "median" = stats::median(values, na.rm = na.rm),
                                            "sd" = stats::sd(values, na.rm = na.rm),
                                            "skew" = skew(values, na.rm = na.rm),
                                            "sur" = sur(final_value = final_value, past_values = values[-length(values)], na.rm = na.rm),
                                            "cagr" = cagr(begin = begin_value, final = final_value, period = period),
                                            "mean_std" = mean_std(values, na.rm = na.rm),
                                            stop("Unsupported function type"))
              }
            ###############

            #Assign computed values to a new column
            ###############
            short_window_name <- if (window == "rolling") "roll" else "fwd_seas"
            new_col_name <- if (is.null(feature_name)) paste0(metric, "_", FUN, "_", short_window_name, "_", period, "m") else feature_name
            pre_silver_m_xts <- cbind(pre_silver_m_xts, rolling_values)
            colnames(pre_silver_m_xts)[ncol(pre_silver_m_xts)] <- new_col_name

            #Update workflow
            new_workflow <- list(
              list(current_date = current_date,
                   timestamp = Sys.time(),
                   metric = metric,
                   period = period,
                   window = window,
                   feature_name = new_col_name,
                   FUN = FUN,
                   call = match.call(),
                   na.rm = na.rm,
                   only_unique = only_unique,
                   min_non_na = min_non_na)
            )

            #Recreate the meta_xts object
            pre_silver_m_xts <- create_meta_xts(pre_silver_m_xts,
                                                meta_xts_name = meta_xts_name,
                                                workflow = c(meta_xts_workflow, new_workflow),
                                                type = "metrics",
                                                source = c(source,
                                                           paste0("compute_", metric, "_", FUN, "_", short_window_name, "_", period, "m", "_", current_date))
            )

            names(pre_silver_m_xts@workflow)[length(pre_silver_m_xts@workflow)] <-
              paste0("compute_", metric, "_", FUN, "_", short_window_name, "_", period, "m", "_", current_date)


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


## Standardized Unexpected Realization (SUR)
#' Calculate Standardized Unexpected Realization (SUR)
#'
#' This function computes the Standardized Unexpected Realization (SUR) for a numeric vector.
#' SUR is defined as the deviation of the most recent (final) value from the mean of the vector,
#' divided by the standard deviation. This metric quantifies how extreme the last observation is compared to the historical distribution.
#'
#' @param values A numeric vector representing a series of observations.
#' @param na.rm A logical value indicating whether NA values should be removed before computation (default is TRUE).
#'
#' @return A numeric value representing the SUR. If the standard deviation is zero, \code{NA_real_} is returned.
#'
#' @export
#'
#' @examples
#' sur(c(1, 2, 3, 4, 5))
sur <- function(final_value, past_values, na.rm = TRUE) {

  #Calculate the SUR
  sd_val <- stats::sd(past_values, na.rm = na.rm)
  if (is.na(sd_val) || sd_val == 0) return(NA_real_)
  (final_value - mean(past_values, na.rm = na.rm)) / sd_val

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
#'
#' @examples
#' count_if(c(1, 0, 3, 0, 5), function(x) x == 0)  # Counts zeros -> returns 2
#' count_if(c(1, 2, 3, 4, 5), function(x) x > 2)   # Counts values greater than 2 -> returns 3
#' count_if(c(1, NA, 3, 0, 5), function(x) x == 0, na.rm = TRUE)  # Ignores NA, counts 0s -> returns 1
#' count_if(c(1, NA, 3, 0, 5), function(x) x == 0, na.rm = FALSE) # Includes NA, counts 0s -> returns NA
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

