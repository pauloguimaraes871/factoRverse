# compute_window ----------------------------------------------------
#' Compute Rolling or Seasonal Calculation for a given signal in a meta_dataframe
#'
#' This method computes a calculation for each observation in a \code{meta_dataframe} object by applying a
#' predefined function (specified by a character) on the filtered values (for the same ticker) that fall within a period
#' preceding the current observation's date. The filtering can be done either using a full rolling window (all dates between
#' \code{current_date - period months} and \code{current_date}) or a seasonal window. In the seasonal mode, only observations
#' whose months belong to the consecutive months immediately following the current observation's month are selected.

#'
#' @param features_m_df A \code{meta_dataframe} object.
#' @param period A \code{numeric} value representing the number of months to look back in case of 'rolling' and the number of months to look ahead in case of 'seasonal'.
#' @param window A \code{character} specifying the window type. Can be either "rolling" or "seasonal". Default is "rolling".
#' @param signal A \code{character} specifying the column name on which the rolling function is computed.
#' @param FUN A \code{character} specifying the function to apply. Options are "median", "sd", "cagr", "skew", "sur", "mean_std",
#' "res_mom", "idio_vol".
#' @param benchmark_returns_m_xts A meta_xts object. Required for FUN "res_mom" and "idio_vol".
#' @param selected_bench A \code{character} specifying the column name in benchmark_returns_m_xts@data to use. Required for FUN "res_mom" and "idio_vol".
#' @param na.rm A \code{logical} indicating whether to remove NA values (default TRUE).
#' @param only_unique A \code{logical} indicating whether to compute the metric on unique values only (default FALSE).
#' @param feature_name A \code{character} specifying the name of the feature to be added to the meta_dataframe. If NULL,
#' the feature name will be set to "<signal>_roll_<period>_<FUN>". Default is NULL.
#' @param min_non_na A \code{numeric} value specifying the minimum number of non-NA values required to compute the rolling stat. Default is 0.
#' The only exception is for FUN "cagr" where the default minimum number of non-NA values required is period + 1 (to ensure correct number of periods).
#'
#' @return A \code{meta_dataframe} object with an added column named \code{<signal>_<window>_<period>_<FUN>} in its
#' \code{data} slot containing the computed values.
#'
#' @details
#' For each row (current observation), the method identifies all previous observations (for the same ticker) whose dates fall
#' between \code{current_date - period months} (inclusive) and \code{current_date} (inclusive) for 'rolling' and
#' all past months that are 'period' ahead of current month, then applies a function, determined by \code{FUN},
#' on the corresponding values in the specified signal column.
#' If no matching observation is found, the resulting value is \code{NA}. The available functions are:
#' \itemize{
#'   \item \strong{median}: \code{stats::median(x, na.rm = na.rm)}
#'   \item \strong{sd}: \code{stats::sd(x, na.rm = na.rm)}
#'   \item \strong{skew}: computed as \code{mean((x - mean(x))^3, na.rm = na.rm) / stats::sd(x, na.rm = na.rm)^3}
#'   \item \strong{sur}: \code{(final_value - mean(x, na.rm = na.rm)) / stats::sd(x, na.rm = na.rm)}, where final_value is the current signal value.
#'   \item \strong{cv}: \code{stats::mean(unique(x), na.rm = na.rm) / stats::sd(unique(x), na.rm = na.rm)}
#'   \item \strong{res_mom}: calculates rolling windows for both the signal and the benchmark (from benchmark_returns_m_xts).
#'         It then regresses the signal on the benchmark to obtain alpha and beta and returns
#'         \code{(alpha + beta * current_benchmark) - current_signal}.
#'   \item \strong{idio_vol}: calculates rolling windows for both the signal and the benchmark, obtains beta via regression,
#'         and then computes \code{sqrt(sd(signal)^2 - beta^2 * sd(benchmark)^2)}.
#' }
#'
#' @examples
#' \dontrun{
#'   # Suppose features_m_df is a meta_dataframe object, benchmark_returns_m_xts is a meta_xts object,
#'   # and "Alpha" is one of the signal columns and "SPY" is the benchmark column:
#'   features_m_df <- compute_rolling(features_m_df, period = 3, signal = "Alpha", FUN = "res_mom",
#'                                    benchmark_returns_m_xts = benchmark_returns_m_xts, bench = "SPY")
#' }
#'
#' @export
setGeneric("compute_meta_dataframe", function(features_m_df, period, signal, FUN, window = "rolling", benchmark_returns_m_xts = NULL, selected_bench = NULL, na.rm = TRUE,
                                              only_unique = FALSE, feature_name = NULL, min_non_na = 0){
  standardGeneric("compute_meta_dataframe")
})

setMethod("compute_meta_dataframe",
          signature(features_m_df = "meta_dataframe", period = "numeric", signal = "character", FUN = "character"),
          function(features_m_df, period, signal, FUN, window = "rolling", benchmark_returns_m_xts = NULL, selected_bench = NULL, na.rm = TRUE, only_unique = FALSE,
                   feature_name = NULL, min_non_na = 0) {

            #Pass features_m_df as pre_silver_features_m_df
            ############
            meta_dataframe_workflow <- features_m_df@workflow
            meta_dataframe_name <- features_m_df@meta_dataframe_name
            current_date <- features_m_df@current_date
            pre_silver_features_m_df <- features_m_df@data
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
            if (period < 0) {
              stop("The period must be greater or equal to 0.")
            }
            ##Check for only unique and FUN
            if (only_unique && FUN %in% c("cagr", "res_mom", "idio_vol")){
              stop("The 'only_unique' is not supported for FUN ", FUN)
            }
            ##Additional Checks for FUN types
            if (FUN %in% c("res_mom", "idio_vol")) {
              if (window != "rolling"){
                stop("The 'window' argument must be 'rolling' for FUN ", FUN)
              }
              if (is.null(benchmark_returns_m_xts)) {
                stop("benchmark_returns_m_xts must be provided for FUN ", FUN)
              }
              if (is.null(selected_bench)) {
                stop("The 'selected_bench' argument must be provided for FUN ", FUN)
              }
              if (!all(class(benchmark_returns_m_xts) == "returns_meta_xts")) {
                stop("benchmark_returns_m_xts must be an returns_meta_xts object")
              }

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
                  fwd_months <- if (period == 0) months(0) else months(1:period)
                  seasonal_months <- lubridate::month(lubridate::add_with_rollback(current_row_date, fwd_months, roll = TRUE))

                  ####Filter
                  selected_pre_silver_features_m_df <- pre_silver_features_m_df %>%
                    dplyr::filter(tickers == ticker_i) %>%
                    dplyr::filter(dates <= current_row_date) %>%
                    dplyr::filter(lubridate::month(dates) %in% seasonal_months)
                } else {
                  stop("Invalid window type. Must be either 'rolling' or 'seasonal'.")
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
                ####Extract rolling window for benchmark from benchmark_returns_m_xts@data
                bench_xts <- benchmark_returns_m_xts@data
                bench_ret_values <- as.numeric(bench_xts[zoo::index(bench_xts) >= lower_date & zoo::index(bench_xts) <= current_row_date, selected_bench])
              }

              ##Returns NA in case of certain conditions
                ###Return NA in case min_non_na is not filled
                if (FUN == "cagr" && length(values) < period + 1){
                  message("For cagr, the should be at least ", period + 1, " values")
                  return(NA_real_)
                }
                if (FUN != "cagr" && length(values) < min_non_na) return(NA_real_)

                ###Return NA in case values is not of class numeric
                if (!is.numeric(values)) {
                  return(NA_real_)
                }
                ###Return NA in case bench_ret_values length is 0
                if (!is.null(benchmark_returns_m_xts) && length(bench_ret_values) == 0) {
                  NA_real_
                }

              ##Depending on FUN, compute the rolling metric
              switch(FUN,
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
                     "res_mom" = res_mom(ret_values = values, bench_ret_values = bench_ret_values, na.rm = na.rm),
                     "idio_vol" = idio_vol(ret_values = values, bench_ret_values = bench_ret_values, na.rm = na.rm),
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
