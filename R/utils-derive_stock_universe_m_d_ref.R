#' Build Stock Universe with Expected Return Score
#'
#' This function creates a stock universe data frame by merging ticker information
#' with either out-of-sample predictions or signal values. It then transforms the score
#' using a user-provided function (`signal_transform`), which is typically used to
#' winsorize, z-score, and/or otherwise adjust the raw signal.
#'
#' @param signals_m_d_ref A data frame containing at least the columns `id`, `tickers`,
#'   and one or more signal metrics.
#' @param oos_predictions_m_d_ref Optional. A data frame with out-of-sample predictions.
#'   It must contain columns `id` and `pred`. If not provided (i.e. `NULL`), the signal
#'   from `signals_m_d_ref` is used.
#' @param chosen_score_metric_and_position A named character vector indicating the chosen
#'   signal metric and its associated position. For example, \code{c("signal" = "long")}
#'   implies a long position (multiplier 1) while \code{c("signal" = "short")} implies
#'   a short position (multiplier -1).
#' @param scaler_m_d_ref Optional. A data frame containing scaling factors with columns `id`, `tickers`,
#'  `dates`, and one or more scaler metrics. If not provided (i.e. `NULL`), no scaling is applied.
#' @param chosen_scaler Optional. A string indicating the chosen scaler metric from `scaler_m_d_ref`.
#'  If not provided (i.e. `NULL`), no scaling is applied.
#' @param scaler_shrinkage Numeric between 0 and 1. If greater than 0, applies shrinkage to the scaler values
#' towards 1 (i.e., no scaling). A value of 0 means no shrinkage, while a value of 1 means all scalers are set to 1.
#' Default is 0.
#' @param lower_quantile_winsorization Numeric. Lower quantile value for winsorization
#'   in \code{signal_transform}.
#' @param upper_quantile_winsorization Numeric. Upper quantile value for winsorization
#'   in \code{signal_transform}.
#'
#' @return A data frame with columns:
#'   \item{`id`}{A unique identifier combining the ticker and date.}
#'   \item{`tickers`}{Ticker symbols.}
#'   \item{`dates`}{The current date.}
#'   \item{`exp_ret_score`}{The expected return score after transformation.}
#'
#' @export
derive_stock_universe_m_d_ref <- function(signals_m_d_ref, oos_predictions_m_d_ref = NULL, chosen_score_metric_and_position = NULL,
                                          scaler_m_d_ref = NULL, chosen_scaler = NULL, scaler_shrinkage = 0,
                                          lower_quantile_winsorization, upper_quantile_winsorization) {

  #Initial checks
  ####################
    ##Check if one of oos_predictions_m_d_ref and chosen_score_metric_and_position are provided
    if (is.null(oos_predictions_m_d_ref) && is.null(chosen_score_metric_and_position)) {
      stop("Either oos_predictions_m_d_ref or chosen_score_metric_and_position must be provided.")
    }

    ##Check if one of oos_predictions_m_d_ref and chosen_score_metric_and_position are provided
    if (!is.null(oos_predictions_m_d_ref) && !is.null(chosen_score_metric_and_position)) {
      stop("Only one of oos_predictions_m_d_ref or chosen_score_metric_and_position should be provided.")
    }

    ##Check if chosen_scaler is in scaler_m_d_ref if provided
    if (!is.null(chosen_scaler) && !is.null(scaler_m_d_ref) && !(chosen_scaler %in% names(scaler_m_d_ref))) {
      stop("chosen_scaler must be the name of a column in scaler_m_d_ref.")
    }

    ##Check if chosen_scaler is not NULL if scaler_m_d_ref is provided
    if (!is.null(scaler_m_d_ref) && is.null(chosen_scaler)) {
      stop("If scaler_m_d_ref is provided, a chosen_scaler must be provided.")
    }

    ##Check if scaler_shrinkage is between 0 and 1
    if (!is.null(scaler_shrinkage) && (scaler_shrinkage < 0 || scaler_shrinkage > 1)) {
      stop("scaler_shrinkage must be a numeric value between 0 and 1.")
    }


  ####################

  #Initialize the stock universe data frame
  ####################
  ##Join into data.frame
  stock_universe_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates)
  ####################

  #Add exp_ret_score
  ####################
  ###oos_predictions_m_d_ref
  if (!is.null(oos_predictions_m_d_ref)) {
    # Define the metric name for expected return score
    stock_universe_m_d_ref <- stock_universe_m_d_ref %>%
      dplyr::left_join(
        oos_predictions_m_d_ref %>% dplyr::select(id, pred),
        by = "id"
      ) %>%
      dplyr::rename(exp_ret_score_raw = pred) %>%
      dplyr::mutate(exp_ret_score_raw = signal_transform(
        exp_ret_score_raw,
        lower_quantile_winsorization = lower_quantile_winsorization,
        upper_quantile_winsorization = upper_quantile_winsorization
       )
      )
    is_dummy <- FALSE
  } else {
    ###chosen score metric
    ####No predictions provided: use signals from signals_m_d_ref.
    ####Determine position and chosen signal metric.
    if (chosen_score_metric_and_position == "long") {
      position <- 1
      chosen_score <- names(chosen_score_metric_and_position)
    } else {
      position <- -1
      chosen_score <- names(chosen_score_metric_and_position)
    }

    #####Check if the chosen score column exists in signals_m_d_ref
    if (!chosen_score %in% names(signals_m_d_ref)) {
      stop("The chosen score column '", chosen_score, "' is not found in signals_m_d_ref.")
    }

    ####Check for dummy
    chosen_vals <- signals_m_d_ref[[chosen_score]]
    is_dummy <- all(is.na(chosen_vals) | chosen_vals %in% c(0, 1, TRUE, FALSE))

    ####Add chosen score
    stock_universe_m_d_ref <- stock_universe_m_d_ref %>%
      dplyr::left_join(
        signals_m_d_ref %>% dplyr::select(id, !!rlang::sym(chosen_score)),
        by = "id"
      ) %>%
      dplyr::rename(exp_ret_score_raw = !!rlang::sym(chosen_score)) %>%
      dplyr::mutate(
        exp_ret_score_raw =
          if (is_dummy){
            # keep dummy semantics; coerce TRUE/FALSE to 1/0, apply sign
            (as.numeric(exp_ret_score_raw %in% c(1, TRUE))) * position
          } else {
            signal_transform(
              exp_ret_score_raw * position,
              lower_quantile_winsorization = lower_quantile_winsorization,
              upper_quantile_winsorization = upper_quantile_winsorization
            )
          }
      )
  }


  #Scale exp_ret_score
  ####################
  if (!is.null(scaler_m_d_ref) && !is.null(chosen_scaler)){

    #Check if is_dummy is TRUE and short-circuit
    if (exists("is_dummy") && isTRUE(is_dummy)) {
      stop("Scaler provided but chosen score is a binary dummy. Prefer user AND rules for this use case.")
    }

    #Extract scaler values
    scaler_values <- scaler_m_d_ref %>%
      dplyr::select(id, !!rlang::sym(chosen_scaler)) %>%
      dplyr::rename(scaler = !!rlang::sym(chosen_scaler))

    #Join scaler values to stock_universe_m_d_ref
    stock_universe_m_d_ref <- stock_universe_m_d_ref %>%
      dplyr::left_join(scaler_values, by = "id")

    #Winsorize and transform scaler values
    #After transform, 1 => scaler is equal to sample mean
    stock_universe_m_d_ref <- stock_universe_m_d_ref %>%
      dplyr::mutate(
        scaler = signal_transform(
          scaler,
          lower_quantile_winsorization = lower_quantile_winsorization,
          upper_quantile_winsorization = upper_quantile_winsorization
        )
      )

    #Apply shrinkage if scaler_shrinkage > 0
    #If scaler_shrinkage is 1, all scalers are set to 1 (i.e. no scaling)
    if (scaler_shrinkage > 0) {
      stock_universe_m_d_ref <- stock_universe_m_d_ref %>%
        dplyr::mutate(
          scaler = (1 - scaler_shrinkage) * scaler + scaler_shrinkage * 1
        )
    }

    #Scale exp_ret_score and make sure it is the last column
    stock_universe_m_d_ref <- stock_universe_m_d_ref %>%
      dplyr::select(id, tickers, dates, exp_ret_score_raw, scaler) %>%
      dplyr::mutate(
        exp_ret_score = exp_ret_score_raw * scaler
      )


  } else {

    #If no scaling is applied, just rename exp_ret_score_raw
    stock_universe_m_d_ref <- stock_universe_m_d_ref %>%
      dplyr::mutate(
        exp_ret_score = exp_ret_score_raw
      )


  }


  return(stock_universe_m_d_ref)
}
