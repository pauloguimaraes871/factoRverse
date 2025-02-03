#' Build Stock Universe with Expected Return Score
#'
#' This function creates a stock universe data frame by merging ticker information
#' with either out-of-sample predictions or signal values. It then transforms the score
#' using a user-provided function (`signal_transform`), which is typically used to
#' winsorize, z-score, and/or otherwise adjust the raw signal.
#'
#' @param signals_m_d_ref A data frame containing at least the columns `id`, `tickers`,
#'   and one or more signal metrics.
#' @param oos_predictions_m_df Optional. A data frame with out-of-sample predictions.
#'   It must contain columns `id` and `pred`. If not provided (i.e. `NULL`), the signal
#'   from `signals_m_d_ref` is used.
#' @param chosen_score_metric_and_position A named character vector indicating the chosen
#'   signal metric and its associated position. For example, \code{c("signal" = "long")}
#'   implies a long position (multiplier 1) while \code{c("signal" = "short")} implies
#'   a short position (multiplier -1).
#' @param lower_quantile_winsorization Numeric. Lower quantile value for winsorization
#'   in \code{signal_transform}.
#' @param upper_quantile_winsorization Numeric. Upper quantile value for winsorization
#'   in \code{signal_transform}.
#' @param signal_transform A function that transforms the score. It should accept at least
#'   three arguments: the score vector, \code{lower_quantile_winsorization}, and
#'   \code{upper_quantile_winsorization}.
#'
#' @return A data frame with columns:
#'   \item{`id`}{A unique identifier combining the ticker and date.}
#'   \item{`tickers`}{Ticker symbols.}
#'   \item{`dates`}{The current date.}
#'   \item{`exp_ret_score`}{The expected return score after transformation.}
#'
#' @examples
#' \dontrun{
#' # Define your own signal_transform function (here a dummy identity function is used)
#' dummy_signal_transform <- function(x, lower_quantile_winsorization, upper_quantile_winsorization) { x }
#'
#' current_tickers <- c("AAPL", "GOOG")
#' current_date <- "2025-02-03"
#'
#' signals_m_d_ref <- data.frame(
#'   id = paste0(current_tickers, "-", current_date),
#'   tickers = current_tickers,
#'   signal = c(0.1, 0.2),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example with predictions:
#' oos_predictions_m_df <- data.frame(
#'   id = paste0(current_tickers, "-", current_date),
#'   pred = c(0.15, 0.25),
#'   stringsAsFactors = FALSE
#' )
#'
#' result <- build_stock_universe(
#'   current_tickers = current_tickers,
#'   current_date = current_date,
#'   signals_m_d_ref = signals_m_d_ref,
#'   oos_predictions_m_df = oos_predictions_m_df,
#'   chosen_score_metric_and_position = c("signal" = "long"),
#'   lower_quantile_winsorization = 0.05,
#'   upper_quantile_winsorization = 0.95,
#'   signal_transform = dummy_signal_transform
#' )
#' }
#'
#' @export
derive_stock_universe_m_d_ref <- function(signals_m_d_ref, oos_predictions_m_df = NULL, chosen_score_metric_and_position = NULL,
                                          lower_quantile_winsorization, upper_quantile_winsorization) {

  #Initial checks
  ####################
    ##Check if one of oos_predictions_m_df and chosen_score_metric_and_position are provided
    if (is.null(oos_predictions_m_df) && is.null(chosen_score_metric_and_position)) {
      stop("Either oos_predictions_m_df or chosen_score_metric_and_position must be provided.")
    }

    ##Check if one of oos_predictions_m_df and chosen_score_metric_and_position are provided
    if (!is.null(oos_predictions_m_df) && !is.null(chosen_score_metric_and_position)) {
      stop("Only one of oos_predictions_m_df or chosen_score_metric_and_position should be provided.")
    }
  ####################

  #Initialize the stock universe data frame
  ####################
    ##Get tickers and date
    current_tickers <- signals_m_d_ref %>% dplyr::pull(tickers)
    current_date <- signals_m_d_ref %>% dplyr::pull(dates) %>% unique()

    ##Join into data.frame
    stock_universe_m_d_ref <- signals_m_d_ref %>% dplyr::select(id, tickers, dates)
  ####################

  #Add exp_ret_score
  ####################
    ###oos_predictions_m_df
    if (!is.null(oos_predictions_m_df)) {
      # Define the metric name for expected return score
      exp_ret_score_metric <- "oos_pred"

      stock_universe_m_d_ref <- stock_universe_m_d_ref %>%
        dplyr::left_join(
          oos_predictions_m_df %>% dplyr::select(id, pred),
          by = "id"
        ) %>%
        dplyr::rename(exp_ret_score = pred) %>%
        dplyr::mutate(exp_ret_score = signal_transform(
          exp_ret_score,
          lower_quantile_winsorization = lower_quantile_winsorization,
          upper_quantile_winsorization = upper_quantile_winsorization
        ))
    } else {
      ###chosen score metric
        ####No predictions provided: use signals from signals_m_d_ref.
        ####Determine position and chosen signal metric.
      if (chosen_score_metric_and_position == "long") {
        position <- 1
        chosen_score <- names(chosen_score_metric_and_position)
        exp_ret_score_metric <- chosen_score
      } else {
        position <- -1
        chosen_score <- names(chosen_score_metric_and_position)
        exp_ret_score_metric <- paste0("low_", chosen_score)
      }

          #####Check if the chosen score column exists in signals_m_d_ref
          if (!chosen_score %in% names(signals_m_d_ref)) {
            stop("The chosen score column '", chosen_score, "' is not found in signals_m_d_ref.")
          }

        ####Add chosen score
        stock_universe_m_d_ref <- stock_universe_m_d_ref %>%
          dplyr::left_join(
            signals_m_d_ref %>% dplyr::select(id, !!rlang::sym(chosen_score)),
            by = "id"
          ) %>%
          dplyr::rename(exp_ret_score = !!rlang::sym(chosen_score)) %>%
          dplyr::mutate(exp_ret_score = signal_transform(
            exp_ret_score * position,
            lower_quantile_winsorization = lower_quantile_winsorization,
            upper_quantile_winsorization = upper_quantile_winsorization
          ))
    }

  return(stock_universe_m_d_ref)
}
