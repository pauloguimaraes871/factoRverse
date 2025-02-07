#' Roll Forward Portfolio Weights
#'
#' This function updates (or "rolls forward") a set of end-of-period portfolio weights
#' using forward returns. It multiplies each weight by \code{(1 + fwd_return_1m/100)} and then
#' normalizes the result so that the updated weights sum to 1.
#'
#' @param port_weights_m_d_ref A data frame containing the current end-of-period (EOP) portfolio weights.
#'   Must include columns:
#'   \itemize{
#'     \item \code{tickers}: A unique identifier for each asset.
#'     \item \code{eop_port_weights}: The end-of-period weights of each ticker.
#'   }
#' @param clean_fwd_return_1m_m_d_ref A data frame containing forward monthly returns (in percent).
#'   Must include columns:
#'   \itemize{
#'     \item \code{tickers}: The same identifier used in \code{port_weights_m_d_ref}.
#'     \item \code{fwd_return_1m}: The forward return in percentage points (e.g., 2 means 2%).
#'   }
#'
#' @details
#' The function joins \code{port_weights_m_d_ref} with \code{clean_fwd_return_1m_m_d_ref} by \code{tickers},
#' calculates the new weights by multiplying each \code{eop_port_weights} by
#' \code{(1 + fwd_return_1m / 100)}, and then normalizes the resulting vector to ensure the weights sum to 1.
#'
#' The columns \code{eop_port_weights} and \code{fwd_return_1m} are removed in the final output,
#' leaving \code{updated_port_weights} along with any remaining columns brought in by the join.
#'
#' @return A data frame that includes all joined columns (except for the removed \code{eop_port_weights} and
#'   \code{fwd_return_1m} columns) plus a newly calculated \code{updated_port_weights} column.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Example data
#' port_weights_m_d_ref <- data.frame(
#'   tickers = c("A", "B", "C"),
#'   eop_port_weights = c(0.5, 0.3, 0.2)
#' )
#'
#' clean_fwd_return_1m_m_d_ref <- data.frame(
#'   tickers = c("A", "B", "C"),
#'   fwd_return_1m = c(2, -1, 3)  # 2% for A, -1% for B, 3% for C
#' )
#'
#' # Roll forward the weights
#' new_weights <- roll_fwd_port_weights(
#'   port_weights_m_d_ref,
#'   clean_fwd_return_1m_m_d_ref
#' )
#'
#' # Inspect updated weights
#' new_weights
#' }
#'
roll_fwd_port_weights <- function(port_weights_m_d_ref, clean_fwd_return_1m_m_d_ref){

  #Update port weights
  #######################
  ##Create rolled_fwd_port_weights_m_d_ref obj
  rolled_fwd_port_weights_m_d_ref <- port_weights_m_d_ref

  ##Add fwd_return_1m to rolled_fwd_port_weights_m_d_ref
  rolled_fwd_port_weights_m_d_ref <- rolled_fwd_port_weights_m_d_ref %>% dplyr::left_join(clean_fwd_return_1m_m_d_ref, by = "tickers")

  ##Calculate updated_port_weights
  rolled_fwd_port_weights_m_d_ref <- rolled_fwd_port_weights_m_d_ref %>%
    dplyr::mutate(
      updated_port_weights = eop_port_weights * (1 + fwd_return_1m/100) #Update port_weights with fwd_return_1m
    ) %>%
    dplyr::mutate(
      updated_port_weights = updated_port_weights/sum(updated_port_weights) #Normalize updated_port_weights
    ) %>%
    dplyr::select(-dplyr::any_of("eop_port_weights", "fwd_return_1m", "bench_weights")) #Remove eop_port_weights, fwd_return_1m and possibly bench-weights

  return(rolled_fwd_port_weights_m_d_ref)


  #######################

}
