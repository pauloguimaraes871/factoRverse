#'Financial Ratios
#'
#' This function calculates financial ratios and handles situations when both elements cannot be negative at the same time, such as in the calculation of Return on Equity (ROE).
#'
#' @param inc_statement_item A matrix representing items from the income statement.
#' @param bs_item A matrix representing items from the balance sheet.
#'
#' @return A matrix containing the calculated financial ratios.
#' 
#' @details This function calculates ratios by dividing corresponding elements of the income statement matrix by the balance sheet matrix. If both elements are negative, the ratio is considered undefined and marked as NA.
#'
#' @export
#'
#' @examples
#' # Example matrices for income statement and balance sheet items
#' income_statement <- matrix(c(100, 200, -50, 300, NA, 150), nrow = 3)
#' balance_sheet <- matrix(c(1000, 1500, -200, -300, 500, 700), nrow = 3)
#' # Calculate financial ratios
#' fin_ratio(income_statement, balance_sheet)
#' 
fin_ratio <- function(inc_statement_item, bs_item) {
  if(!all(dim(inc_statement_item) == dim(bs_item))) {
    stop("Input matrices must have the same dimensions.")
  } else {
    ratio_matrix <- matrix(NA, nrow = nrow(inc_statement_item), ncol = ncol(inc_statement_item))
    for (i in 1:nrow(ratio_matrix)) {
      for (j in 1:ncol(ratio_matrix)) {
        if(is.na(inc_statement_item[i,j]) == TRUE || is.na(bs_item[i,j] == TRUE)) { #If either is NA, ratio is NA
          ratio_matrix[i,j] <- NA 
        } else {
          if (inc_statement_item[i, j] < 0 & bs_item[i, j] < 0) { # If both net_inc and be are negative, ratio is undefined
            ratio_matrix[i, j] <- NA
          } else {
            ratio_matrix[i, j] <- inc_statement_item[i, j] / bs_item[i, j]
          }
        }
      }
    }
  }
  #Return  matrix
  return(ratio_matrix)
}