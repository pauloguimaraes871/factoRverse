#' Calculate element-wise 36m median matrix.
#' Useful for calculating the median of firm-level data from last 3 years.
#'
#' @param matrix1 A matrix with firm-level data
#' @param matrix2 A matrix with firm-level data
#' @param matrix3 A matrix with firm-level data
#' 
#'
#' @return A matrix in which inputs are element-wise medians of corresponding elements in each matrix.
#' The function automatically handles `NA` values by ignoring them during median calculation.
#' 
#' 
#' @details This function computes the element-wise median across three matrices of equal dimensions.
#' It is particularly useful for scenarios requiring aggregation of data over time or among different data sets.
#' By default, `NA` values are excluded from the median calculation. If all values in a position across matrices
#' are `NA`, the result will be `NA` for that position.
#'
#' 
#'
#' @export
#'
#' @examples
#' median_matrix_36m(
#'   matrix(c(1,2,3,4), nrow=2, ncol=2),
#'   matrix(c(5,6,7,8), nrow=2, ncol=2),
#'   matrix(c(1,1,1,1), nrow=2, ncol=2))
median_matrix_36m <- function(matrix1, matrix2, matrix3){
  # Check if the matrices have the same dimensions
  if (!all(dim(matrix1) == dim(matrix2) & dim(matrix1) == dim(matrix3))) {
    stop("Input matrices must have the same dimensions.")
  }
  calculated_median_36m <- matrix(NA, nrow = nrow(matrix1), ncol = ncol(matrix1))
  for(i in 1:nrow(matrix1)){
    for(j in 1:ncol(matrix1)){
        calculated_median_36m[i,j] <- stats::median(c(matrix1[i,j], matrix2[i,j], matrix3[i,j]), na.rm = TRUE)
      
    }
  }
  return(calculated_median_36m)
}


