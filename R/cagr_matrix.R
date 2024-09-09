#' Calculate Element-Wise Compound Annual Growth Rate (CAGR) for Matrices
#'
#' @param matrix_begin A numeric matrix containing the initial values.
#' @param matrix_final A numeric matrix containing the final values.
#' @param period The number of periods.
#'
#' @return A numeric matrix of CAGR values for each element.
#' @export
#'
#' @examples
#' matrix_begin <- matrix(c(100, 200, 300, 400), nrow = 2)
#' matrix_final <- matrix(c(150, 250, 350, 450), nrow = 2)
#' period <- 1
#' cagr_matrix(matrix_begin, matrix_final, period)
cagr_matrix <- function(matrix_begin, matrix_final, period){
  # Check if the matrices have the same dimensions
  if(!all(dim(matrix_begin) == dim(matrix_final))) {
    stop("Input matrices must have the same dimensions.")
  } else {
    cagr_matrix <- matrix(NA, nrow = nrow(matrix_begin), ncol = ncol(matrix_begin))
    for(i in 1:nrow(matrix_begin)){
      for(j in 1:ncol(matrix_begin)){
        cagr_matrix[i,j] <- cagr(matrix_begin[i,j], matrix_final[i,j], period)
      }
    }
    return(cagr_matrix)
  }
}
