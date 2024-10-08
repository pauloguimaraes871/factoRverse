#' Calculate Element-Wise Compound Annual Growth Rate (CAGR)
#'
#' This function computes the Compound Annual Growth Rate (CAGR) for corresponding
#' elements in two matrices. The CAGR represents the average annual growth rate of
#' an investment over a specified time period, assuming the investment grows at a
#' steady rate.
#'
#' @param matrix_begin A numeric matrix, data.frame, or tibble containing the initial values.
#' @param matrix_final A numeric matrix, data.frame, or tibble containing the final values.
#' @param period A numeric value representing the number of periods over which the CAGR is calculated.
#'
#' @return A numeric matrix of CAGR values for each element. The function will return `NA`
#' for positions where the initial value is zero or the final value is `NA`.
#'
#' @details The function first converts any data frames or tibbles to matrices. It then
#' checks that the input matrices have the same dimensions before proceeding with the
#' CAGR calculations. The formula used for CAGR is:
#'
#' \deqn{CAGR = \left( \frac{final}{begin} \right)^{\frac{1}{period}} - 1}
#'
#' If the initial value is zero, the CAGR is set to `NA` to avoid division by zero errors.
#'
#' @export
#'
#' @examples
#' matrix_begin <- matrix(c(100, 200, 300, 400), nrow = 2)
#' matrix_final <- matrix(c(150, 250, 350, 450), nrow = 2)
#' period <- 1
#' cagr_matrix(matrix_begin, matrix_final, period)
cagr_matrix <- function(matrix_begin, matrix_final, period) {

  # Check that all inputs are either matrices, data frames, or tibbles
  if (!all(sapply(list(matrix_begin, matrix_final), function(mat) {
    is.matrix(mat) || is.data.frame(mat) || tibble::is_tibble(mat)
  }))) {
    stop("Both inputs must be matrices, data.frames, or tibbles.")
  }

  # Convert data frames and tibbles to matrices
  matrices <- lapply(list(matrix_begin, matrix_final), function(mat) {
    if (is.data.frame(mat) || tibble::is_tibble(mat)) {
      return(as.matrix(mat))
    }
    return(mat)
  })

  # Extract matrices after conversion
  matrix_begin <- matrices[[1]]
  matrix_final <- matrices[[2]]

  # Check if the matrices have the same dimensions
  if (!all(dim(matrix_begin) == dim(matrix_final))) {
    stop("Input matrices must have the same dimensions.")
  }

  # Initialize result matrix
  cagr_matrix <- matrix(NA, nrow = nrow(matrix_begin), ncol = ncol(matrix_begin))

  # Calculate CAGR for each element
  for (i in 1:nrow(matrix_begin)) {
    for (j in 1:ncol(matrix_begin)) {
        cagr_matrix[i, j] <- (matrix_final[i, j] / matrix_begin[i, j])^(1 / period) - 1
      }
  }

  return(cagr_matrix)
}
