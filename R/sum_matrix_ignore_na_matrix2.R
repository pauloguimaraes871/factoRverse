#' Element-wise Matrix Sum with Special NA Handling
#'
#' Computes the element-wise sum of two matrices while ignoring values in the second matrix if corresponding element in matrix1 is NA.
#'
#' This function takes two matrices of the same dimensions and returns another matrix where each element is the sum of the corresponding elements in the input matrices. Unlike traditional matrix addition, this function treats values in the second matrix differently depending on corresponding element in matrix1: if an element is NA in first matrix, corresponding element in the resulting matrix is NA, regardless of the value in the second matrix.
#'
#' @param matrix1 A matrix, data frame, or tibble. Values in this matrix are not ignored during addition.
#' @param matrix2_ignoreifna A matrix, data frame, or tibble. with the same dimensions as matrix1. Values in this matrix are ignored during addition if there are NAs in corresponding element in matrix1.
#' @return A numeric matrix containing the element-wise sum of matrix1 and matrix2_ignoreifna, with values in matrix2_ignoreifna ignored if corresponding element in matrix1 is NA.
#' @examples
#' # Example matrices
#' matrix1 <- matrix(1:4, nrow = 2)
#' matrix2_ignoreifna <- matrix(c(5,NA,7,8), nrow = 2)
#'
#' # Compute the sum
#' result <- sum_matrix_ignore_na_matrix2(matrix1, matrix2_ignoreifna)
#' result
#'
#' matrix1 <- matrix(c(1,2,NA,4), nrow = 2)
#' matrix2_ignoreifna <- matrix(c(5,6,7,8), nrow = 2)
#'
#' # Compute the sum
#' result <- sum_matrix_ignore_na_matrix2(matrix1, matrix2_ignoreifna)
#' result
#'
#' @export
sum_matrix_ignore_na_matrix2 <- function(matrix1, matrix2_ignoreifna) {
  # Check if inputs are matrices, data frames, or tibbles and convert to matrices
  if (!is.matrix(matrix1) && !is.data.frame(matrix1) && !is_tibble(matrix1)) {
    stop("matrix1 must be a matrix, data frame, or tibble.")
  }
  if (!is.matrix(matrix2_ignoreifna) && !is.data.frame(matrix2_ignoreifna) && !is_tibble(matrix2_ignoreifna)) {
    stop("matrix2_ignoreifna must be a matrix, data frame, or tibble.")
  }

  # Convert to matrices
  matrix1 <- as.matrix(matrix1)
  matrix2_ignoreifna <- as.matrix(matrix2_ignoreifna)

  # Check for dimension equality
  if (!all(dim(matrix1) == dim(matrix2_ignoreifna))) {
    stop("Input matrices must have the same dimensions.")
  }

  # Initialize sum matrix
  sum_matrix <- matrix(NA, nrow = nrow(matrix1), ncol = ncol(matrix1))

  # Element-wise sum with NA handling
  for (i in 1:nrow(matrix1)) {
    for (j in 1:ncol(matrix1)) {
      if (!is.na(matrix1[i, j]) && !is.na(matrix2_ignoreifna[i, j])) {
        sum_matrix[i, j] <- matrix1[i, j] + matrix2_ignoreifna[i, j]
      } else if (is.na(matrix1[i, j])) {
        sum_matrix[i, j] <- NA
      } else {
        sum_matrix[i, j] <- matrix1[i, j]
      }
    }
  }

  return(sum_matrix)
}
