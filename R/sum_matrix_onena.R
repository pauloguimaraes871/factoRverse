#' Sum Two Matrices with NA Handling
#'
#' Computes the element-wise sum of two matrices while handling missing values (NA).
#'
#' This function takes two matrices of the same dimensions and returns another matrix where each element is the sum of the corresponding elements in the input matrices. Missing values (NA) are handled appropriately, so if an element is missing in one matrix but present in the other, it is treated as if it were zero for the purpose of addition.
#'
#' @param matrix1 A matrix, data frame, or tibble.
#' @param matrix2 A matrix, data frame, or tibble with the same dimensions as matrix1.
#'
#' @return A numeric matrix containing the element-wise sum of matrix1 and matrix2.
#' @export
#'
#' @examples
#'
#' matrix1 <- matrix(1:4, nrow = 2)
#' matrix2 <- matrix(c(5, NA, 7, 8), nrow = 2)
#'
#' # Compute the sum
#' result <- sum_matrix_onena(matrix1, matrix2)
#' result
sum_matrix_onena <- function(matrix1, matrix2) {
  # Check if inputs are matrices, data frames, or tibbles and convert to matrices
  if (!is.matrix(matrix1) && !is.data.frame(matrix1) && !is_tibble(matrix1)) {
    stop("matrix1 must be a matrix, data frame, or tibble.")
  }
  if (!is.matrix(matrix2) && !is.data.frame(matrix2) && !is_tibble(matrix2)) {
    stop("matrix2 must be a matrix, data frame, or tibble.")
  }

  # Convert to matrices
  matrix1 <- as.matrix(matrix1)
  matrix2 <- as.matrix(matrix2)

  # Check for dimension equality
  if (!all(dim(matrix1) == dim(matrix2))) {
    stop("Input matrices must have the same dimensions.")
  }

  # Initialize sum matrix
  sum_matrix <- matrix(NA, nrow = nrow(matrix1), ncol = ncol(matrix1))

  # Element-wise sum with NA handling
  for (i in seq_len(nrow(matrix1))) {
    for (j in seq_len(ncol(matrix1))) {
      if (!is.na(matrix1[i, j]) && !is.na(matrix2[i, j])) {
        sum_matrix[i, j] <- matrix1[i, j] + matrix2[i, j]
      } else if (is.na(matrix1[i, j])) {
        sum_matrix[i, j] <- matrix2[i, j]
      } else {
        sum_matrix[i, j] <- matrix1[i, j]
      }
    }
  }

  return(sum_matrix)
}
