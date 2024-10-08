#' Sharpe Ratio Matrix
#'
#' This function calculates the Sharpe ratio matrix from a list of matrices or data frames.
#'
#' @param initial_list A list of matrices, data frames, or tibbles with the same dimensions.
#'
#' @return A matrix containing the Sharpe ratio calculated for each element of the input matrices.
#'
#' @details The Sharpe ratio is calculated as the mean divided by the standard deviation for each element of the input matrices.
#'
#' @examples
#' # Create a list of matrices
#' mat1 <- matrix(rnorm(9), nrow = 3)
#' mat2 <- matrix(rnorm(9), nrow = 3)
#' initial_list <- list(mat1, mat2)
#' # Calculate the Sharpe ratio matrix
#' sharpe_matrix(initial_list)
#'
#' @export
#'
#' @references William F. Sharpe. "The Sharpe Ratio". The Journal of Portfolio Management 21 (1994): 49–58.

sharpe_matrix <- function(initial_list) {
  # Check if the input is a list of valid classes and transform to matrices
  if (!is.list(initial_list) ||
      !all(vapply(initial_list, function(x) is.matrix(x) || is.data.frame(x) || inherits(x, "tbl_df"), logical(1))) ||
      length(unique(sapply(initial_list, nrow))) != 1 ||
      length(unique(sapply(initial_list, ncol))) != 1) {
    stop("Input must be a list of matrices, data.frames, or tibbles with the same dimensions")
  }

  # Convert all elements to matrices
  matrix_list <- lapply(initial_list, as.matrix)

  # Initialize vectors to store means and standard deviations
  num_elements <- ncol(matrix_list[[1]]) * nrow(matrix_list[[1]])
  mean_vector <- numeric(num_elements)
  sd_vector <- numeric(num_elements)

  # Calculate mean and sd for each element position
  for (idx in seq_len(num_elements)) {
    elements <- sapply(matrix_list, function(m) c(m)[idx])
    mean_vector[idx] <- mean(elements, na.rm = TRUE)
    sd_vector[idx] <- stats::sd(elements, na.rm = TRUE)
  }

  # Calculate the Sharpe ratio
  sharpe_matrix <- matrix(mean_vector / sd_vector, nrow = nrow(matrix_list[[1]]), ncol = ncol(matrix_list[[1]]))

  # Return the Sharpe ratio matrix
  return(sharpe_matrix)
}
