#' Calculate Element-wise Median Across Multiple Matrices
#'
#' This function computes the median of corresponding elements from multiple matrices,
#' data frames, or tibbles. It is particularly useful for aggregating firm-level data
#' over time or across different datasets.
#'
#' @param ... One or more matrices, data frames, or tibbles containing firm-level data.
#' Each input must have the same dimensions.
#'
#' @return A matrix containing the element-wise medians of the corresponding elements
#' in the input matrices. The function automatically handles `NA` values by excluding
#' them from the median calculation. If all values at a specific position across all
#' matrices are `NA`, the result for that position will also be `NA`.
#'
#' @details The function combines all input matrices into a 3D array and calculates the
#' median across the third dimension. This makes it flexible for handling any number
#' of input matrices. It is designed to work seamlessly with data frames and tibbles
#' by converting them to matrices before processing.
#'
#' @export
#'
#' @examples
#' median_of_matrices(
#'   matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2),
#'   matrix(c(5, 6, 7, 8), nrow = 2, ncol = 2),
#'   matrix(c(1, 1, 1, 1), nrow = 2, ncol = 2)
#' )
#'
#' median_of_matrices(
#'   tibble::as_tibble(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)),
#'   tibble::as_tibble(matrix(c(5, 6, 7, 8), nrow = 2, ncol = 2)),
#'   tibble::as_tibble(matrix(c(1, 1, 1, 1), nrow = 2, ncol = 2))
#' )
median_of_matrices <- function(...) {

  matrices <- list(...)  # Capture all input matrices, data frames, and tibbles

  # Check that all inputs are either matrices, data frames, or tibbles
  if (!all(sapply(matrices, function(mat) {
    is.matrix(mat) || is.data.frame(mat) || tibble::is_tibble(mat)
  }))) {
    stop("All inputs must be matrices, data.frames, or tibbles.")
  }

  # Convert data frames and tibbles to matrices
  matrices <- lapply(matrices, function(mat) {
    if (is.data.frame(mat) || tibble::is_tibble(mat)) {
      return(as.matrix(mat))
    }
    return(mat)
  })

  # Check if all matrices have the same dimensions
  dims <- lapply(matrices, dim)
  if (!all(sapply(dims, function(x) identical(x, dims[[1]])))) {
    stop("All input matrices must have the same dimensions.")
  }

  # Combine the matrices into a 3D array
  combined <- array(unlist(matrices), dim = c(dims[[1]], length(matrices)))

  # Calculate the median across the third dimension
  calculated_medians <- apply(combined, c(1, 2), median, na.rm = TRUE)

  return(calculated_medians)
}
