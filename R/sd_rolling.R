#' Row-wise Standard Deviation
#'
#' Calculates the row-wise Standard Deviation (SD) of a matrix, considering lagged realizations.
#'
#' @param main_matrix A matrix, data.frame, or tibble of characteristics.
#' @param complementary_matrix A matrix, data.frame, or tibble containing lagged information necessary
#'                             to calculate SD for the first columns in main_matrix. The number of columns
#'                             of this matrix specifies how many previous observations should be considered
#'                             in the SD calculation, determining the width of the rolling windows used.
#'
#' @return A numeric matrix of row-wise SD. Non-unique values are ignored to limit the impact of repeated
#'         information, quite common when dealing with accounting information in monthly observations. By
#'         default, NAs are ignored.
#' @export
#'
#' @examples
#' # Create a complete matrix with lagged information
#' main_matrix <- matrix(c(5, 3, 7, 8), nrow = 2, ncol = 2)
#' complementary_matrix <- matrix(c(1, 2, 6, 4), nrow = 2, ncol = 2)
#'
#' # Calculate SD using a rolling window of 3 (1 + 2 additional columns)
#' sd_rolling(main_matrix, complementary_matrix)
sd_rolling <- function(main_matrix, complementary_matrix) {
  # Check that all inputs are either matrices, data frames, or tibbles
  if (!all(sapply(list(main_matrix, complementary_matrix), function(mat) {
    is.matrix(mat) || is.data.frame(mat) || tibble::is_tibble(mat)
  }))) {
    stop("Both main_matrix and complementary_matrix must be matrices, data.frames, or tibbles.")
  }

  # Convert data frames and tibbles to matrices
  matrices <- lapply(list(main_matrix, complementary_matrix), function(mat) {
    if (is.data.frame(mat) || tibble::is_tibble(mat)) {
      return(as.matrix(mat))
    }
    return(mat)
  })

  # Extract matrices after conversion
  main_matrix <- matrices[[1]]
  complementary_matrix <- matrices[[2]]

  # Check if the matrices have the same number of rows
  if (nrow(main_matrix) != nrow(complementary_matrix)) {
    stop("Main matrix and complementary_matrix should have the same number of rows.")
  }

  # Initialize SD matrix
  sd_matrix <- matrix(NA, nrow = nrow(main_matrix), ncol = ncol(main_matrix))
  complete_matrix <- cbind(complementary_matrix, main_matrix) # Join both matrices
  size_complementary_matrix <- ncol(complementary_matrix) # Size of complementary matrix

  for (i in 1:nrow(main_matrix)) {
    for (j in 1:ncol(main_matrix)) {
      past_values <- unlist(complete_matrix[i, j:(j + size_complementary_matrix)]) # Consider most recent month
      sd_matrix[i, j] <- stats::sd(base::unique(past_values), na.rm = TRUE) # Calculate SD
    }
  }

  return(sd_matrix)
}
