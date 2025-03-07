#' Row-wise Skewness Rolling Calculation of a Matrix
#'
#' Calculates the row-wise skewness of elements in a matrix using a rolling window, which incorporates both lagged and current observations to assess the asymmetry of data distribution at each point.
#'
#' The function appends the complementary matrix (containing lagged observations) to the main matrix to create a complete matrix used for skewness calculation. The skewness for each element in the main matrix is calculated using a rolling window that spans across the corresponding columns in the complete matrix, starting from the lagged values and extending to include the current value.
#'
#' @param main_matrix A matrix, data frame, or tibble whose skewness needs to be calculated.
#' @param complementary_matrix A matrix, data frame, or tibble containing lagged observations of the main matrix. This matrix is used to extend the data available for calculating skewness at the beginning of the main matrix. The number of columns in the complementary matrix defines the width of the rolling window used for calculating skewness.
#'
#' @return A numeric matrix with the same dimensions as main_matrix where each entry is the skewness calculated from a rolling window of observations that includes both the lagged and current data. By default, NAs are ignored.
#'
#' @examples
#' # Create a main matrix and a complementary matrix
#' main_matrix <- matrix(c(5, 3, 7, 8), nrow = 2, ncol = 2)
#' complementary_matrix <- matrix(c(1, 2, 6, 4), nrow = 2, ncol = 2)
#'
#' # Calculate skewness using a rolling window
#' skew_rolling(main_matrix, complementary_matrix)
#'
#' @export
skew_rolling <- function(main_matrix, complementary_matrix) {
  # Check if inputs are data frames, tibbles, or matrices and convert to matrices
  if (!is.matrix(main_matrix) && !is.data.frame(main_matrix) && !tibble::is_tibble(main_matrix)) {
    stop("main_matrix must be a matrix, data frame, or tibble.")
  }
  if (!is.matrix(complementary_matrix) && !is.data.frame(complementary_matrix) && !inherits(complementary_matrix, "tbl_df")) {
    stop("complementary_matrix must be a matrix, data frame, or tibble.")
  }

  # Convert to matrices
  main_matrix <- as.matrix(main_matrix)
  complementary_matrix <- as.matrix(complementary_matrix)

  # Input validation for row counts
  if (nrow(main_matrix) != nrow(complementary_matrix)) {
    stop("Main matrix and complementary_matrix should have the same number of rows.")
  }

  # Initialize skewness matrix
  skew_matrix <- matrix(NA, nrow = nrow(main_matrix), ncol = ncol(main_matrix))

  # Create complete matrix by combining both matrices
  complete_matrix <- cbind(complementary_matrix, main_matrix)

  # Size of the complementary matrix
  size_complementary_matrix <- ncol(complementary_matrix)

  # Calculate skewness for each element in the main matrix
  for (i in seq_len(nrow(main_matrix))) {
    for (j in seq_len(ncol(main_matrix))) {
      # Extract past values for skewness calculation
      past_values <- complete_matrix[i, j:(j + size_complementary_matrix)]
      skew_matrix[i, j] <- moments::skewness(past_values, na.rm = TRUE)
    }
  }

  return(skew_matrix)
}

