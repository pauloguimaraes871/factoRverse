#' Score Matrix Generation
#'
#' This function generates a score matrix based on input matrices or data frames.
#'
#' @param initial_list A list of matrices, data frames, or tibbles with the same dimensions.
#'
#' @return A score matrix.
#' @export
#'
#' @examples
#' # Create example matrices
#' mat1 <- matrix(c(1, 2, 3, 4), nrow = 2)
#' mat2 <- matrix(c(5, 6, 7, 8), nrow = 2)
#' mat_list <- list(mat1, mat2)
#' # Generate score matrix
#' score_matrix(mat_list)
score_matrix <- function(initial_list) {
  # Check if the input is a list of valid classes and transform to matrices
  if (!is.list(initial_list) ||
      !all(vapply(initial_list, function(x) is.matrix(x) || is.data.frame(x) || tibble::is_tibble(x), logical(1))) ||
      length(unique(sapply(initial_list, nrow))) != 1 ||
      length(unique(sapply(initial_list, ncol))) != 1) {
    stop("Input must be a list of matrices, data.frames, or tibbles with the same dimensions")
  }

  # Convert all elements to matrices
  matrix_list <- lapply(initial_list, function(x) as.matrix(x))

  # Initialize a score vector
  scores <- vector("list", nrow(matrix_list[[1]]) * ncol(matrix_list[[1]]))

  # Calculate scores for each element position
  for (idx in seq_along(scores)) {
    elements <- sapply(matrix_list, function(m) c(m)[idx])
    if (all(is.na(elements))) {
      scores[[idx]] <- NA
    } else {
      scores[[idx]] <- sum(ifelse(elements > 0, 1, ifelse(elements == 0, 0, -1)), na.rm = TRUE)
    }
  }

  # Create the final score matrix
  score_matrix <- matrix(unlist(scores), nrow = nrow(matrix_list[[1]]), ncol = ncol(matrix_list[[1]]))

  # Return the final matrix
  return(score_matrix)
}
