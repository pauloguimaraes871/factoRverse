## Score Matrix Generation
#' This function generates a score matrix based on input matrices or data frames.
#'
#' @param initial_list A list of matrices or data frames with the same dimension.
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
  # Check if the input is a list of matrices
  if (!is.list(initial_list) || 
      !all(sapply(initial_list, is.data.frame) | sapply(initial_list, is.matrix)) ||
      length(unique(c(sapply(initial_list, dim)[1,]))) != 1 || length(unique(c(sapply(initial_list, dim)[2,]))) != 1 
      ){
    stop("Input must be a list of matrices/data.frame with same dimension")
  }
  
  #initiate intermediary list
  list_matrices <- lapply(initial_list, as.matrix)
  intermediary_list <- list()
  #Each element of the list will be a vector corresponding to ij_th element of each matrix in the list
  for(i_j in 1:(ncol(list_matrices[[1]])*nrow(list_matrices[[1]]))){
    intermediary_list[[i_j]] <- vector(length=length(list_matrices))
    for(l in 1:length(list_matrices)){
      intermediary_list[[i_j]][l] <- c(list_matrices[[l]])[i_j]
    }
  }
  
  #Creta final list, applying ifelse to vector
  final_list <- list()
  for(i_j in 1:length(intermediary_list)){
    if(all(is.na(intermediary_list[[i_j]]))){
      final_list[[i_j]] <- NA
    } else {
      final_list[[i_j]] <- sum(ifelse(intermediary_list[[i_j]] > 0, 1, 
                                      ifelse(intermediary_list[[i_j]] == 0, 0, -1)), na.rm = TRUE)
    }
  }
  
  #Final score matrix
  score_matrix <- matrix(unlist(final_list), nrow = nrow(list_matrices[[1]]), ncol = ncol(list_matrices[[1]]))
  
  #Return final matrix
  return(score_matrix)
  
}