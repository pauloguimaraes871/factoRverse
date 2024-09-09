#' Sharpe Ratio Matrix
#'
#' This function calculates the Sharpe ratio matrix from a list of matrices or data frames.
#'
#' @param initial_list A list of matrices or data frames with the same dimension.
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
  # Check if the input is a list of matrices
  if (!is.list(initial_list) || !all(sapply(initial_list, is.data.frame) | sapply(initial_list, is.matrix)) || 
      length(unique(c(sapply(initial_list, dim)[1,]))) != 1 || length(unique(c(sapply(initial_list, dim)[2,]))) != 1
      ){
    stop("Input must be a list of matrices/data.frame with same dimension")
  } else {
    
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
    
    #initiate mean and sd vectors
    mean_vector <- vector(length = ncol(list_matrices[[1]])+nrow(list_matrices[[1]]))
    sd_vector <- vector(length = ncol(list_matrices[[1]])+nrow(list_matrices[[1]]))
    
    #Create vectors of i+j length in which each element correspond to mean/sd of all ij_th's elements of each matrix.  
    for(i_j in 1:(ncol(list_matrices[[1]])*nrow(list_matrices[[1]]))){
      mean_vector[i_j] <- mean(intermediary_list[[i_j]], na.rm = TRUE)
      sd_vector[i_j] <- stats::sd(intermediary_list[[i_j]], na.rm = TRUE)
    }
    
    #Deliver the final sharpe matrix
    sharpe_matrix <- matrix(mean_vector/sd_vector, nrow = nrow(list_matrices[[1]]), ncol = ncol(list_matrices[[1]]))
    
    #Return sharpe matrix
    return(sharpe_matrix)
  } 
}