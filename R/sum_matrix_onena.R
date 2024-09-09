#' Sum Two Matrices with NA Handling
#'
#' Computes the element-wise sum of two matrices while handling missing values (NA).
#' 
#' This function takes two matrices of the same dimensions and returns another matrix where each element is the sum of the corresponding elements in the input matrices. Missing values (NA) are handled appropriately, so if an element is missing in one matrix but present in the other, it is treated as if it were zero for the purpose of addition.
#'
#' @param matrix1 A numeric matrix.
#' @param matrix2 A numeric matrix with the same dimensions as matrix1.
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
sum_matrix_onena <- function(matrix1, matrix2){ 
  if(!all(dim(matrix1) == dim(matrix2))) {
    stop("Input matrices must have the same dimensions.")
  } else {
    sum_matrix <- matrix(NA, nrow = nrow(matrix1), ncol = (ncol(matrix1)))
    for(i in 1:(nrow(matrix1))){
      for(j in 1:(ncol(matrix1))){
        if(is.na(matrix1[i,j]) == FALSE & is.na(matrix2[i,j]) == FALSE){
          sum_matrix[i,j] <- matrix1[i,j] + matrix2[i,j]
        } else {
          if(is.na(matrix1[i,j]) == TRUE & is.na(matrix2[i,j]) == FALSE){
            sum_matrix[i,j] <- matrix2[i,j]
          } else {
            if(is.na(matrix1[i,j]) == FALSE & is.na(matrix2[i,j]) == TRUE){
              sum_matrix[i,j] <- matrix1[i,j]
            } else {
              if(is.na(matrix1[i,j]) == TRUE & is.na(matrix2[i,j]) == TRUE){
                sum_matrix[i,j] <- NA
              }
            }
          }
        }
      }
    }
    return(sum_matrix)
  }
}
