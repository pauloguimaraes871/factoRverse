#' Row-wise Skewness Rolling Calculation of a Matrix
#'
#' Calculates the row-wise skewness of elements in a matrix using a rolling window, which incorporates both lagged and current observations to assess the asymmetry of data distribution at each point.
#'
#' The function appends the complementary matrix (containing lagged observations) to the main matrix to create a complete matrix used for skewness calculation. The skewness for each element in the main matrix is calculated using a rolling window that spans across the corresponding columns in the complete matrix, starting from the lagged values and extending to include the current value.
#'
#' @param main_matrix A numeric matrix whose skewness needs to be calculated.
#'
#' @param complementary_matrix A numeric matrix containing lagged observations of the main matrix. This matrix is used to extend the data available for calculating skewness at the beginning of the main matrix. The number of columns in the complementary matrix defines the width of the rolling window used for calculating skewness.
#'
#' @return A numeric matrix with the same dimensions as main_matrix where each entry is the skewness calculated from a rolling window of observations that includes both the lagged and current data. By default, NAs are ignored.
#'
#' @examples
#' # Create a main matrix and a complementary matrix
#' main_matrix <- matrix(c(5, 3, 7, 8), nrow = 2, ncol = 2)
#' complementary_matrix <- matrix(c(1, 2, 6, 4), nrow = 2, ncol = 2)
#'
#' # Calculate skewness using a rolling window of 3 (1 current + 2 lagged observations)
#' skew_rolling(main_matrix, complementary_matrix)
#'
#' @export
skew_rolling <- function(main_matrix, complementary_matrix){
  if(nrow(main_matrix) != nrow(complementary_matrix)){
    stop("Main matrix and complementary_matrix should have same number of rows.")
  }
  #Size Complmenetary Matrix is the difference in #col by adding older data from lagged matrix
  skew_matrix <- matrix(NA, nrow = nrow(main_matrix), ncol = ncol(main_matrix)) #skew_matrix will have same dim as main_matrix
  complete_matrix <- cbind(complementary_matrix, main_matrix) #Join both matrices
  size_complementary_matrix <- ncol(complementary_matrix) #Size of complementary matrix
  
  for(i in 1:(nrow(main_matrix))){
    for(j in 1:(ncol(main_matrix))){

      past_values <- unlist(complete_matrix[i,j:(j+size_complementary_matrix)]) #consider most recent month
      skew_matrix[i,j] <- moments::skewness(past_values, na.rm = TRUE)
    }
  }
  return(skew_matrix)
}


