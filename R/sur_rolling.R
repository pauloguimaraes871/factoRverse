#' Row-wise Standardized Unexpected Realization (SUR) of a Matrix
#'
#' Calculates row-wise Standardized Unexpected Realization (SUR) of a matrix, which measures how unexpected each value in the matrix is, considering lagged realizations.
#'
#' @param main_matrix A matrix of characteristics.
#'                        
#' @param complementary_matrix A complementary matrix containing lagged information necessary to calculate SUR for first columns in main_matrix.  
#'                                  The number of columns of this matrix specifies how many previous observations should be considered in the SUR calculation.
#'                                  It determines the width of the rolling windows used for calculating mean and standard deviation.
#' @return A numeric matrix of row-wise SUR. Non-unique values are ignored in order to limit impact of repeated information, quite common when dealing with accounting information in monthly observations. By default, NAs are ignored.
#' @export
#' @examples
#' # Create a complete matrix with lagged information
#' main_matrix <- matrix(c(5,3,7,8), nrow = 2, ncol = 2)
#' complementary_matrix <- matrix(c(1,2,6,4), nrow = 2, ncol = 2) 
#'                              
#' # Calculate SUR using a rolling window of 3(1+2 additional columns)
#' sur_rolling(main_matrix, complementary_matrix)
sur_rolling <- function(main_matrix, complementary_matrix){
  if(nrow(main_matrix) != nrow(complementary_matrix)){
    stop("Main matrix and complementary_matrix should have same number of rows.")
  }
  #Size Complmenetary Matrix is the difference in #col by adding older data from lagged matrix
  sur_matrix <- matrix(NA, nrow = nrow(main_matrix), ncol = ncol(main_matrix)) #sur_matrix will have same dim as main_matrix
  complete_matrix <- cbind(complementary_matrix, main_matrix) #Join both matrices
  size_complementary_matrix <- ncol(complementary_matrix) #Size of complementary matrix
  for(i in 1:(nrow(main_matrix))){
    for(j in 1:(ncol(main_matrix))){

      past_values <- unlist(complete_matrix[i,j:(j+size_complementary_matrix)]) #consider most recent month in mean and sd for z-score
      sur_matrix[i,j] <- (complete_matrix[i,j+size_complementary_matrix] -  #Most recent data. Include 36m + 1 of data (includes more obs to compare)
                            base::mean(unique(past_values), na.rm = TRUE))/ #Repetitions represent counting same quarter more than once
        stats::sd(base::unique(past_values), na.rm = TRUE) #Standardized Earnings
    }
  }
  return(sur_matrix)
}
