#' Financial Cias Adjust Financials
#'
#' This function adjusts characteristics for specific financial subsectors for which that characteristic is not defined, replacing it 
#' based on a provided matrix of data available for those subsectors.
#'
#' @param characteristic_matrix A matrix of financial characteristics, where rows represent observations and columns represent variables.
#' @param subsector_classification A matrix indicating the subsector classification of each observation. Must have the same dimensions as \code{characteristic_matrix}.
#' @param characteristic_financial_cia_matrix A matrix of financial characteristics specific to certain subsectors, where rows represent observations and columns represent variables. Must have the same dimensions as \code{characteristic_matrix}.
#' @param subsectors_to_adjust A vector of subsector labels for which the financial characteristics will be adjusted.
#'
#' @return A matrix of adjusted financial characteristics.
#' @export
#'
#' @details
#' This function is useful for when a specific variable is not defined for sectors in subsectors_to_adjust (eg. ebit/inv_cap). If that is the case, it will replace the value by the corresponding value in 
#' characteristic_financial_cia_matrix (eg. roe)
#' 
#' @examples
#' \dontrun{
#' # Create example matrices
#' characteristic_matrix <- matrix(c(10, 20, NA, 30, 40, 50), nrow = 3, ncol = 2)
#' subsector_classification <- matrix(c("A", "B", NA, "C", "A", "B"), nrow = 3, ncol = 2)
#' characteristic_financial_cia_matrix <- matrix(c(15, 25, 35, 45, 55, 65), nrow = 3, ncol = 2)
#' subsectors_to_adjust <- c("A", "B")
#' 
#' # Adjust financial characteristics
#' adjusted_matrix <- financial_cia_adjust(characteristic_matrix, subsector_classification, 
#' characteristic_financial_cia_matrix, subsectors_to_adjust)
#' }
#' 
#' 
#' @seealso
#' \code{\link{fin_ratio}}
financial_cia_adjust <- function(characteristic_matrix, subsector_classification, characteristic_financial_cia_matrix, subsectors_to_adjust){
  #Check dimensions
  if (!all(dim(characteristic_matrix) == dim(subsector_classification) & dim(characteristic_matrix) == dim(characteristic_financial_cia_matrix))) {
    stop("Input matrices must have the same dimensions.")
  } else {
    #Init mtrix
    financial_cia_adjusted_characteristic_matrix <- matrix(NA, nrow = nrow(characteristic_matrix), ncol = ncol(characteristic_matrix))
    for(i in 1:nrow(characteristic_matrix)){
      for(j in 1:ncol(characteristic_matrix)){
        #In case the observation belongs to a sector in which the original characteristic (characteristic_matrix) is undefined, use a substitute (characteristic_financial_cia_matrix)
        if(is.na(subsector_classification[i,j]) == FALSE & subsector_classification[i,j] %in% subsectors_to_adjust){
          financial_cia_adjusted_characteristic_matrix[i,j] <- characteristic_financial_cia_matrix[i,j]
        #Keep the variable intact
          } else {
          financial_cia_adjusted_characteristic_matrix[i,j] <- characteristic_matrix[i,j]
        }
      }
    }
    #Return  matrix
    return(financial_cia_adjusted_characteristic_matrix)
  }
}