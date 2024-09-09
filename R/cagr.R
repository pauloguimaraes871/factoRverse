##CAGR
#' Flexible function for calculating Compound Annual Growth Rate (CAGR) with handling for various scenarios, including negative values and NAs 
#'
#' @param begin Initial Value
#' @param final Final Value
#' @param period Number of periods
#'
#' @return A numeric value
#' 
#' @export
#'
#' @examples
#' 
#' cagr(1,2,1)
#' 
cagr <- function(begin, final, period){
  if (period <= 0) { #Checks if period is equal or less than zero
    stop("Period must be greater than zero.")
  }
  
    if(is.na(begin) == TRUE | is.na(final) == TRUE){ #Checks for NAs
    calculated_cagr <- NA
    
  } else {
    
    if(!is.numeric(begin) | !is.numeric(begin)){
      stop("Inputs are not numeric")
    }
    
    #Calculate according to inputs
    if(final >= 0 & begin >= 0){ 
      calculated_cagr <- (final/begin)^(1/period) - 1
    }
    if(final <= 0 & begin <= 0){
      calculated_cagr <- (-1) * ((abs(final)/abs(begin))^(1/period) - 1)
    }
    if(final >= 0 & begin <= 0){
      calculated_cagr <- ((final + 2 * abs(begin))/abs(begin))^(1/period) - 1
    }
    if(final <= 0 & begin >= 0){
      calculated_cagr <- (-1) * (((abs(final) + 2 * begin)/begin)^(1/period) - 1)
    }
    return(calculated_cagr)
  }
}