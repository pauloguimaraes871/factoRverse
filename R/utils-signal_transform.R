#' Signal Transformation Function
#'
#' This function performs a sequence of transformations on a numeric vector:
#' - Winsorizes the values based on specified quantiles.
#' - Computes the z-score of the winsorized values.
#' - Transforms the z-scores into a new vector based on their sign.
#'
#' @param vector A numeric vector to be transformed.
#' @param upper_quantile_winsorization A numeric value between 0 and 1 specifying the quantile threshold for upper winsorization.
#' @param lower_quantile_winsorization A numeric value between 0 and 1 specifying the quantile threshold for lower winsorization.
#'
#' @return A numeric vector of the same length as `vector` with transformed values.
#'
#' @details
#' The function first applies winsorization to the \code{vector} based on the provided quantile thresholds.
#' Values exceeding the upper quantile are replaced with the upper quantile value, and values below the lower quantile are replaced with the lower quantile value.
#' The function then computes the z-scores of the winsorized values.
#' Finally, it transforms these z-scores:
#' - Positive z-scores are adjusted to \eqn{1 + Z}
#' - Negative z-scores are transformed to \eqn{\frac{1}{1 - Z}}
#' - A z-score of zero is transformed to 1#'
#' @examples
#' vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' upper_quantile_winsorization <- 0.9
#' lower_quantile_winsorization <- 0.1
#' transformed_vector <- signal_transform(vector, upper_quantile_winsorization, lower_quantile_winsorization)
#' print(transformed_vector)
#'
#' @export
signal_transform <- function(vector, lower_quantile_winsorization = 0.05, upper_quantile_winsorization = 0.95){

  #Check if quantiles are correct and adjust if needed
  if(upper_quantile_winsorization <= lower_quantile_winsorization){
    #Swap quantiles
    adj_lower_quantile_winsorization <- upper_quantile_winsorization
    adj_upper_quantile_winsorization <- lower_quantile_winsorization
    lower_quantile_winsorization <- adj_lower_quantile_winsorization
    upper_quantile_winsorization <- adj_upper_quantile_winsorization

    warning("The lower quantile threshold was higher than the upper quantile threshold. The quantiles have been swapped.")
  }

  #If all NAs, return a vector of NAs
  if(all(is.na(vector))){
    return(rep(NA, length(vector)))
  }

  #If single value, return 1
  if(length(vector) == 1){
    return(1)
  }

  #Calculate quantiles for winsorization
  quantile_upper <- quantile(vector, upper_quantile_winsorization, na.rm = TRUE) #calculate quantile
  quantile_lower <- quantile(vector, lower_quantile_winsorization, na.rm = TRUE) #calculate quantile

  #Winsorize
  winsorized_vector <- ifelse(vector >= quantile_upper, #If upper quantile
                              quantile_upper, #winsorize
                              ifelse(vector <= quantile_lower,
                                     quantile_lower, #winsorizer
                                     vector)) #do nothing
  #Zscore
  if(sd(winsorized_vector, na.rm = TRUE) != 0){
    zscore_vector <- (winsorized_vector - mean(winsorized_vector, na.rm = TRUE))/sd(winsorized_vector, na.rm = TRUE)
  } else {
    zscore_vector <- (winsorized_vector - mean(winsorized_vector, na.rm = TRUE))
  }

  #Transformed
  transformed_vector <- ifelse(zscore_vector > 0,
                               1 + zscore_vector, #Z>0 -> 1+Z
                               ifelse(zscore_vector < 0,
                                      1/(1-zscore_vector), #Z<0 -> 1/1-Z
                                      1 #Z = 0 -> 1
                               )
  )

  return(transformed_vector)
}
