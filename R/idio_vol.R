#' Calculate Element-Wise Idiosyncratic Volatility
#'
#' Calculates  idiosyncratic volatility of assets given their total volatility, benchmark volatility, and beta values.
#'
#' This function computes the idiosyncratic volatility for each asset based on their total volatility, benchmark volatility, and beta values. Idiosyncratic volatility represents the portion of an asset's total volatility that is not explained by the volatility of the benchmark index. It is computed as the square root of the difference between the squared total volatility of the asset and the product of the squared benchmark volatility and the squared beta value.
#' 
#' @param vol_assets A numeric matrix representing the total volatilities of assets. Each row corresponds to an asset, and each column represents a time period.
#' @param vol_bench A numeric matrix representing the volatilities of the benchmark index. Each column corresponds to a time period.
#' @param beta_bench A numeric matrix representing the beta values of assets with respect to the benchmark index.
#'                   Each row corresponds to an asset, and each column represents a time period.
#'                   By default, Infs are interpreted as mistakes in data and, thus, set results to NA.
#' @return A numeric matrix containing the idiosyncratic volatilities of assets.
#' @examples
#' # Example data
#' vol_assets <- matrix(c(0.1, 0.2, 0.15, 0.25), nrow = 2)
#' vol_bench <- matrix(c(0.05, 0.06), nrow = 1)
#' beta_bench <- matrix(c(0.8, 1.2, 0.9, 1.1), nrow = 2)
#' 
#' # Calculate idiosyncratic volatility
#' idio_volatility <- idio_vol(vol_assets, vol_bench, beta_bench)
#' idio_volatility
#' 
#' @export
idio_vol <- function(vol_assets, vol_bench, beta_bench){
  if(!all(dim(vol_assets) == dim(beta_bench)) | #Check dimensions
     ncol(vol_assets) != ncol(vol_bench)){
    stop("Objects don't have compatible dimensions.")
  } else {}
  if(nrow(vol_bench) > 1 ){
    stop("vol_bench nrow > 1")
  } else {}
  idio_matrix <- matrix(NA, nrow = nrow(vol_assets), ncol = (ncol(vol_assets))) #Initialize matrix
  idio_var <- matrix(NA, nrow = nrow(vol_assets), ncol = (ncol(vol_assets))) #Initialize matrix
  for(i in 1:nrow(vol_assets)){
    for(j in 1:ncol(vol_assets)){
      if(vol_bench[,j] <= 0){
        stop("Benchmark volatility is probably wrong") #This is probably a mistake in the database
      } else {}

      idio_var[i,j] <- (vol_assets[i,j]^2) - (beta_bench[i,j]^2*vol_bench[,j]^2) #Calculate idiosyncrativ variance
      if(idio_var[i,j] < 0 || 
         is.na(idio_var[i,j]) || 
         vol_assets[i,j] <= 0){ #check if there is resulting negative value for idio_var, for which there is no real sqrt, or vol_assets
        idio_matrix[i,j] <- NA #If there is negative sqrt, assign NA
      } else {
        idio_matrix[i,j] <- sqrt(idio_var[i,j])
      }
    }
  }
  return(idio_matrix)
}


