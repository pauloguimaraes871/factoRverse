#' Industry Weighted Mean
#'
#' This function calculates the industry weighted mean of characteristics for each sector.
#'
#' @param characteristic_matrix A matrix, data.frame, or tibble containing characteristics.
#' @param sector_classification A matrix, data.frame, or tibble containing sector classifications.
#' @param weighting_matrix A matrix, data.frame, or tibble containing weighting variables.
#' @param ew A binary indicator (0 or 1) indicating whether to use equal weights (ew = 1) or weighted means (ew = 0).
#'
#' @return A matrix of industry weighted means.
#' @export
#'
#' @examples
#' # Example usage:
#' # industry_weighted_mean(characteristic_matrix, sector_classification, weighting_matrix, ew = 0)
#'
#' @details
#' When elements in the weighting matrix are missing, they are set to zero.
#' When there is NA in sector classification, the characteristic value is ignored.
#'
industry_weighted_mean <- function(characteristic_matrix, sector_classification, weighting_matrix, ew){
  sectors <- characteristic <- weighting_variable <- mean_by_sector <- NULL

  # Check that all inputs are either matrices, data frames, or tibbles
  if (!all(sapply(list(characteristic_matrix, sector_classification, weighting_matrix), function(mat) {
    is.matrix(mat) || is.data.frame(mat) || tibble::is_tibble(mat)
  }))) {
    stop("All inputs must be matrices, data.frames, or tibbles.")
  }

  # Convert data frames and tibbles to matrices
  matrices <- lapply(list(characteristic_matrix, sector_classification, weighting_matrix), function(mat) {
    if (is.data.frame(mat) || tibble::is_tibble(mat)) {
      return(as.matrix(mat))
    }
    return(mat)
  })

  # Extract matrices after conversion
  characteristic_matrix <- matrices[[1]]
  sector_classification <- matrices[[2]]
  weighting_matrix <- matrices[[3]]

  #Check dimensions
  if (!all(dim(characteristic_matrix) == dim(sector_classification) & dim(characteristic_matrix) == dim(weighting_matrix))) {
    stop("Input matrices must have the same dimensions.")
  }

  # Initialize matrix for industry weighted means
    industry_weighted_mean_matrix <- matrix(NA, nrow = nrow(characteristic_matrix), ncol = ncol(characteristic_matrix)) #Init matrix
      for(j in 1:ncol(characteristic_matrix)){
        #Join everything
        full_j_matrix <- data.frame(sectors = sector_classification[,j],
                                   characteristic = characteristic_matrix[,j],
                                   weighting_variable = weighting_matrix[,j])

        #Weighting variable should be strictly positive
        if(any(full_j_matrix$weighting_variable[which(!is.na(full_j_matrix$weighting_variable))] <= 0)){
          stop("Weighting matrix should be strictly positive")
        }

        full_j_matrix$characteristic <- as.numeric(full_j_matrix$characteristic)  #coerce to numeric
        full_j_matrix$weighting_variable <- as.numeric(full_j_matrix$weighting_variable)  #coerce to numeric
        #When there is no information in weighting variable, assume weight to be zero.
        full_j_matrix$weighting_variable[which(is.na(full_j_matrix$weighting_variable))] <- 0 #Change NA in Weight to Zero
        #When sector is NA, change characteristic also to NA
        full_j_matrix$characteristic[which(is.na(full_j_matrix$sectors))] <- NA #Change NA in Weight to Zero


        #Checks if EW == 1
        if(ew == 1){

          #Calculate sector mean by group
        sector_mean <- full_j_matrix %>%
          dplyr::group_by(sectors) %>%
          dplyr::summarize(mean_by_sector = mean(x = characteristic,na.rm = TRUE))
        } else {
          #Calculate sector weighted_mean according to weighting_variable
          sector_mean <- full_j_matrix %>%
            dplyr::group_by(sectors) %>%
            dplyr::summarize(mean_by_sector = stats::weighted.mean(x = characteristic,
                                                                   w = weighting_variable,na.rm = TRUE))
        }
          #Place in column space
        industry_weighted_mean_matrix[,j] <- (dplyr::left_join(full_j_matrix, sector_mean, by = "sectors") %>% dplyr::select(mean_by_sector))[,1] %>% as.numeric()
      }
    #Return  matrix
    return(industry_weighted_mean_matrix)
}
