#' Fill Financial Cias Missing Data
#'
#' This function fills missing data in a panel data matrix, specifically for banks and other financial companies.
#'
#' @param features_matrix A data frame containing panel data with columns for "id", "tickers", "dates", "segment", and other features.
#' @param dates_vector A vector of dates for which missing data filling is performed.
#' @param segments_to_adjust A character vector specifying the segments (e.g., banks) for which missing data should be filled.
#' @param features_to_preserve A character vector specifying features that should not have missing data filled.
#'
#' @return A panel data matrix with missing data filled.
#'
#' @details This function fills missing data in the panel data matrix for the specified segments. It calculates the mean for each feature across all companies (including non-banks) and fills missing values in bank segments with the calculated mean. 
#' Features listed in \code{features_to_preserve} will not be filled even if they have missing values. Features that have missing values for all banks (but not for non-bank companies) are not filled, as it's assumed they require specific treatment.
#'
#' @examples
#' # Example usage
#' features_matrix <- data.frame(
#'   id = c(1, 2, 3, 4, 5),
#'   tickers = c("A", "B", "C", "D", "E"),
#'   dates = as.Date(c("2022-01-01", "2022-01-01", "2022-01-02", "2022-01-02", "2022-01-03")),
#'   segment = c("Bank", "Bank", "Other", "Bank", "Other"),
#'   feature1 = c(1, 2, NA, 4, 5),
#'   feature2 = c(NA, 6, NA, 8, NA)
#' )
#' dates_vector <- as.Date(c("2022-01-01", "2022-01-02", "2022-01-03" ))
#' segments_to_adjust <- c("Bank")
#' filled_data <- financialcia_fill_panel(features_matrix, dates_vector, 
#' segments_to_adjust, features_to_preserve = c("tickers", "dates"))
#'
#' @export
#'
financialcia_fill_panel <- function(features_matrix, dates_vector, segments_to_adjust, features_to_preserve = NA){
  #Check for correct format in features_matrix
  if(!all(c("id", "tickers", "dates") %in% colnames(features_matrix))){
    stop("features_matrix should have id, tickers and dates columns.")
  } else {}
  #Check there is a segment column. A segment column is necessary, because it provides granular information about banks. sector information will treat as "Financial Intermediaries", including both banks and insurance cias.
  if(!(c("segment") %in% colnames(features_matrix))){
    stop("there must be a segment column in features_matrix")
  } else {}
  #Check structure of dates_vector and features_matrix$dates
  if(!all(as.character(dates_vector) %in% unique(as.character(features_matrix$dates))) ||
     !all(unique(as.character(features_matrix$dates)) %in% as.character(dates_vector))){
    stop("all dates in dates_vector must have a correspondence in features_matrix")
  } else {}
    bank_panel <- features_matrix
    for (d in 1:length(dates_vector)) {
      #Subset based on dates
      subset_matrix_ref <- which(as.character(features_matrix$dates) == as.character(dates_vector[d]))
      subset_matrix <- features_matrix[subset_matrix_ref, ] 
      colnames(subset_matrix) <- colnames(features_matrix)#Change colanmes
      
      #Select banks and other financial cias inside subset (segments to adjust)
      banks_ref <- which(subset_matrix$segment %in%  segments_to_adjust) 
      subset_banks <- subset_matrix[banks_ref,] #Subset banks 
      colnames(subset_banks) <- colnames(features_matrix)#Change colanmes
      
      for (j in 1:ncol(features_matrix)) {
         if (class(features_matrix[, j]) %in% c("factor", "character", "Date") ||
            colnames(features_matrix)[j] %in% features_to_preserve ||
            #Features that have NAs for all banks (only banks) should not be replaced by subset means. It is better to handle each specifically, based on similarity to other features.
            (all(is.na(features_matrix[which(features_matrix$segment %in% segments_to_adjust),j])) &
            !all(is.na(features_matrix[,j])))) {
          # If the column is a factor, character, date or is in features_to_preserve, skip 
          subset_banks[, j] <- subset_banks[, j]
        } else {
          for(i in 1:nrow(subset_banks)){
            #Take mean of cias in subset
            subset_mean <- mean(subset_matrix[,j], na.rm = TRUE)
            #It there is a NA in subset_banks, replace by subset_mean. Otherwise, keep it as is.
            subset_banks[i,j] <- ifelse(is.na(subset_banks[i,j]), subset_mean, subset_banks[i,j])
          }
        }
        subset_matrix[banks_ref, j] <- subset_banks[,j]
        bank_panel[subset_matrix_ref, j] <- subset_matrix[,j]
      }
    }
    return(bank_panel)

}