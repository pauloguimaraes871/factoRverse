#' Winsorize Panel Data
#'
#' Winsorizes a panel data matrix, replacing extreme values with specified quantiles.
#'
#' @param features_df A data frame containing panel data with columns for "id", "tickers" and "dates".
#' @param dates_vector A vector of dates for which winsorization is performed.
#' @param probs A vector of probabilities specifying the quantiles for winsorization (e.g., c(0.99, 0.01)).
#' @param Infs_to_preserve A vector of column names for features where infinite values should be replaced by max/min and not by NA.
#'
#' @return A winsorized panel data matrix.
#'
#' @details This function winsorizes a panel data matrix, replacing extreme values with specified quantiles. For each specified date in the dates_vector, it calculates the quantiles for each feature and replaces values above the upper quantile with the upper quantile and values below the lower quantile with the lower quantile. Infinite values are treated differently depending on whether they should be preserved or not.
#'
#' @examples
#' # Example usage
#' features_df <- data.frame(
#'   id = c(1, 2, 3, 4),
#'   tickers = c("A", "B", "C", "D"),
#'   dates = as.Date(c("2022-01-01", "2022-01-01", "2022-01-02", "2022-01-02")),
#'   feature1 = c(1, 2, 3, 4),
#'   feature2 = c(5, 6, 7, 8)
#' )
#' dates_vector <- as.factor(c("2022-01-01", "2022-01-02"))
#' probs <- c(0.05, 0.95)
#' winsorized_data <- winsorize_panel_data(features_df, dates_vector, probs)
#'
#' @export
#'
winsorize_panel_data <- function(features_df, dates_vector, probs, Infs_to_preserve = NULL) {
  #Check structure of features_df
  if(!all(c("id", "tickers", "dates") %in% colnames(features_df))){
    stop("features_df should have id, tickers and dates columns.")
  } else {}
  #Check formats
  if(!is.data.frame(features_df)){
    stop("features_df must be a data frame.")
  } else {}
  if(!is.factor(dates_vector) & !inherits(dates_vector,"Date")){
    stop("dates_vector must be factor or date.")
  } else {}
  if(!is.numeric(probs) || length(probs) != 2){
    stop("probs must be a numeric vector of length 2")
  } else {}
  
  #Check structure of dates_vector and features_df$dates
  if(!all(as.Date(dates_vector, format = "%Y-%m-%d") %in% unique(as.Date(features_df$dates, format = "%Y-%m-%d"))) ||
     !all(unique(as.Date(features_df$dates, format = "%Y-%m-%d")) %in% as.Date(dates_vector, format = "%Y-%m-%d"))){
    stop("all dates in dates_vector must have a correspondence in features_df")
  } else {}
  
  #Check for correct format in dates_vector
  if(any(is.na(strptime(dates_vector, format = "%Y-%m-%d"))) ||
     any(format(strptime(dates_vector, format = "%Y-%m-%d"), "%Y-%m-%d") != dates_vector)){
    stop("dates_vector must be a date object with format %Y-%m-%d")
  } else {}
  
  
    dates_vector <- as.Date(dates_vector, format = "%Y-%m-%d") #Coerce dates
    features_df <- as.data.frame(features_df)
    winsorized_matrix <- features_df
    for (d in 1:length(dates_vector)) {
      #Subset
      subset_matrix_ref <- which(as.Date(features_df$dates, format = "%Y-%m-%d") == as.Date(dates_vector[d], format = "%Y-%m-%d"))
      subset_matrix <- features_df[subset_matrix_ref, ]
      for (j in 1:ncol(features_df)) {
        if (class(features_df[, j]) %in% c("factor", "character", "Date")) {
          # If the column is a factor, character, or Date, skip winsorization
          subset_matrix[, j] <- subset_matrix[, j]
        } else {
          if(colnames(features_df)[j] %in% Infs_to_preserve){ #Should this variable preserve Infinite values?
            finite_ref <- which(is.finite(subset_matrix[,j])) #Who is finite?
            neg_infinite_ref <- which(is.infinite(subset_matrix[,j]) & subset_matrix[,j] < 0) #Who is neg infinite
            pos_infinite_ref <- which(is.infinite(subset_matrix[,j]) & subset_matrix[,j] > 0) #Who is pos infinite
            subset_matrix[neg_infinite_ref, j] <- ifelse(all(is.na(subset_matrix[finite_ref, j])), NA, 
                                                         min(subset_matrix[finite_ref, j], na.rm = TRUE)) #What is the highest max, excluding Infinites
            
            
            subset_matrix[pos_infinite_ref, j] <-ifelse(all(is.na(subset_matrix[finite_ref, j])), NA, 
                                                        max(subset_matrix[finite_ref, j], na.rm = TRUE)) #What is the highest max, excluding Infinites
            
          } else {
            infinite_ref <- which(is.finite(subset_matrix[,j]) == FALSE) #Who is finite?
            subset_matrix[infinite_ref, j] <- NA
          }
          #Calculate quantiles and see detect outliers
          subset_quantile_right <- stats::quantile(subset_matrix[, j], probs = max(probs), na.rm = TRUE)
          right_outliers_ref <- which(subset_matrix[, j] >= subset_quantile_right)
          
          subset_quantile_left <- stats::quantile(subset_matrix[, j], probs = min(probs), na.rm = TRUE)
          left_outliers_ref <- which(subset_matrix[, j] <= subset_quantile_left)
          
          subset_matrix[right_outliers_ref, j] <- subset_quantile_right
          subset_matrix[left_outliers_ref, j] <- subset_quantile_left
        }
        winsorized_matrix[subset_matrix_ref, j] <- subset_matrix[, j]
      }
    }
    return(winsorized_matrix)
}
