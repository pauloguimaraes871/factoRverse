# Normalize Panel Data
#'
#' This function normalizes a panel data matrix, scaling each feature to a range of \code{-1} to \code{1}.
#'
#' @param features_df A data frame containing panel data with columns for "id", "tickers" and "dates".
#' @param dates_vector A vector of dates for which normalization is performed.
#'
#' @return A normalized panel data matrix.
#'
#' @details This function normalizes each feature in the panel data matrix to a range of \code{-1} to \code{1}. For each specified date in the dates_vector, it computes the minimum and maximum values for each feature and scales the values accordingly.
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
#' dates_vector <- as.Date(c("2022-01-01", "2022-01-02"))
#' normalized_data <- normalize_panel_data(features_df, dates_vector)
#'
#' @export
#'
normalize_panel_data <- function(features_df, dates_vector) {
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
  #Check for correct format in dates_vector
  if(any(is.na(strptime(dates_vector, format = "%Y-%m-%d"))) ||
     any(format(strptime(dates_vector, format = "%Y-%m-%d"), "%Y-%m-%d") != dates_vector)){
    stop("dates_vector must be a date object with format %Y-%m-%d")
  } else {}
  #Check structure of dates_vector and features_df$dates
  if(!all(as.Date(dates_vector, format = "%Y-%m-%d") %in% unique(as.Date(features_df$dates, format = "%Y-%m-%d"))) ||
     !all(unique(as.Date(features_df$dates, format = "%Y-%m-%d")) %in% as.Date(dates_vector, format = "%Y-%m-%d"))){
    stop("all dates in dates_vector must have a correspondence in features_df")
  } else {}
  normalized_matrix <- features_df
  dates_vector <- as.Date(dates_vector, format = "%Y-%m-%d") #Coerce dates
  for (d in 1:length(dates_vector)) {
      #Set subset
      subset_matrix_ref <- which(as.Date(features_df$dates, format = "%Y-%m-%d") == as.Date(dates_vector[d], format = "%Y-%m-%d"))
      subset_matrix <- features_df[subset_matrix_ref, ]
      for (j in 1:ncol(features_df)) {
        if (class(features_df[, j]) %in% c("factor", "character", "Date")) {
          # If the column is a factor, character, or Date, skip normalization
          subset_matrix[, j] <- subset_matrix[, j]
        } else {
          subset_min <- ifelse(all(is.na(subset_matrix[,j])), NA, min(subset_matrix[,j], na.rm = TRUE))
          subset_max <- ifelse(all(is.na(subset_matrix[,j])), NA, max(subset_matrix[,j], na.rm = TRUE))
          
          for(i in 1:nrow(subset_matrix)){
            subset_matrix[i,j] <- (2*(subset_matrix[i,j] - subset_min)/(subset_max - subset_min))-1
          }
        }
        normalized_matrix[subset_matrix_ref, j] <- subset_matrix[, j]
      }
    }
    return(normalized_matrix)
}