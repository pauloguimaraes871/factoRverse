#' Fill Panel Data
#'
#' This function fills missing values in a panel data set by replacing them with the mean of the respective feature across similar entities and time periods.
#'
#' @param features_df A data frame containing the panel data with columns "id", "tickers", "dates", and features to be filled.
#' @param dates_vector A vector of dates or factors representing the time periods of the panel data.
#' @param features_to_preserve A vector of column names of features that should not be filled.
#'
#' @return A data frame with missing values filled by the mean of each feature across similar entities and time periods.
#' @export
#'
#' @examples
#' # Create sample data frame
#' features_df <- data.frame(id = c(1, 1, 2, 2),
#' tickers = c("A", "A", "B", "B"),
#' dates = as.Date(c("2022-01-01", "2022-01-02", "2022-01-01", "2022-01-02")),
#' feature1 = c(1, NA, 3, 4),
#' feature2 = c(NA, 2, 3, NA))
#'
#' # Define dates vector
#' dates_vector <- as.Date(c("2022-01-01", "2022-01-02"))
#'
#' # Fill missing values
#' filled_panel <- all_fill_panel(features_df, dates_vector, 
#' features_to_preserve = c("id", "tickers", "dates"))
all_fill_panel <- function(features_df, dates_vector, features_to_preserve = NA){
  #Check for correct format in features_df
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
  
  filled_panel <- features_df
  dates_vector <- as.Date(dates_vector, format = "%Y-%m-%d") #Coerce dates
  
  for (d in 1:length(dates_vector)) {
    #Initialize objects
    #Subset based on dates
    subset_matrix_ref <- which(as.Date(features_df$dates, format = "%Y-%m-%d") == as.Date(dates_vector[d], format = "%Y-%m-%d"))
    subset_matrix <- features_df[subset_matrix_ref, ] 
    #Change colnames
    colnames(subset_matrix) <- colnames(features_df)
    
    #Loop for features
    for (j in 1:ncol(features_df)) {
      # If the column is a factor, character, date or is in features_to_preserve, skip 
      if (class(features_df[, j]) %in% c("factor", "character", "Date") ||
          colnames(features_df)[j] %in% features_to_preserve) {
        #In this case, do nothing
        subset_matrix[, j] <- subset_matrix[, j]
      } else {
        for(i in 1:nrow(subset_matrix)){
          #Take mean of cias in
          subset_mean <- mean(subset_matrix[,j], na.rm = TRUE)
          #It there is a NA in subset_banks, replace by subset_mean. Otherwise, keep it as is.
          subset_matrix[i,j] <- ifelse(is.na(subset_matrix[i,j]), subset_mean, subset_matrix[i,j])
        }
      }
      #Replace in filled_panel
      filled_panel[subset_matrix_ref, j] <- subset_matrix[,j]
    }
  }
  return(filled_panel)
  
}