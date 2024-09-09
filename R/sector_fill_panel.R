#' Sector Fill Panel
#'
#' This function fills missing values in a features dataframe based on sector-wise means for each date.
#'
#' @param features_df A data frame containing the features with columns for "id", "tickers" and "dates".
#' @param dates_vector A factor or Date vector containing dates corresponding to rows in \code{features_df}.
#' @param industry_classification_column_name The name of the column in \code{features_df} containing industry classifications.
#' @param features_to_preserve (Optional) A character vector containing names of features to preserve without filling missing values.
#'
#' @return A data frame with missing values filled based on sector-wise means for each date.
#'
#' @examples
#' features_df <- data.frame(id = 1:5,
#'                            tickers = c("AAPL", "GOOG", "MSFT", "AMZN", "FB"),
#'                            dates = as.Date(c("2022-01-01", "2022-01-01", 
#'                            "2022-01-02", "2022-01-02", "2022-01-03")),
#'                            industry = c("Tech", "Tech", "Finance", "Finance", "Tech"),
#'                            feature1 = c(NA, 2, NA, 4, 5),
#'                            feature2 = c(1, NA, NA, 3, NA))
#' dates_vector <- factor(as.character(features_df$dates))
#' sector_fill_panel(features_df, dates_vector, "industry")
#'
#' @export
sector_fill_panel <- function(features_df, dates_vector, industry_classification_column_name, features_to_preserve = NA){
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
  if(!is.character(industry_classification_column_name)){
    stop("industry_classification_column_name must be a character.")
  } else {}
  #Check there is a segment column. A segment column is necessary, because it provides granular information about banks. sector information will treat as "Financial Intermediaries", including both banks and insurance cias.
  if(!(industry_classification_column_name %in% colnames(features_df))){
    stop("industry_classification_column is not present in features_df")
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
  
  dates_vector <- as.Date(dates_vector, format = "%Y-%m-%d") #coerce to date
  sector_panel <- features_df
  for (d in 1:length(dates_vector)) {
    #Initialize objects
    #Subset based on dates
    subset_matrix_ref <- which(as.Date(features_df$dates, format = "%Y-%m-%d") == as.Date(dates_vector[d], format = "%Y-%m-%d"))
    subset_matrix <- features_df[subset_matrix_ref, ] 
    #Change colnames
    colnames(subset_matrix) <- colnames(features_df)
    
    #Industry classification object
    industry_classification_col_position <- which(colnames(sector_panel) == industry_classification_column_name) #Get sector classification column position
    
    #Loop for features
    for (j in 1:ncol(features_df)) {
      # If the column is a factor, character, date or is in features_to_preserve, skip 
      if (class(features_df[, j]) %in% c("factor", "character", "Date") ||
          colnames(features_df)[j] %in% features_to_preserve) {
        #In this case, do nothing
        subset_matrix[, j] <- subset_matrix[, j]
      } else {
        for(i in 1:nrow(subset_matrix)){
          #Get Sector Row Reference
          sector_ref <- which(subset_matrix[, industry_classification_col_position] == 
                              subset_matrix[i,industry_classification_col_position])
          #Take mean of cias in sector
          sector_mean <- mean(subset_matrix[sector_ref,j], na.rm = TRUE)
          #It there is a NA in subset_banks, replace by subset_mean. Otherwise, keep it as is.
          subset_matrix[i,j] <- ifelse(is.na(subset_matrix[i,j]), sector_mean, subset_matrix[i,j])
        }
      }
      #Replace in sector_panel
      sector_panel[subset_matrix_ref, j] <- subset_matrix[,j]
    }
  }
  return(sector_panel)
  
}