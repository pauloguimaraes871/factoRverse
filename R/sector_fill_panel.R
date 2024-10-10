#' Sector Fill Panel
#'
#' This function fills missing values in a features dataframe based on sector-wise means for each date.
#'
#' @param features_m_df A meta dataframe or coercible object containing features with columns for "id", "tickers" and "dates".
#' @param industry_classification_column_name The name of the column in \code{features_m_df} containing industry classifications.
#' @param features_to_preserve (Optional) A character vector containing names of features to preserve without filling missing values.
#'
#' @return A data frame with missing values filled based on sector-wise means for each date.
#'
#' @examples
#' features_m_df <- data.frame(id = 1:5,
#'                            tickers = c("AAPL", "GOOG", "MSFT", "AMZN", "FB"),
#'                            dates = as.Date(c("2022-01-01", "2022-01-01",
#'                            "2022-01-02", "2022-01-02", "2022-01-03")),
#'                            industry = c("Tech", "Tech", "Finance", "Finance", "Tech"),
#'                            feature1 = c(NA, 2, NA, 4, 5),
#'                            feature2 = c(1, NA, NA, 3, NA))
#' sector_fill_panel(features_m_df, "industry")
#'
#' @export
sector_fill_panel <- function(features_m_df, industry_classification_column_name, features_to_preserve = NA){

  #Check structure of features_m_df
  if(!is_coercible_to_meta_dataframe(features_m_df)){
    stop("features_m_df should be coercible to meta_dataframe object")
  }

  #Extract data.frame in case of meta_dataframe obj
  if(is_meta_dataframe(features_m_df)){
    past_workflow <- features_m_df@workflow #get past workflow
    features_m_df <- features_m_df@data #get data
  } else {
    past_workflow <- NULL
  }

  #Check industry classification
  if(!is.character(industry_classification_column_name)){
    stop("industry_classification_column_name must be a character.")
  } else {}
  if(!(industry_classification_column_name %in% colnames(features_m_df))){
    stop("industry_classification_column is not present in features_m_df")
  } else {}

  #Get dates vector
  dates_vector <- as.Date(unique(features_m_df$dates), format = "%Y-%m-%d") #Get dates

  #Initialize
  sector_panel <- features_m_df
  for (d in 1:length(dates_vector)) {
    #Initialize objects
    #Subset based on dates
    subset_matrix_ref <- which(as.Date(features_m_df$dates, format = "%Y-%m-%d") == as.Date(dates_vector[d], format = "%Y-%m-%d"))
    subset_matrix <- features_m_df[subset_matrix_ref, ]
    #Change colnames
    colnames(subset_matrix) <- colnames(features_m_df)

    #Industry classification object
    industry_classification_col_position <- which(colnames(sector_panel) == industry_classification_column_name) #Get sector classification column position

    #Loop for features
    for (j in 1:ncol(features_m_df)) {
      # If the column is a factor, character, date or is in features_to_preserve, skip
      if (class(features_m_df[, j]) %in% c("factor", "character", "Date") ||
          colnames(features_m_df)[j] %in% features_to_preserve) {
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


  # Calculate metadata
  unique_dates_count <- length(unique(sector_panel$dates))
  unique_tickers_count <- length(unique(sector_panel$tickers))
  total_observations_count <- nrow(sector_panel)
  features_names <- colnames(sector_panel[,-c(1:3)])

  #adjust workflow
  new_workflow <- paste("NAs filled according to sector_fill_panel. All features were adjusted according to", industry_classification_column_name,
                        ", except for", features_to_preserve)
  workflow <- if(is.null(past_workflow)) {
    list(new_workflow)
  } else {
    c(past_workflow, new_workflow)
  }

  # Create meta_dataframe object
  sector_meta_df <- new("meta_dataframe",
                                 data = sector_panel,
                                 workflow = workflow,
                                 signals = features_names,
                                 unique_dates = unique_dates_count,
                                 unique_tickers = unique_tickers_count,
                                 n_obs = total_observations_count)

  return(sector_meta_df)

}
