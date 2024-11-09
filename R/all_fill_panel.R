#' Fill Panel Data
#'
#' This function fills missing values in a panel data set by replacing them with the mean of the respective feature across similar entities and time periods.
#'
#' @param features_m_df A data frame containing the panel data with columns "id", "tickers", "dates", and features to be filled.
#' @param features_to_preserve A vector of column names of features that should not be filled.
#'
#' @return A data frame with missing values filled by the mean of each feature across similar entities and time periods.
#' @export
#'
#' @examples
#' # Create sample data frame
#' features_m_df <- data.frame(id = c(1, 1, 2, 2),
#' tickers = c("A", "A", "B", "B"),
#' dates = as.Date(c("2022-01-01", "2022-01-02", "2022-01-01", "2022-01-02")),
#' feature1 = c(1, NA, 3, 4),
#' feature2 = c(NA, 2, 3, NA))
#'
#'
#' # Fill missing values
#' filled_panel <- all_fill_panel(features_m_df,  features_to_preserve = c("id", "tickers", "dates"))
all_fill_panel <- function(features_m_df,  features_to_preserve = NA){

  #Check structure of features_m_df
  if(!is_coercible_to_meta_dataframe(features_m_df)){
    stop("features_m_df should be coercible to meta_dataframe object")
  }

  #Extract data.frame in case of meta_dataframe obj
  if(is_meta_dataframe(features_m_df)){
    meta_dataframe_name <- features_m_df@meta_dataframe_name #get name
    past_workflow <- features_m_df@workflow #get past workflow
    features_m_df <- features_m_df@data #get data
  } else {
    past_workflow <- NULL
    meta_dataframe_name <- "not_identified"
  }

  #Init obj
  filled_panel <- features_m_df
  #Get dates vector
  dates_vector <- as.Date(unique(features_m_df$dates), format = "%Y-%m-%d") #Get dates

  for (d in 1:length(dates_vector)) {
    #Initialize objects
    #Subset based on dates
    subset_matrix_ref <- which(as.Date(features_m_df$dates, format = "%Y-%m-%d") == as.Date(dates_vector[d], format = "%Y-%m-%d"))
    subset_matrix <- features_m_df[subset_matrix_ref, ]
    #Change colnames
    colnames(subset_matrix) <- colnames(features_m_df)

    #Loop for features
    for (j in 1:ncol(features_m_df)) {
      # If the column is a factor, character, date or is in features_to_preserve, skip
      if (class(features_m_df[, j]) %in% c("factor", "character", "Date") ||
          colnames(features_m_df)[j] %in% features_to_preserve) {
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

  # Calculate metadata
  unique_dates_count <- length(unique(filled_panel$dates))
  unique_tickers_count <- length(unique(filled_panel$tickers))
  total_observations_count <- nrow(filled_panel)
  features_names <- colnames(filled_panel[,-c(1:3)])

  #adjust workflow
  new_workflow <- paste("NAs filled according to all_fill_panel. All features were adjusted, except for", features_to_preserve)
  workflow <- if(is.null(past_workflow)) {
    list(new_workflow)
  } else {
    c(past_workflow, new_workflow)
  }

  # Create meta_dataframe object
  filled_meta_df <- new("meta_dataframe",
                                   data = filled_panel,
                                   workflow = workflow,
                                   signals = features_names,
                                   unique_dates = unique_dates_count,
                                   unique_tickers = unique_tickers_count,
                                   n_obs = total_observations_count,
                                   meta_dataframe_name = meta_dataframe_name
                        )

  return(filled_meta_df)

}
