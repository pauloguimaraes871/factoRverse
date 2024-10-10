# Normalize Panel Data
#'
#' This function normalizes a panel data matrix, scaling each feature to a range of \code{-1} to \code{1}.
#'
#' @param features_m_df A data frame containing panel data with columns for "id", "tickers" and "dates".
#'
#' @return A normalized panel data matrix.
#'
#' @details This function normalizes each feature in the panel data matrix to a range of \code{-1} to \code{1}. For each specified date, it computes the minimum and maximum values for each feature and scales the values accordingly.
#'
#' @examples
#' # Example usage
#' features_m_df <- data.frame(
#'   id = c(1, 2, 3, 4),
#'   tickers = c("A", "B", "C", "D"),
#'   dates = as.Date(c("2022-01-01", "2022-01-01", "2022-01-02", "2022-01-02")),
#'   feature1 = c(1, 2, 3, 4),
#'   feature2 = c(5, 6, 7, 8)
#' )
#' normalized_data <- normalize_panel_data(features_m_df)
#'
#' @export
#'
normalize_panel_data <- function(features_m_df) {

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

  #Get dates vector and check
  dates_vector <- as.Date(unique(features_m_df$dates), format = "%Y-%m-%d") #Get dates

  #Check structure of dates_vector and features_m_df$dates
  if(!all(as.Date(dates_vector, format = "%Y-%m-%d") %in% unique(as.Date(features_m_df$dates, format = "%Y-%m-%d"))) ||
     !all(unique(as.Date(features_m_df$dates, format = "%Y-%m-%d")) %in% as.Date(dates_vector, format = "%Y-%m-%d"))){
    stop("all dates in dates_vector must have a correspondence in features_m_df")
  } else {}

  #Initialize normalized_matrix
  normalized_matrix <- features_m_df
  for (d in 1:length(dates_vector)) {
      #Set subset
      subset_matrix_ref <- which(as.Date(features_m_df$dates, format = "%Y-%m-%d") == as.Date(dates_vector[d], format = "%Y-%m-%d"))
      subset_matrix <- features_m_df[subset_matrix_ref, ]
      for (j in 1:ncol(features_m_df)) {
        if (class(features_m_df[, j]) %in% c("factor", "character", "Date")) {
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

  # Calculate metadata
  unique_dates_count <- length(unique(normalized_matrix$dates))
  unique_tickers_count <- length(unique(normalized_matrix$tickers))
  total_observations_count <- nrow(normalized_matrix)
  features_names <- colnames(normalized_matrix[,-c(1:3)])

  #adjust workflow
  new_workflow <- "normalization"
  workflow <- if(is.null(past_workflow)) {
    list(new_workflow)
  } else {
    c(past_workflow, new_workflow)
  }

  # Create meta_dataframe object
  normalized_meta_df <- new("meta_dataframe",
                            data = normalized_matrix,
                            workflow = workflow,
                            signals = features_names,
                            unique_dates = unique_dates_count,
                            unique_tickers = unique_tickers_count,
                            n_obs = total_observations_count)

  return(normalized_meta_df)
}
