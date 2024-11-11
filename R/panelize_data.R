#' Panelize Data
#'
#' Convert a list of matrices or data frames into panel data format.
#'
#' @param features_list A list of matrices, data frames or tibbles containing the features.
#' @param row_names A vector of row names for the panel data.
#' @param column_names A vector of column names for the panel data.
#' @param features_names A vector of names for each feature.
#'
#' @return A meta dataframe object.
#'
#' @details This function takes a list of matrices or data frames, each representing a set of features for a group of entities, and converts them into a panel data format. Each matrix or data frame in the list represents a time series of features for a set of entities. The resulting panel data matrix has one row for each combination of entity and time point, with columns representing different features.
#'
#' @examples
#' # Example usage
#' features_list <- list(matrix(1:9, nrow = 3), matrix(11:19, nrow = 3))
#' row_names <- c("A", "B", "C")
#' column_names <- c("X", "Y", "Z")
#' features_names <- c("Feature1", "Feature2")
#' panelize_data(features_list, row_names, column_names, features_names)
#'
#'

panelize_data <- function(features_list, row_names, column_names, features_names){

  # Check if features_list is a list of matrices, data frames, or tibbles
  if (!is.list(features_list) ||
      !all(sapply(features_list, function(x) is.data.frame(x) || is.matrix(x) || tibble::is_tibble(x))) ||
      length(unique(sapply(features_list, nrow))) != 1 ||
      length(unique(sapply(features_list, ncol))) != 1 ||
      length(row_names) != unique(sapply(features_list, nrow)) ||
      length(column_names) != unique(sapply(features_list, ncol)) ||
      length(features_names) != length(features_list)) {
    stop("Input must be a list of matrices, data frames or tibbles with the same dimensions.")
  }

  # Convert each feature in features_list to data frame
  features_list <- lapply(features_list, as.data.frame)

  #Initialize list
  panel_features <- list()

  #for every element in list
  for(l in 1:length(features_list)){
    #Tickers + Features
    features_df <- data.frame(row_names, features_list[[l]])
    colnames(features_df)[1] <- "tickers" #change col name
    colnames(features_df)[2:length(colnames(features_df))] <- as.character(column_names)

    #melt to panel format
    panel_matrix <- reshape2::melt(features_df, id.vars="tickers")
    colnames(panel_matrix)[2] <- "dates" #change name
    id <- paste(panel_matrix$tickers, panel_matrix$dates, sep = "-") #create new id
    panel_matrix <- cbind(id, panel_matrix) #append id

    #change col name to characteristic name
    colnames(panel_matrix)[4] <- features_names[l]
    panel_matrix <- panel_matrix[order(panel_matrix$id), ] #order alphabetically by id
    panel_features[[l]] <- panel_matrix #save in list
  }

  # Create new data frame to store panel data
  final_panel <- data.frame(id = panel_features[[1]]$id,
                            tickers = panel_features[[1]]$tickers,
                            dates = as.Date(panel_features[[1]]$dates),
                            stringsAsFactors = FALSE)

  #Fill columns with characteristics
  for(l in 1:length(features_list)){
    final_panel[[features_names[l]]] <- panel_features[[l]][, 4] #append last column, which is the characteristic
  }


  return(final_panel)

}
