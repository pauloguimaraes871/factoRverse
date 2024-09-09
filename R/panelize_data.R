#' Panelize Data
#'
#' Convert a list of matrices or data frames into panel data format.
#'
#' @param features_list A list of matrices or data frames containing the features.
#' @param row_names A vector of row names for the panel data.
#' @param column_names A vector of column names for the panel data.
#' @param features_names A vector of names for each feature.
#'
#' @return A panel data matrix.
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
#'
#' @export
panelize_data <- function(features_list, row_names, column_names, features_names){
  # Check if the input is a list of matrices
  if (!is.list(features_list) || #Check if list
      !all(sapply(features_list, is.data.frame) | sapply(features_list, is.matrix)) ||
      length(unique(c(sapply(features_list, dim)[1,]))) != 1 || #Check dimensions
      length(unique(c(sapply(features_list, dim)[2,]))) != 1 || #Check dimensions
      length(row_names) != unique(c(sapply(features_list, dim)[1,])) || #Check dimensions
      length(column_names) != unique(c(sapply(features_list, dim)[2,])) || #Check dimensions
      length(features_names) != length(features_list) #Check dimensions
     ){ 
    stop("Input must be a list of matrices/data.frame with same dimension")
  } else {
    #Initialize list
    panel_features <- list()
    #for every element in list
    for(l in 1:length(features_list)){ 
      features_df <- data.frame(row_names, features_list[[l]]) #Tickers + Features
      colnames(features_df)[1] <- "tickers" #change col name
      colnames(features_df)[2:length(colnames(features_df))] <- as.character(column_names)
      panel_matrix <- reshape2::melt(features_df, id.vars="tickers") #melt to panel format
      colnames(panel_matrix)[2] <- "dates" #change name 
      id <- paste(panel_matrix$tickers, panel_matrix$dates, sep = "-") #create new id
      panel_matrix <- cbind(id, panel_matrix) #append id
      colnames(panel_matrix)[4] <- features_names[l] #change col name to characteristic name
      panel_matrix <- panel_matrix[order(panel_matrix$id), ] #order alphabetically by id
      panel_features[[l]] <- panel_matrix #save in list
    }
    #create new matrix object to store panel data
    final_panel <- matrix(NA, nrow = length(row_names)*length(column_names), ncol = length(features_list))
    final_panel <- cbind(panel_features[[1]][,1:3], final_panel)
    for(l in 1:length(features_list)){
      final_panel[,3+l] <- panel_features[[l]][,4] #append last column, which is the characteristic
      colnames(final_panel)[3+l] <- features_names[l] #change name
      rownames(final_panel) <- NULL
    }
    return(final_panel)
  }
}
