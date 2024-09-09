#' Adjust Machine Learning Walk-Forward Validation Results
#'
#' This function processes a list of machine learning walk-forward validation results and extracts
#' the predictions for a specified date. It combines these predictions into a single data frame and
#' renames the columns based on the machine learning algorithms used. The function also warns if any
#' of the columns in the resulting data frame show no variation in their values.
#'
#' @param ml_walk_forward_validation_results_list A list where each element is a list containing
#'   `prediction_list` and `metadata`. `prediction_list` should be a named list of predictions, and
#'   `metadata` should include the machine learning algorithm used.
#' @param current_date A string representing the date for which predictions should be extracted.
#'
#' @return A data frame where each row corresponds to a ticker, and columns correspond to predictions
#'   from different machine learning algorithms. The column names are the machine learning algorithms
#'   used. A warning is issued if any column shows no variation in its values.
#'
#' @details This function assumes that `ml_walk_forward_validation_results_list` is a list of lists,
#'   each containing a `prediction_list` (which is a list of predictions keyed by date) and `metadata`
#'   (which includes information such as the machine learning algorithm used). It uses the specified
#'   `current_date` to extract predictions and merge them into a single data frame. Columns are named
#'   according to the machine learning algorithms used. If any column has no variation (i.e., all values
#'   are identical), a warning is generated.
#'
adjust_ml_walk_forward_validation_results_list <- function(ml_walk_forward_validation_results_list, current_date){

  #Prepare ml_predictions_df
  ##########################
  ##Place current predictions in a list
  ml_predictions_list <- ml_walk_forward_validation_results_list %>% #get list of ml_predictions
    lapply(function(x) x$prediction_list[[which(names(x$prediction_list) == current_date)]] %>%
             as.data.frame() %>% tibble::rownames_to_column(var = "tickers")) #coerce to data.frame

  ##Reduce to data frame
  ml_predictions_m_d_ref <- Reduce(function(x,y) merge(x, y, by = "tickers", all = TRUE), ml_predictions_list) #Reduce
  colnames(ml_predictions_m_d_ref)[-1] <- ml_walk_forward_validation_results_list %>% sapply(function(x) x$metadata$ml_algorithm) #rename

  ##########################

  ###Check for no variation in ml_predictions
  no_variation_ml_prediction <- as.data.frame(ml_predictions_m_d_ref[,-1]) %>% apply(2, function(x) length(unique(x)) == 1) #check condition
  if(any(no_variation_ml_prediction)){
    warning(paste("No variation observed in column",
                  colnames(ml_predictions_m_d_ref[,-1])[which(no_variation_ml_prediction == TRUE)],
                  "of ml_predictions_m_d_ref in date",
                  current_date)
    )
  }

  return(ml_predictions_m_d_ref)

}
