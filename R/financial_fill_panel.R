#' Fill Financial Cias Missing Data
#'
#' This function fills missing data in a panel data matrix, specifically for banks and other financial companies.
#'
#' @param features_m_df A meta dataframe or coercible object containing panel data with columns for "id", "tickers", "dates", "segment", and other features.
#' @param segment_column A character indicating which column in features_m_df contain economic segment classification to fill based on segments_to_adjust
#' @param segments_to_adjust A character vector specifying the segments (e.g., banks) for which missing data should be filled.
#' @param features_to_preserve A character vector specifying features that should not have missing data filled.
#'
#' @return A panel data matrix with missing data filled.
#'
#' @details This function fills missing data in the panel data matrix for the specified segments. It calculates the mean for each feature across all companies (including non-banks) and fills missing values in bank segments with the calculated mean.
#' Features listed in \code{features_to_preserve} will not be filled even if they have missing values. Features that have missing values for all banks (but not for non-bank companies) are not filled, as it's assumed they require specific treatment.
#'
#' @examples
#' # Example usage
#' features_m_df <- data.frame(
#'   id = c(1, 2, 3, 4, 5),
#'   tickers = c("A", "B", "C", "D", "E"),
#'   dates = as.Date(c("2022-01-01", "2022-01-01", "2022-01-02", "2022-01-02", "2022-01-03")),
#'   segment = c("Bank", "Bank", "Other", "Bank", "Other"),
#'   feature1 = c(1, 2, NA, 4, 5),
#'   feature2 = c(NA, 6, NA, 8, NA)
#' )
#' segments_to_adjust <- c("Bank")
#' filled_data <- financialcia_fill_panel(features_m_df, segment_column = "segment", segments_to_adjust)
#'
#' @export
#'
financialcia_fill_panel <- function(features_m_df, segment_column = "segment", segments_to_adjust, features_to_preserve = NULL){

  #Check structure of features_m_df
  if(!is_coercible_to_meta_dataframe(features_m_df)){
    stop("features_m_df should be coercible to meta_dataframe object")
  }

  #Extract data.frame in case of meta_dataframe obj
  if(is_meta_dataframe(features_m_df)){
    meta_dataframe_name <- features_m_df@meta_dataframe_name
    past_workflow <- features_m_df@workflow #get past workflow
    features_m_df <- features_m_df@data #get data
  } else {
    past_workflow <- NULL
    meta_dataframe_name <- "not_identified"
  }

  #Check there is a segment column.
  #A segment column is necessary, because it informs sectors.
  #For economática, "segment" provides the granular information, as "sector" will have a "Financial Intermediaries" classification,
  #including both banks and insurance cias.
  if(!(segment_column %in% colnames(features_m_df))){
    stop("there must be a segment_column in features_m_df")
  } else {}

  #Get dates vector
  dates_vector <- as.Date(unique(features_m_df$dates), format = "%Y-%m-%d") #Get dates

  #Check structure of dates_vector and features_m_df$dates
  if(!all(as.character(dates_vector) %in% unique(as.character(features_m_df$dates))) ||
     !all(unique(as.character(features_m_df$dates)) %in% as.character(dates_vector))){
    stop("all dates in dates_vector must have a correspondence in features_m_df")
  } else {}

  #Initialize object
    bank_panel <- features_m_df
    for (d in 1:length(dates_vector)) {
      #Subset based on dates
      subset_matrix_ref <- which(as.character(features_m_df$dates) == as.character(dates_vector[d]))
      subset_matrix <- features_m_df[subset_matrix_ref, ]
      colnames(subset_matrix) <- colnames(features_m_df)#Change colanmes

      #Select banks and other financial cias inside subset (segments to adjust)
      banks_ref <- which(subset_matrix[, segment_column] %in%  segments_to_adjust)
      subset_banks <- subset_matrix[banks_ref,] #Subset banks
      colnames(subset_banks) <- colnames(features_m_df)#Change colanmes

      for (j in 1:ncol(features_m_df)) {
         if (class(features_m_df[, j]) %in% c("factor", "character", "Date") ||
            colnames(features_m_df)[j] %in% features_to_preserve ||
            #Features that have NAs for all banks (only banks) should not be replaced by subset means. It is better to handle each specifically, based on similarity to other features.
            (all(is.na(features_m_df[which(features_m_df[, segment_column] %in% segments_to_adjust),j])) &
            !all(is.na(features_m_df[,j])))) {
          # If the column is a factor, character, date or is in features_to_preserve, skip
          subset_banks[, j] <- subset_banks[, j]
        } else {
          for(i in 1:nrow(subset_banks)){
            #Take mean of cias in subset
            subset_mean <- mean(subset_matrix[,j], na.rm = TRUE)
            #It there is a NA in subset_banks, replace by subset_mean. Otherwise, keep it as is.
            subset_banks[i,j] <- ifelse(is.na(subset_banks[i,j]), subset_mean, subset_banks[i,j])
          }
        }
        subset_matrix[banks_ref, j] <- subset_banks[,j]
        bank_panel[subset_matrix_ref, j] <- subset_matrix[,j]
      }
    }


    # Calculate metadata
    unique_dates_count <- length(unique(bank_panel$dates))
    unique_tickers_count <- length(unique(bank_panel$tickers))
    total_observations_count <- nrow(bank_panel)
    features_names <- colnames(bank_panel[,-c(1:3)])

    #adjust workflow
    new_workflow <- paste("NAs filled according to financialcia_fill_panel. All features of cias in the", segments_to_adjust, segment_column,
                          "were adjusted, except for", features_to_preserve)
    workflow <- if(is.null(past_workflow)) {
      list(new_workflow)
    } else {
      c(past_workflow, new_workflow)
    }

    # Create meta_dataframe object
    winsorized_bank_meta_df <- new("meta_dataframe",
                              data = bank_panel,
                              workflow = workflow,
                              signals = features_names,
                              unique_dates = unique_dates_count,
                              unique_tickers = unique_tickers_count,
                              n_obs = total_observations_count,
                              meta_dataframe_name = meta_dataframe_name
                              )


    return(winsorized_bank_meta_df)

}
