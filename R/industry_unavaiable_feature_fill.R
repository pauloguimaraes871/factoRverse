#' Fill Industry-Unavailable Feature
#'
#' This function fills missing values of an unavailable feature in a features matrix with the mean value of similar features.
#' An industry-unavaiable feature is a feature that is not defined for a given industry.
#' This is a rather common problem for banks, in which some accounting information are not defined.
#' For instance, one might replace FCF Yield by Dividend Yield.
#'
#' @param features_m_df A data frame containing the features matrix with columns for "id", "tickers" and "dates".
#' @param unavaiable_feature The name of the unavailable feature to be filled.
#' @param similar_features A character vector containing the names of similar features used for imputation.
#' @param industry_classification_column_name The name of the column in \code{features_m_df} containing industry classifications.
#' @param selected_industries A character vector containing the names of industries to which the imputation should be restricted.
#'
#' @return A data frame with missing values of \code{unavaiable_feature} filled with the mean of \code{similar_features} within specified industries.
#'
#' @export
#'
#' @examples
#' features_m_df <- data.frame(id = 1:5,
#'                                tickers = c("AAPL", "GOOG", "JPM", "WFC", "FB"),
#'                                dates = as.Date("2022-01-01") + 0:4,
#'                                industry = c("Tech", "Tech", "Banks", "Banks", "Tech"),
#'                                feature1 = c(NA, 2, NA, 4, 5),
#'                                feature2 = c(1, NA, NA, 3, NA))
#' industry_unavaiable_feature_fill(features_m_df, "feature1", "feature2", "industry", c("Finance"))
industry_unavaiable_feature_fill <- function(features_m_df, unavaiable_feature, similar_features, industry_classification_column_name,
                                             selected_industries){
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

  #Check formats
  if(!is.character(unavaiable_feature)){
    stop("unavaiable_feature must be a character.")
  } else {}
  if(!is.character(industry_classification_column_name)){
    stop("industry_classification_column_name must be a character.")
  } else {}
  if(!is.character(similar_features)){
    stop("similar_features must be a character.")
  } else {}
  if(!is.character(selected_industries)){
    stop("selected_industries must be a character.")
  } else {}

  #Check there is a segment column. A segment column is necessary, because it provides granular information about banks. sector information will treat as "Financial Intermediaries", including both banks and insurance cias.
  if(!(industry_classification_column_name %in% colnames(features_m_df))){
    stop("industry_classification_column is not present in features_m_df")
  } else {}
 #Check if unavaiable feature is indeed part of features_m_df
  if(!all(unavaiable_feature %in% colnames(features_m_df))){
    stop("unavaiable features must be present in features_m_df")
  } else {}
  #Check if similar_features feature is indeed part of features_m_df
  if(!all(similar_features %in% colnames(features_m_df))){
    stop("similar features must be present in features_m_df")
  }



  #Initialize objects
  ###########
  fill_panel <- features_m_df
  #Missing feature objects
  missing_feature_col_position <- which(colnames(fill_panel) == unavaiable_feature) #Get missing feature column position
  missing_feature_vector <- fill_panel[,missing_feature_col_position] #Get missing features
  missing_feature_NA_ref <- which(is.na(missing_feature_vector)) #Which is NA?

  #In case there are no NAs, stop
  if(length(missing_feature_NA_ref) == 0){
    stop("No NA to fill")
  } else {}


  #Industry classification object
  industry_classification_col_position <- which(colnames(fill_panel) == industry_classification_column_name) #Get sector classification column position
  #Check if missing feature realy has a industry-related behavior.
  #If a given feature is not missing for all entries in selected_industries, it makes more sense to replace NA's by industry-wise information than replacing the feature
  for(s in 1:length(selected_industries)){
    selected_industry_ref <- which(fill_panel[,industry_classification_col_position] == selected_industries[s]) #Which are the id's for selected industries?
    #Check if all entries for a missing feature
    if(!all(is.na(missing_feature_vector[selected_industry_ref]))){
      stop("unavaiable_feature is not unavaiable across all entries in selected industry")
    } else {}
  }
  #Given that unavaiable_feature is unavaiable across all entries, get selected industries entries
  selected_industry_ref <- which(fill_panel[,industry_classification_col_position] %in% selected_industries)

  #Similar feature objects
  similar_features_col_positions <- which(colnames(fill_panel) %in% similar_features) #Get similar feature column position
  similar_features_panel <- fill_panel[selected_industry_ref,similar_features_col_positions] #Get similar features for given industries



  #Replacement
  ###########
  if(length(similar_features) == 1){
    similar_features_mean <- similar_features_panel #If there is only one similar feature, take its value
  } else {
    similar_features_mean <- rowMeans(similar_features_panel, na.rm = TRUE) #calculate row means for similar features when there are more than 1
  }

  fill_panel[base::intersect(selected_industry_ref,missing_feature_NA_ref) #Get references for NAs in selected_industries
             ,missing_feature_col_position] <- similar_features_mean

  # Calculate metadata
  unique_dates_count <- length(unique(fill_panel$dates))
  unique_tickers_count <- length(unique(fill_panel$tickers))
  total_observations_count <- nrow(fill_panel)
  features_names <- colnames(fill_panel[,-c(1:3)])

  #adjust workflow
  new_workflow <- paste("NAs filled according to industry_unavaiable_feature_fill. The industry-unavaiable feature:", unavaiable_feature, "which is unavaible for",
  selected_industries, "(following the:", industry_classification_column_name, "industry classification) was filled with the cia-wise mean of:",
  paste(similar_features, collapse = ", "))

  workflow <- if(is.null(past_workflow)) {
    list(new_workflow)
  } else {
    c(past_workflow, new_workflow)
  }

  # Create meta_dataframe object
  fill_meta_df <- new("meta_dataframe",
                                   data = fill_panel,
                                   workflow = workflow,
                                   signals = features_names,
                                   unique_dates = unique_dates_count,
                                   unique_tickers = unique_tickers_count,
                                   n_obs = total_observations_count)

  #Return Fill Panel
  return(fill_meta_df)

}
