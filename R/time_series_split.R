#' Time Series Split
#'
#' Split time series data into training, validation, and refitting samples, in order to perform a walk-forward expanding or rolling time series validation.
#'
#' @param current_date A single date in the format '%Y-%m-%d'.
#' @param features_m_df A matrix or data frame containing features with columns 'id', 'tickers', and 'dates'.
#' @param target_m_df A matrix or data frame containing target variables.
#' @param dates_m_vector A vector of dates in the format '%Y-%m-%d'.
#' @param training_sample_size Number of observations to include in the training sample.
#' @param validation_sample_size Number of observations to include in the validation sample (default is 0).
#' @param target_fwd The number of periods forward to forecast.
#' @param target_fwd_name The name of the target variable.
#' @param split_method The method used for splitting the data, either 'expanding' or 'rolling' (default is 'expanding').
#'
#' @return A list containing the training, validation, and refit samples.
#'
time_series_split <- function(current_date, features_m_df, target_m_df, dates_m_vector, training_sample_size, validation_sample_size = 0, target_fwd,
                              target_fwd_name, split_method = "expanding"){


  ###Initial Checks###
  ################

  #Check structure
  if(!(is.matrix(features_m_df) || is.data.frame(features_m_df)) ||
     !(is.matrix(target_m_df) || is.data.frame(target_m_df)) ||
     !(any(is.factor(dates_m_vector), inherits(dates_m_vector, "Date"))) ||
     !(is.numeric(target_fwd)) ||
     !(is.character(target_fwd_name)) ||
     !is.numeric(training_sample_size) ||
     !is.numeric(validation_sample_size)
  ){
    stop("Objects not in correct class.")
  }

  #Check for corret format in current_date
  if(!lubridate::is.Date(current_date) ||
     is.na(as.Date(current_date, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d"))) ||
    length(current_date) != 1){
    stop("current_date must be a single date object with format %Y-%m-%d")
  }


  #Check for correct format in dates_m_vector
  if(any(!lubridate::is.Date(dates_m_vector)) ||
     any(is.na(as.Date(dates_m_vector, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d"))))){
    stop("dates_m_vector must be a date object with format %Y-%m-%d")
  }

  #Check for correct format in features_m_df
  if(!all(c("id", "tickers", "dates") %in% colnames(features_m_df))){
    stop("features_m_df should have id, tickers and dates columns.")
  }

  #Check structure of dates_m_vector and features_m_df$dates
  if(!all(as.character(dates_m_vector) %in% unique(as.character(features_m_df$dates))) ||
     !all(unique(as.character(features_m_df$dates)) %in% as.character(dates_m_vector))){
    stop("all dates in dates_m_vector must have a correspondence in features_m_df")
  }

  #Check structure of dates_m_vector and target_m_df$dates
  if(!all(as.character(dates_m_vector) %in% unique(as.character(target_m_df$dates))) ||
     !all(unique(as.character(target_m_df$dates)) %in% as.character(dates_m_vector))){
    stop("all dates in dates_m_vector must have a correspondence in target_m_df")
  }
  if(length(dates_m_vector) <= target_fwd){
    stop("dates_m_vector should have more dates than target_fwd")
  }
  if(!all(dates_m_vector == dates_m_vector[order(dates_m_vector)])){
    stop("dates_m_vector should be in ascending chronological order")
  }

  #Check if method is corret
  if(!split_method %in% c("expanding", "rolling")){
    stop("split_method should be expanding or rolling.")
  }

  #Check sizes of validation, training and target_fwd
  if(training_sample_size <= target_fwd){
    stop("training_sample_size should be higher than target_fwd")
  }

  if(validation_sample_size != 0){
  if(validation_sample_size < target_fwd){
    stop("validation_sample_size should be greater than target_fwd (or equal)")
  }
  }

  if((training_sample_size+validation_sample_size-target_fwd) >= length(dates_m_vector)){
    stop("sample size should be higher than training_sample_size + validation_sample_size - target_fwd")
  }



  #Set relative position in dates_m_vector
  d <- which(dates_m_vector == current_date)

  #Takes column corresponding to specific target
  target_vector <- target_m_df %>% dplyr::pull(target_fwd_name)

  #Training Sample
  #################
  training_sample_ref <- which(as.Date(features_m_df$dates, format = "%Y-%m-%d") >= dates_m_vector[1] &


                                 #                       This is of ultimate importance!!!!!!!


                                 as.Date(features_m_df$dates, format = "%Y-%m-%d") <=
                                 #Check if it is rebalancing_month to set sample
                                 ifelse(
                                   (d == (training_sample_size + validation_sample_size)), #Is it rebalancing month or first training sample?
                                   dates_m_vector[training_sample_size - target_fwd], #First training sample
                                   dates_m_vector[d - validation_sample_size - target_fwd])) #Rebalancing

  #Get training sample objects
  features_training_sample <- features_m_df[training_sample_ref,]
  target_training_sample <- target_vector[training_sample_ref]
  full_data_training_sample_clean <- cbind(target_training_sample, features_training_sample[,-c(1:3)])

  #Rename
  colnames(full_data_training_sample_clean)[1] <- target_fwd_name

  ###################

  #Validation Sample
  #################
  if(validation_sample_size > 0){
  validation_sample_ref <- which(as.Date(features_m_df$dates, format = "%Y-%m-%d") >=
                                   ifelse(
                                     (d == (training_sample_size + validation_sample_size)), #Is it rebalancing month or first validation sample?
                                     dates_m_vector[training_sample_size], #First validation sample
                                     dates_m_vector[d - validation_sample_size]) & #Rebalancing


                                   #                        This is of ultimate importance!!!!!!!


                                   as.Date(features_m_df$dates, format = "%Y-%m-%d") <=
                                   #Check if it is rebalancing_month to set sample
                                   ifelse(
                                     (d == training_sample_size + validation_sample_size), #Is it rebalancing month or first training sample?
                                     dates_m_vector[training_sample_size + validation_sample_size - target_fwd], #First validation sample
                                     dates_m_vector[d - target_fwd])) #Rebalancing


  #Get validation sample objects
  features_validation_sample <- features_m_df[validation_sample_ref,]
  target_validation_sample <- target_vector[validation_sample_ref]
  }
  #Refitting
  ###################
  refit_d_ref <- which(as.Date(features_m_df$dates, format = "%Y-%m-%d") >= dates_m_vector[1] &


                         # This is of ultimate importance!!!!!!!


                         as.Date(features_m_df$dates, format = "%Y-%m-%d") <=
                         #Check if it is rebalancing_month to set sample
                         ifelse(
                           (d == (training_sample_size + validation_sample_size)), #Is it rebalancing month or first training sample?
                           dates_m_vector[training_sample_size + validation_sample_size - target_fwd], #First training sample
                           dates_m_vector[d - target_fwd])) #Rebalancing

  #Refit new model using data from d - target_fwd
  features_m_refit <- features_m_df[refit_d_ref,] #Subset
  target_m_refit <- target_vector[refit_d_ref] #Subset
  full_data_m_refit_clean <- cbind(target_m_refit, features_m_refit[,-c(1:3)]) #Full data

  #Rename
  colnames(full_data_m_refit_clean)[1] <- target_fwd_name

  #Results
  if(validation_sample_size != 0){ #If there is a validation sample
  results <- list()
  results[[1]] <- list()
  results[[2]] <- list()
  results[[3]] <- list()

  names(results) <- c("training", "validation", "refit")

  #Training
  results$training[[1]] <- features_training_sample
  results$training[[2]] <- target_training_sample
  results$training[[3]] <- full_data_training_sample_clean
  names(results$training) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")

  #Validation
  results$validation[[1]] <- features_validation_sample
  results$validation[[2]] <- target_validation_sample
  names(results$validation) <- c("features_validation_sample", "target_validation_sample")

  #Refit
  results$refit[[1]] <- features_m_refit
  results$refit[[2]] <- target_m_refit
  results$refit[[3]] <- full_data_m_refit_clean
  names(results$refit) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")

  } else { #If there is not
    results <- list()
    results[[1]] <- list()
    results[[2]] <- list()

    names(results) <- c("training", "refit")

    #Training
    results$training[[1]] <- features_training_sample
    results$training[[2]] <- target_training_sample
    results$training[[3]] <- full_data_training_sample_clean
    names(results$training) <- c("features_training_sample", "target_training_sample", "full_data_training_sample_clean")


    #Refit
    results$refit[[1]] <- features_m_refit
    results$refit[[2]] <- target_m_refit
    results$refit[[3]] <- full_data_m_refit_clean
    names(results$refit) <- c("features_m_refit", "target_m_refit", "full_data_m_refit_clean")


  }

  return(results)
}
