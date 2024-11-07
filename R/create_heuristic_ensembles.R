#' Create Heuristic Ensemble of Model Configurations
#'
#' This function creates an ensemble by combining multiple model configuration lists.
#' Each configuration contains a series of date-named lists with values. The function
#' checks if dates across configurations match and calculates the mean or weighted
#' mean for each date.
#'
#' @param config_list A list of model configurations, each containing date-named lists
#'   with numeric values (e.g., `list(rf_config = list("2008-05-11" = 0.5, "2008-06-11" = 0.6))`).
#' @param weights Optional numeric vector of weights for each configuration. The length
#'   should match the length of `config_list`. Only used if `method = "weighted_mean"`.
#' @param method A character string specifying the aggregation method. Should be either
#'   `"mean"` or `"weighted_mean"`. Defaults to `"mean"`.
#'
#' @return A named list where each element corresponds to a date, containing the
#'   aggregated value (mean or weighted mean) for that date across all configurations.
#'
#' @export
create_heuristic_ensembles <- function(ml_backtest_results_list, ensemble_eval_metric = "rmse", ensemble_huber_delta = NULL, ensemble_quantile_tau = NULL) {

  #Check validity
  ####################
  valid_metrics <- c("rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")
  if (!ensemble_eval_metric %in% valid_metrics) {
    stop(
      "Invalid ensemble_eval_metric. Must be one of: ",
      paste(valid_metrics, collapse = ", "), "."
    )
  }

  # Validate ml_backtest_results_list
  if (!is.list(ml_backtest_results_list) || length(ml_backtest_results_list) == 0) {
    stop("ml_backtest_results_list must be a non-empty list of ml_backtest_results objects.")
  }

  if (is.null(names(ml_backtest_results_list)) || any(names(ml_backtest_results_list) == "")) {
    stop("All elements in ml_backtest_results_list must be named.")
  }

  ####################


  #Create Ensembles
  ###########################
  ##Number of strategies
  n_ml_configs <- length(ml_backtest_results_list)
  names_ml_configs <- names(ml_backtest_results_list)

  ##Get OOS Predictions
  #######################
  oos_prediction_list <- lapply(ml_backtest_results_list, function(x) x@oos_prediction_list)
  names(oos_prediction_list) <- names_ml_configs
  #######################

  ##Get Target FWD
  target_fwd <- unique(sapply(ml_backtest_results_list, function(x) {
    workflow <- x@ml_backtest_workflow
    if (is.null(workflow$target_fwd)) {
      stop("Each ml_backtest_results object must contain @ml_backtest_workflow$target_fwd.")
    }
    workflow$target_fwd
  }))
  ### Check if target_fwd is unique
  if(length(target_fwd) > 1){
    stop("Disagreement in target_fwd between ml backtests")
  }
  #########

  ##Get OOS Dates and rebalances dates
  #######################
  oos_dates_list <- lapply(oos_prediction_list, names)

  ### Check if all configurations have matching dates
  first_dates <- oos_dates_list[[1]]
  if (!all(purrr::map_lgl(oos_dates_list, ~ identical(.x, first_dates)))) {
    stop("Error: Not all configurations have matching dates.")
  }

  ###Get all unique dates (now we know they match across configurations)
  unique_oos_dates <- as.Date(first_dates)
  ew_unique_oos_dates <- unique_oos_dates
  optimal_unique_oos_dates <- as.Date(first_dates)[-c(1:target_fwd)]

  # Extract rebalance dates from each configuration and ensure consistency
  rebalance_dates_list <- lapply(ml_backtest_results_list, function(x) {
    workflow <- x@ml_backtest_workflow
    if (is.null(workflow$rebalance_dates)) {
      stop("Each ml_backtest_results object must contain @ml_backtest_workflow$rebalance_dates.")
    }
    as.Date(workflow$rebalance_dates)
  })

  # Check if rebalance dates match across configurations
  first_rebalance_dates <- rebalance_dates_list[[1]]
  if (!all(purrr::map_lgl(rebalance_dates_list, ~ identical(.x, first_rebalance_dates)))) {
    stop("Error: Rebalance dates do not match across configurations.")
  }
  rebalance_dates <- as.Date(first_rebalance_dates)
  #######################



  ##Get Y
  #########
  oos_y_list <- lapply(ml_backtest_results_list, function(x) x@oos_y_list)

  ###Check if all configurations have matching y
  if(!all(purrr::map_lgl(oos_y_list, ~ identical(.x, oos_y_list[[1]])))){
    stop("Y values do not match across configurations")
  }

  ##Get unique oos_y_list since they are unique
  ew_oos_y_list <- oos_y_list[[1]]
  optimal_oos_y_list <- oos_y_list[[1]][-c(1:target_fwd)]
  #########


  #######################


  ## Calculate weights
  #######################
  ###EW
  ew_weights <- purrr::map(1:length(ew_unique_oos_dates), ~rep(1/n_ml_configs, n_ml_configs))
  names(ew_weights) <- ew_unique_oos_dates

  ###Optimal Weights

  ####Get Eval Metric at validation dates
  eval_metric_ts <- data.frame(
    #Get the desired eval metric to calc weights
    lapply(ml_backtest_results_list, function(x) x@oos_testing_eval_metrics[!rownames(x@oos_testing_eval_metrics) %in% "consolidated", ensemble_eval_metric]), #not consolidate row
    #Get rownames
    row.names = unique(purrr::map( #Get unique
      lapply(ml_backtest_results_list, function(x) rownames(x@oos_testing_eval_metrics)), #Get dates from rownames
      ~ setdiff(.x, "consolidated") #not consolidate row
    ))[[1]]
  )

  eval_metric_ts_dates <- as.Date(rownames(eval_metric_ts)) #Transform rownames to dates

  ##Get weights
  optimal_weights <- vector("list", length = length(optimal_unique_oos_dates))
  names(optimal_weights) <- as.character(optimal_unique_oos_dates)

  ##Loop through oos_dates
  for(d in 1:length(optimal_unique_oos_dates)){
    optimal_current_date <- optimal_unique_oos_dates[d] #Get current date for optimal weights
    cutoff_date <- unique_oos_dates[which(unique_oos_dates == optimal_current_date) - target_fwd] #Get cutoff date
    #If rebalance date
      if(optimal_current_date %in% rebalance_dates){
        #If it is a rebalance_date, update optimal weights
        ##Get most up to date weight
        mean_eval_metric_uptd <- eval_metric_ts[
          which( #Get the most up to date oned
            eval_metric_ts_dates <= cutoff_date #Get eval_metric_ts of all dates with ensemble_eval_metric known at d
          ), ] %>% colMeans() #Get col means

        ##Transform to optimal_weights
        if(ensemble_eval_metric %in% c("mae", "rmse", "mphe", "mpe", "mape")){
          optimal_weights[[d]] <- (1/mean_eval_metric_uptd)/sum(1/mean_eval_metric_uptd) #For such metrics, less is better
        } else {
          optimal_weights[[d]] <- mean_eval_metric_uptd / sum(mean_eval_metric_uptd) #For others, more is better
        }
      } else {
        optimal_weights[[d]] <- optimal_weights[[d-1]] #Keep the last weight when not rebalancing
      }
    }
  names(optimal_weights) <- optimal_unique_oos_dates #Rename with dates

  #######################

  ##Get FUN
  #######################

  # Helper function to calculate mean or weighted mean for each date
  calculate_weighted_mean <- function(date, weights) {

    # Extract values across configurations for the specific date
    date_predictions <- purrr::map(oos_prediction_list, ~ .x[[which(names(.x) == date)]])

    ##Check if stocks match
    if (!all(purrr::map_lgl(date_predictions, ~ identical(names(.x), names(date_predictions[[1]]))))) {
      stop("Error: Not all configurations have matching stocks.")
    }

    # Check if the number of weights matches the number of configurations
    if (length(weights) != length(date_predictions)) {
      stop("The number of weights must match the number of configurations in config_list")
    }

    # Element-wise weighted mean across all configurations
    elementwise_weighted_mean <- purrr::map_dbl(names(date_predictions[[1]]), function(name) {
      # Sum the weighted values for each configuration at the specified name
      sum(purrr::map_dbl(seq_along(date_predictions), ~ date_predictions[[.x]][name] * weights[.x])) / sum(weights)
    })

    # Set names for the resulting vector
    names(elementwise_weighted_mean) <- names(date_predictions[[1]])

    # Return
    return(elementwise_weighted_mean)
  }

  #######################

  ##Calculate ensemble predictions
  #######################
  ###EW
  ew_ensemble_oos_predictions_list <- purrr::map2(ew_unique_oos_dates, ew_weights, calculate_weighted_mean)
  names(ew_ensemble_oos_predictions_list) <- ew_unique_oos_dates

  ###Optimal
  optimal_ensemble_oos_predictions_list <- purrr::map2(optimal_unique_oos_dates, optimal_weights, calculate_weighted_mean)
  names(optimal_ensemble_oos_predictions_list) <- optimal_unique_oos_dates
  #######################

  ##Calculate errors and eval metrics
  #######################


  ###Get Errors and eval metrics
  ###########

  ###Get mean huber/quantile_tau if not provided
  if(is.null(ensemble_huber_delta)){
    ensemble_huber_delta <- 1
  }
  if(is.null(ensemble_quantile_tau)){
    ensemble_quantile_tau <- 0.5
  }

  ###Calculate errors and eval metrics
  ###EW
  ew_ensemble_oos_errors_list_and_eval_metrics <- purrr::map2(ew_ensemble_oos_predictions_list, ew_oos_y_list,
                                                              ~ calculate_eval_metrics(pred = .x, target = .y, #Iterate over
                                                                                       chosen_eval_metric = ensemble_eval_metric, #Ensemble eval metric
                                                                                       huber_delta = ensemble_huber_delta, quantile_tau = ensemble_quantile_tau,  #Huber delta and quantile tau
                                                                                       return_error = TRUE
                                                              ))
  ####Separate errors and eval metrics
  ew_ensemble_oos_errors_list <- lapply(ew_ensemble_oos_errors_list_and_eval_metrics, function(x) x$error)
  ew_ensemble_oos_eval_metrics <- lapply(ew_ensemble_oos_errors_list_and_eval_metrics, function(x) x$df_eval_metric)

  ###Optimal
  optimal_ensemble_oos_errors_list_and_eval_metrics <- purrr::map2(optimal_ensemble_oos_predictions_list, optimal_oos_y_list,
                                                                   ~ calculate_eval_metrics(pred = .x, target = .y, #Iterate over
                                                                                            chosen_eval_metric = ensemble_eval_metric, #Ensemble eval metric
                                                                                            huber_delta = ensemble_huber_delta, quantile_tau = ensemble_quantile_tau,  #Huber delta and quantile tau
                                                                                            return_error = TRUE
                                                                   ))
  ####Separate errors and eval metrics
  optimal_ensemble_oos_errors_list <- lapply(optimal_ensemble_oos_errors_list_and_eval_metrics, function(x) x$error)
  optimal_ensemble_oos_eval_metrics <- lapply(optimal_ensemble_oos_errors_list_and_eval_metrics, function(x) x$df_eval_metric)

  ###########
  #######################

  #Place into ml_metabacktest_results object
  #####################################
  ensemble_ml_backtest_results_list <- list()

  ##EW
  ###Transform the list into a data frame
  ew_ensemble_oos_eval_metrics_df <- purrr::map_dfr(names(ew_ensemble_oos_eval_metrics), function(date) {
    data <- ew_ensemble_oos_eval_metrics[[date]][, -1]  # Remove "Score" column
    data <- as.data.frame(data)
    data$date <- date  # Add date column
    data
  }) %>%
    dplyr::select(date, dplyr::everything()) %>%  # Arrange columns to place date at the start
    tibble::column_to_rownames("date")  # Set date as row names

  ###Get consolidated metrics row
  consolidated_metrics <- as.data.frame(calculate_eval_metrics(
    pred = unlist(ew_ensemble_oos_predictions_list), target = unlist(ew_oos_y_list), #Iterate over
    chosen_eval_metric = ensemble_eval_metric, #Ensemble eval metric
    huber_delta = ensemble_huber_delta, quantile_tau = ensemble_quantile_tau,  #Huber delta and quantile tau
    return_error = FALSE)[-1]) #eliminate score

  rownames(consolidated_metrics) <- "consolidated" #change name

  ew_ensemble_oos_eval_metrics_df <- rbind(ew_ensemble_oos_eval_metrics_df, consolidated_metrics) #Add consolidated metrics row

  ###Create ml_backtest_results obj
  ew_ensemble_results <- new("ml_backtest_results",
                             oos_prediction_list = ew_ensemble_oos_predictions_list,
                             oos_error_list = ew_ensemble_oos_errors_list,
                             oos_y_list = ew_oos_y_list,
                             oos_testing_eval_metrics = ew_ensemble_oos_eval_metrics_df,
                             final_model = new("refit_ml_model",
                                               model = ew_weights[[length(ew_weights)]],
                                               model_class = "ensemble_weights",
                                               ml_algorithm = "ew_ensemble",
                                               best_hyperparameters = NULL,
                                               custom_objective = NULL,
                                               huber_delta = ensemble_huber_delta,
                                               keras_architecture_parameters = NULL
                             ),
                             chosen_eval_metric_validation = NULL,
                             best_hyperparameters = NULL,
                             validation_eval_metrics_hyper_choice = NULL,
                             ml_backtest_workflow = list(
                               config_name = paste0("ew", unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$config_name)), collapse = "_"),
                               config_name_bl = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$config_name)),
                               ml_algorithm = "ew_ensemble",
                               ml_algorithm_bl = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$ml_algorithm)),
                               backtest_type = "meta_learner",
                               dates_covered = ew_unique_oos_dates,
                               n_dates = length(ew_unique_oos_dates),
                               dates_covered_bl = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$dates_covered)),
                               n_dates_bl = length(unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$dates_covered))),
                               ensemble_eval_metric = ensemble_eval_metric,
                               ensemble_huber = ensemble_huber_delta,
                               ensemble_quantile_tau = ensemble_quantile_tau,
                               rebalance_dates = rebalance_dates,
                               first_rebalance_date = rebalance_dates[1],
                               training_sample_size_bl = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$training_sample_size)),
                               validation_sample_size_bl = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$validation_sample_size)),
                               testing_sample_size = length(ew_unique_oos_dates),
                               dates_testing_sample = ew_unique_oos_dates,
                               testing_sample_size_bl = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$testing_sample_size)),
                               dates_testing_sample_bl = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$dates_testing_sample)),
                               nobs = length(unlist(ew_ensemble_oos_predictions_list)),
                               tickers = unique(sapply(ew_ensemble_oos_predictions_list, function(x) names(x))),
                               n_stocks = length(unique(sapply(ew_ensemble_oos_predictions_list, function(x) names(x)))),
                               target_fwd_name = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$target_fwd_name)),
                               target_fwd = target_fwd,
                               target_workflow = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$target_workflow)),
                               target_object = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$target_object)),
                               features = names(ml_backtest_results_list),
                               timestamps = Sys.time(),
                               call = sys.call()
                             )
  )

  ##Optimal
  ###Transform the list into a data frame
  optimal_ensemble_oos_eval_metrics_df <- purrr::map_dfr(names(optimal_ensemble_oos_eval_metrics), function(date) {
    data <- optimal_ensemble_oos_eval_metrics[[date]][, -1]  # Remove "Score" column
    data <- as.data.frame(data)
    data$date <- date  # Add date column
    data
  }) %>%
    dplyr::select(date, everything()) %>%  # Arrange columns to place date at the start
    tibble::column_to_rownames("date")  # Set date as row names

  ###Get consolidated metrics row
  consolidated_metrics <- as.data.frame(calculate_eval_metrics(
    pred = unlist(optimal_ensemble_oos_predictions_list), target = unlist(optimal_oos_y_list), #Iterate over
    chosen_eval_metric = ensemble_eval_metric, #Ensemble eval metric
    huber_delta = ensemble_huber_delta, quantile_tau = ensemble_quantile_tau,  #Huber delta and quantile tau
    return_error = FALSE)[-1]) #eliminate score

  rownames(consolidated_metrics) <- "consolidated" #change name

  optimal_ensemble_oos_eval_metrics_df <- rbind(optimal_ensemble_oos_eval_metrics_df, consolidated_metrics) #Add consolidated metrics row

  ###Create ml_backtest_results obj
  optimal_ensemble_results <- new("ml_backtest_results",
                                  oos_prediction_list = optimal_ensemble_oos_predictions_list,
                                  oos_error_list = optimal_ensemble_oos_errors_list,
                                  oos_y_list = optimal_oos_y_list,
                                  oos_testing_eval_metrics = optimal_ensemble_oos_eval_metrics_df,
                                  final_model = new("refit_ml_model",
                                                    model = optimal_weights[[length(optimal_weights)]],
                                                    model_class = "ensemble_weights",
                                                    ml_algorithm = "optimal_ensemble",
                                                    best_hyperparameters = NULL,
                                                    custom_objective = NULL,
                                                    huber_delta = ensemble_huber_delta,
                                                    keras_architecture_parameters = NULL
                                  ),
                                  chosen_eval_metric_validation = NULL,
                                  best_hyperparameters = NULL,
                                  validation_eval_metrics_hyper_choice = NULL,
                                  ml_backtest_workflow = list(
                                    config_name = paste0("optimal", ensemble_eval_metric, "_",
                                                         unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$config_name)), collapse = "_"),
                                    config_name_bl = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$config_name)),
                                    ml_algorithm = "optimal_ensemble",
                                    ml_algorithm_bl = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$ml_algorithm)),
                                    backtest_type = "meta_learner",
                                    dates_covered = optimal_unique_oos_dates,
                                    n_dates = length(optimal_unique_oos_dates),
                                    dates_covered_bl = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$dates_covered)),
                                    n_dates_bl = length(unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$dates_covered))),
                                    split_method = "expanding",
                                    ensemble_eval_metric = ensemble_eval_metric,
                                    ensemble_huber = ensemble_huber_delta,
                                    ensemble_quantile_tau = ensemble_quantile_tau,
                                    rebalance_dates = rebalance_dates[which(rebalance_dates %in% optimal_unique_oos_dates)],
                                    first_rebalance_date = rebalance_dates[which(rebalance_dates %in% optimal_unique_oos_dates)][1],
                                    training_sample_size = 1,
                                    testing_sample_size = length(optimal_unique_oos_dates),
                                    dates_testing_sample = optimal_unique_oos_dates,
                                    nobs = length(unlist(optimal_ensemble_oos_predictions_list)),
                                    tickers = unique(sapply(optimal_ensemble_oos_predictions_list, function(x) names(x))),
                                    n_stocks = length(unique(sapply(optimal_ensemble_oos_predictions_list, function(x) names(x)))),
                                    target_fwd_name = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$target_fwd_name)),
                                    target_fwd = target_fwd,
                                    target_workflow = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$target_workflow)),
                                    target_object = unique(sapply(ml_backtest_results_list, function(x) x@ml_backtest_workflow$target_object)),
                                    features = names(ml_backtest_results_list),
                                    timestamps = Sys.time(),
                                    call = sys.call()
                                    )
                                  )

                                  #Add to list
                                  ensemble_ml_backtest_results_list$ew_ensemble_config <- ew_ensemble_results
                                  ensemble_ml_backtest_results_list$optimal_ensemble_config <- optimal_ensemble_results

                                  #####################################

                                  return(ensemble_ml_backtest_results_list)

}



