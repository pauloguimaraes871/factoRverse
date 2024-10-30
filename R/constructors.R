#' Create a meta_dataframe Object
#'
#' This function creates an object of class \code{meta_dataframe} from a provided data frame.
#' The data frame must meet specific requirements: it must contain 'id', 'tickers', and 'dates' columns,
#' where 'dates' must be of class \code{Date} and sorted in ascending order. The 'id' column should
#' be constructed as \code{paste0(tickers, "-", dates)}. The function also validates that there are no
#' missing dates, duplicated IDs, or NA values in the required columns.
#'
#' @param data A \code{data.frame} containing the data to be converted to a \code{meta_dataframe}.
#'
#' @return An object of class \code{meta_dataframe} if the input data frame meets all validation criteria.
#' The returned object includes metadata such as the number of unique dates, unique tickers, and
#' total number of observations.
#'
#' @details
#' - The 'id' column is expected to be in the format of \code{paste0(tickers, "-", dates)}.
#' - The 'dates' column must be of class \code{Date} and in ascending chronological order.
#' - The function checks for NA values in the 'id', 'tickers', and 'dates' columns.
#' - The function ensures that there are no gaps in the dates sequence and no duplicated IDs.
#' - The metadata includes the number of unique dates, unique tickers, and total observations.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   id = c("A-2024-01-01", "B-2024-02-01"),
#'   tickers = c("A", "B"),
#'   dates = as.Date(c("2024-01-01", "2024-02-01")),
#'   value = c(10, 20)
#' )
#'
#' # Create a meta_dataframe object
#' meta_df <- create_meta_dataframe(df)
#'
#' @export
create_meta_dataframe <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame")
  }

  required_columns <- c("id", "tickers", "dates")
  if (!all(required_columns %in% names(data))) {
    stop("Data must contain 'id', 'tickers', and 'dates' columns")
  }

  # Check for NA values in the required columns
  if (any(is.na(data[required_columns]))) {
    stop("Columns 'id', 'tickers', or 'dates' contain NA values")
  }

  #Check dates format
  if (!inherits(data$dates, "Date")) {
    stop("The 'dates' column must be of class 'Date'")
  }

  if (!all(diff(unique(data$dates)[order(unique(data$dates))]) >= 0)) {
    stop("Dates must be in ascending chronological order")
  }

  #Check tickers format
  if(any(!is.character(data$tickers))){
    stop("Tickers must be of class character")
  }

  # Check for NA values in remaining columns and report them
  remaining_columns <- setdiff(names(data), required_columns)
  na_remaining <- sapply(data[, remaining_columns], function(col) any(is.na(col)))
  if (any(na_remaining)) {
    message("The following columns contain NA values: ",
            paste(remaining_columns[na_remaining], collapse = ", "))
  }

  # Ensure the 'id' column matches paste0(tickers, "-", dates)
  expected_id <- paste0(data$tickers, "-", data$dates)
  if (!all(data$id == expected_id)) {
    stop("The 'id' column does not match 'tickers-dates')")
  }
  # Ensure no gaps in the dates sequence
  unique_dates <- unique(data$dates)
  full_dates <- seq(min(unique_dates), max(unique_dates), by = "month")
  missing_dates <- setdiff(full_dates, unique_dates)

  if (length(missing_dates) > 0) {
    warning("There are gaps in the dates sequence. Missing dates: ", paste(as.Date(missing_dates), collapse = ", "))
  }

  if (any(duplicated(data$id))) {
    stop("ID column contains duplicated values")
  }

  # Check for NA values in remaining columns and report them
  remaining_columns <- setdiff(names(data), required_columns)
  na_remaining <- sapply(obj[, remaining_columns], function(col) any(is.na(col)))
  if (any(duplicated(remaining_columns))) {
    stop("Column names for variables must be unique")
  }

  # Calculate metadata
  unique_dates_count <- length(unique(data$dates))
  unique_tickers_count <- length(unique(data$tickers))
  total_observations_count <- nrow(data)

  # Initialize workflow slot as an empty list
  # Store metadata and column names
  new("meta_dataframe",
      data = data,
      workflow = list(),
      signals = names(data)[-c(1:3)],
      unique_dates = unique_dates_count,
      unique_tickers = unique_tickers_count,
      n_obs = total_observations_count)
}


#' @title Hyperparameter Tuning Strategy Constructor
#' @description A constructor function to create a tuning_strategy object, based on the specified tuning method.
#' @param tuning_method Character string indicating the hyperparameter tuning method. Must be one of 'grid_search', 'random_search', or 'bayesian_opt'.
#' @param validation_sample_size Numeric value representing the size of the validation sample.
#' @param split_method Character string indicating the split method for the data ('expanding' or 'rolling').
#' @param chosen_eval_metric Character or NULL, specifying the evaluation metric to be optimized.
#' @param early_stop ANY, optional argument for halting criteria.
#' @param n_iter Numeric, number of iterations for 'random_search' or 'bayesian_opt'.
#' @param acq Character string for the acquisition function (for 'bayesian_opt' only).
#' @param init_points Numeric, number of initial random points for Bayesian optimization (for 'bayesian_opt' only).
#' @param k_iter Numeric, number of samples to evaluate during Bayesian optimization (for 'bayesian_opt' only).
#' @return An object of class `grid_search_strategy`, `random_search_strategy`, or `bayesian_opt_strategy`, depending on the selected `tuning_method`.
#' @export
create_tuning_strategy <- function(tuning_method, validation_sample_size, split_method = "expanding", chosen_eval_metric, hyper_grid_domain = NULL, early_stop = NULL,
                                                  n_iter = NULL, acq = NULL, init_points = NULL, k_iter = NULL) {

  # Check if hyper_grid_domain is provided; if not, create an empty one
  if(!tuning_method %in% c("grid_search", "random_search", "bayesian_opt")){
    stop("tuning_method must be one of grid_search, random_search or bayesian_opt")
  }
  if (is.null(hyper_grid_domain)) {
    hyper_grid_domain <-  new("hyper_grid_domain", hyperparameter_list = list())
  } else {
    #If not null, check if it matches
    if(hyper_grid_domain@tuning_method != tuning_method){
      stop("tuning_method in hyper_grid_domain do not match function call")
    }
    if(hyper_grid_domain@ml_algorithm != ml_algorithm){
      stop("ml_algorithm in hyper_grid_domain do not match function call")
    }
  }


  # Check the value of tuning_method and create the appropriate subclass
  if (tuning_method == "grid_search") {
    # Create a grid_search_strategy object
    return(new("grid_search_strategy",
               tuning_method = "grid_search",
               validation_sample_size = validation_sample_size,
               split_method = split_method,
               chosen_eval_metric = chosen_eval_metric,
               hyper_grid_domain = hyper_grid_domain,
               early_stop = early_stop))

  } else if (tuning_method == "random_search") {
    # Create a random_search_strategy object
    if (is.null(n_iter)) {
      stop("n_iter must be provided for random_search.")
    }
    return(new("random_search_strategy",
               tuning_method = "random_search",
               validation_sample_size = validation_sample_size,
               split_method = split_method,
               chosen_eval_metric = chosen_eval_metric,
               hyper_grid_domain = hyper_grid_domain,
               early_stop = early_stop,
               n_iter = n_iter))

  } else if (tuning_method == "bayesian_opt") {
    # Create a bayesian_opt_strategy object
    if (is.null(n_iter) || is.null(acq) || is.null(init_points) || is.null(k_iter)) {
      stop("n_iter, acq, init_points, and k_iter must be provided for bayesian_opt.")
    }
    return(new("bayesian_opt_strategy",
               tuning_method = "bayesian_opt",
               validation_sample_size = validation_sample_size,
               split_method = split_method,
               chosen_eval_metric = chosen_eval_metric,
               hyper_grid_domain = hyper_grid_domain,
               early_stop = early_stop,
               n_iter = n_iter,
               acq = acq,
               init_points = init_points,
               k_iter = k_iter))
  } else {
    stop("Invalid tuning_method. Choose from 'grid_search', 'random_search', or 'bayesian_opt'.")
  }
}


#' Add a `tuning_strategy` to an existing `ml_backtest_config`.
#'
#' This generic function adds an existing `tuning_strategy` to an `ml_backtest_config` object or creates a new `tuning_strategy` if none is provided (i.e., when `tuning_strategy` is `NULL`).
#'
#' @param object A `ml_backtest_config` object to which a tuning strategy will be added.
#' @param tuning_strategy An object of class `tuning_strategy` or `NULL`. If `NULL`, additional parameters must be provided to create a new `tuning_strategy`.
#' @param ... Additional arguments required to create a new `tuning_strategy`, only needed when `tuning_strategy` is `NULL`.
#' @return An updated `ml_backtest_config` object with the specified or newly created `tuning_strategy`.
#' @export
setGeneric("add_tuning_strategy", function(object, tuning_strategy, ...) {
  standardGeneric("add_tuning_strategy")
})



#' @describeIn add_tuning_strategy Add an existing `tuning_strategy` to the `ml_backtest_config`.
#'
#' This method adds a pre-existing `tuning_strategy` to the `ml_backtest_config`. It replaces any existing tuning strategy in the experiment.
#'
#' @param object A `ml_backtest_config` object.
#' @param tuning_strategy An object of class `tuning_strategy` to be added to the `ml_backtest_config`.
#' @return The updated `ml_backtest_config` object with the provided `tuning_strategy`.
#' @export
setMethod("add_tuning_strategy", signature(object = "ml_backtest_config", tuning_strategy = "tuning_strategy"),
          function(object, tuning_strategy) {
            if(object@ml_algorithm != "ols"){
              object@tuning_strategy <- tuning_strategy
            } else {
              stop("OLS does not require tuning.")
            }
            # Validate the object explicitly
            validObject(object)

            return(object)
          })



#' @describeIn add_tuning_strategy Create and add a new `tuning_strategy` to the `ml_backtest_config`.
#'
#' This method is used when `tuning_strategy` is `NULL`. It creates a new tuning strategy based on the provided parameters and adds it to the `ml_backtest_config`.
#'
#' @param object A `ml_backtest_config` object.
#' @param tuning_strategy `NULL`, indicating that a new `tuning_strategy` should be created.
#' @param tuning_method Character string indicating the hyperparameter tuning method. Must be one of 'grid_search', 'random_search', or 'bayesian_opt'.
#' @param validation_sample_size Numeric value representing the size of the validation sample.
#' @param split_method Character string indicating the split method for the data. Options are 'expanding' or 'rolling'. Default is 'expanding'.
#' @param chosen_eval_metric Character or `NULL`, specifying the evaluation metric to be optimized.
#' @param early_stop Optional, stopping criteria for early termination. Can be of any type.
#' @param n_iter Numeric, number of iterations for 'random_search' or 'bayesian_opt'.
#' @param acq Character string specifying the acquisition function for Bayesian optimization (for 'bayesian_opt' only).
#' @param init_points Numeric, number of initial random points for Bayesian optimization (for 'bayesian_opt' only).
#' @param k_iter Numeric, number of samples to evaluate during Bayesian optimization (for 'bayesian_opt' only).
#' @return An updated `ml_backtest_config` object with a newly created `grid_search_strategy`, `random_search_strategy`, or `bayesian_opt_strategy`, depending on the selected `tuning_method`.
#' @export
setMethod("add_tuning_strategy", signature(object = "ml_backtest_config", tuning_strategy = "missing"),
          function(object, tuning_strategy = NULL, tuning_method, validation_sample_size, split_method = "expanding", chosen_eval_metric, hyper_grid_domain = NULL, early_stop = NULL,
                   n_iter = NULL, acq = NULL, init_points = NULL, k_iter = NULL) {

            if(object@ml_algorithm != "ols"){

            # Create a new tuning_strategy object
            object@tuning_strategy <- create_tuning_strategy(tuning_method = tuning_method, validation_sample_size = validation_sample_size, split_method = split_method,
                                                             chosen_eval_metric = chosen_eval_metric, early_stop = early_stop, n_iter = n_iter, acq = acq,
                                                             init_points = init_points, k_iter = k_iter)
            } else {
              stop("OLS does not require tuning.")
            }

            # Validate the object explicitly
            validObject(object)

            return(object)
          }
)



#' Add a Hyperparameter to a `hyper_grid_domain`, whether inside a `ml_backtest_config`, a `tuning_strategy` or on its own.
#'
#' This generic function adds a new hyperparameter to an existing `hyper_grid_domain` object. The function is overloaded to handle different types of hyperparameters for different machine learning algorithms.
#'
#' @param object A `tuning_strategy` or a `hyper_grid_domain` object.
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added. Options are:
#' \itemize{
#'  \item \strong{glmnet}: alpha, lambda.min.ratio
#'  \item \strong{rf}: mtry, num.trees, max.depth, min.bucket
#'  \item \strong{xgb}: min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds
#'  \item \strong{nn}: regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs
#' }
#'
#' @return A `hyper_grid_domain` object with the updated hyperparameters.
#'
#' @examples
#' # Create an initial tuning_strategy object for grid-search
#' grid_search_obj <- create_tuning_strategy(
#'   tuning_method = "grid_search",
#'   validation_sample_size = 1000,
#'   split_method = "expanding",
#'   chosen_eval_metric = "rmse"
#' )
#'
#' # Add hyperparameter for grid_search
#' grid_search_obj <- add_hyperparameter(
#'   grid_search_obj,
#'   hyperparameter = c("alpha", "lambda.min.ratio"),
#'   grid = list(c(0.2, 0.5), c(0.5, 0.2, 0.3))
#' )
#'
#' # Create an initial tuning_strategy object for random_search
#' random_search_obj <- create_tuning_strategy(
#'   tuning_method = "random_search",
#'   validation_sample_size = 1000,
#'   split_method = "rolling",
#'   chosen_eval_metric = "mae",
#'   n_iter = 20
#' )
#'
#' # Add hyperparameters for random_search
#' random_search_obj <- add_hyperparameter(
#'   random_search_obj,
#'   hyperparameter = c("min_child_weight", "max_depth", "subsample", "colsample_bytree", "eta", "alpha", "gamma", "nrounds"),
#'   distribution_choice = c("constant", "uniform", "uniform", "uniform", "uniform", "uniform", "constant", "uniform"),
#'   pars = list(3, c(min = 1L, max = 2L), c(min = 0.25, max = 0.50), c(min = 0.25, max = 0.50), c(min = 0.1, max = 0.2),  c(min = 2, max = 5), 0, c(min = 200L, max = 500L))
#' )
#'
#' # Create an initial tuning_strategy object for bayesian_opt
#' bayesian_opt_obj <- create_tuning_strategy(
#'   tuning_method = "bayesian_opt",
#'   validation_sample_size = 1000,
#'   split_method = "expanding",
#'   chosen_eval_metric = "mape",
#'   n_iter = 50,
#'   acq = "ei",
#'   init_points = 5,
#'   k_iter = 3
#' )
#'
#' # Add hyperparameters for bayesian_opt
#' bayesian_opt_obj <- add_hyperparameter(
#'   bayesian_opt_obj,
#'   hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
#'   bounds = list(c(0,1), c(100L, 1000L), c(2L, 8L), c(1, 5))
#' )
#'
#' # Creating a hyper_grid_domain object for a xgb model
#' hyper_grid_xgb_random <- create_hyper_grid_domain(
#'  tuning_method = "random_search",
#'  ml_algorithm = "xgb"
#'  )
#'
#'  hyper_grid_xgb_random <- add_hyperparameter(
#'  hyperparameter = c("min_child_weight", "max_depth", "subsample", "colsample_bytree",
#'                     "eta", "alpha", "gamma", "nrounds"),
#'  distribution_choice = c("constant", "uniform", "uniform", "uniform",
#'                          "uniform", "uniform", "constant", "uniform"),
#'  pars = list(3, c(min = 1L, max = 2L), c(min = 0.25, max = 0.50),
#'            c(min = 0.25, max = 0.50), c(min = 0.1, max = 0.2),
#'            c(min = 2, max = 5), 0, c(min = 200L, max = 500L))
#'  )
#'
#' @export
setGeneric("add_hyperparameter", function(object, hyperparameter, ...) {
  standardGeneric("add_hyperparameter")
})

#' @describeIn add_hyperparameter Add hyperparameter to `hyper_grid_domain` object
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added. Options are:
#' \itemize{
#'  \item \strong{glmnet}: alpha, lambda.min.ratio
#'  \item \strong{rf}: mtry, num.trees, max.depth, min.bucket
#'  \item \strong{xgb}: min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds
#'  \item \strong{nn}: regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs
#' }
#' @param grid A numeric vector or list of numeric vectors for grid search values (only used for grid_search).
#' @param distribution_choice A character vector indicating the distribution to sample from (only used for random_search).
#' @param pars A numeric named vector or list of numeric named vectors specifying parameter values (only used for random_search).
#' @param bounds A vector of length 2 indicating minimum and maximum bounds for each hyperparameter (only used for bayesian_opt).
#' @param new_hyperparameter_list Used to pass new_hyperparameters_list after specific method at tuning_strategy level.
#'
setMethod("add_hyperparameter",
          signature(object = "hyper_grid_domain"),
          function(object, new_hyperparameter_list) {

            #Get names stored in new_hyperparameter_list
            hyperparameter <- names(new_hyperparameter_list)




            # Merge with existing hyperparameters in object
            if (length(object@hyperparameter_list) != 0) {
              old_hyperparameter_list <- object@hyperparameter_list

              #Take only those that have no substitute in new hyperparameters
              if(any(!names(old_hyperparameter_list) %in% hyperparameter)){
              old_hyperparameter_list_disjoint <- old_hyperparameter_list[which(!names(old_hyperparameter_list) %in% hyperparameter)] #Info
              old_hyperparameter_list_disjoint_names <- names(old_hyperparameter_list)[which(!names(old_hyperparameter_list) %in% hyperparameter)] #Names

                #Re-combine
                for(hyper in old_hyperparameter_list_disjoint_names){
                  new_hyperparameter_list[hyper] <- old_hyperparameter_list[hyper]
                }

                #Re-order
                new_hyperparameter_list <- new_hyperparameter_list[c(old_hyperparameter_list_disjoint_names, hyperparameter)]
              }
            }

            # Update the object
            object@hyperparameter_list <- new_hyperparameter_list

            # Validate the object explicitly
            validObject(object)

            return(object)
          })


#' @describeIn add_hyperparameter Add hyperparameter to `grid_search_strategy` object
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added. Options are:
#' \itemize{
#'  \item \strong{glmnet}: alpha, lambda.min.ratio
#'  \item \strong{rf}: mtry, num.trees, max.depth, min.bucket
#'  \item \strong{xgb}: min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds
#'  \item \strong{nn}: regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs
#' }
#' @param grid A numeric vector or list of numeric vectors for grid search values.
#' @export
setMethod("add_hyperparameter",
          signature(object = "grid_search_strategy"),
          function(object, hyperparameter, grid, ...) {

              if (!is.list(grid)) {
                grid <- list(grid)
              }

              if (!all(sapply(grid, function(x) is.numeric(x) && is.vector(x)))) {
                stop("Grid should contain only numeric data.")
              }

              if (length(hyperparameter) != length(grid)) {
                stop("All hyperparameters should have a grid.")
              }

            new_hyperparameter_list <- grid
            names(new_hyperparameter_list) <- hyperparameter


            #Extract the current object
            current_hyper_grid_domain <- object@hyper_grid_domain
            updated_hyper_grid_domain <- add_hyperparameter(current_hyper_grid_domain, new_hyperparameter_list = new_hyperparameter_list)

            # Update the object
            object@hyper_grid_domain <- updated_hyper_grid_domain

            # Validate the object explicitly
            validObject(object)

            return(object)
          })



#' @describeIn add_hyperparameter Add hyperparameter to `random_search_strategy` object
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added. Options are:
#' \itemize{
#'  \item \strong{glmnet}: alpha, lambda.min.ratio
#'  \item \strong{rf}: mtry, num.trees, max.depth, min.bucket
#'  \item \strong{xgb}: min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds
#'  \item \strong{nn}: regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs
#' }
#' @param distribution_choice A character vector indicating the distribution to sample from (only used for random_search).
#' @param pars A numeric named vector or list of numeric named vectors specifying parameter values (only used for random_search).
#' @export
setMethod("add_hyperparameter",
          signature(object = "random_search_strategy"),
          function(object, hyperparameter, distribution_choice, pars, ...) {

            # Logic for random_search
              if (!is.list(distribution_choice)) {
                distribution_choice <- as.list(distribution_choice) #As list
              }
              if (!is.list(pars)) {
                pars <- list(pars) #list
              }

              if (length(hyperparameter) != length(distribution_choice) || length(hyperparameter) != length(pars)) {
                stop("All hyperparameters should have matching distribution_choice and pars.")
              }

              valid_distributions <- c("uniform", "normal", "lognormal", "constant")
              if (!all(distribution_choice %in% valid_distributions)) {
                stop("Invalid distribution_choice. Choose from 'uniform', 'normal', 'lognormal', or 'constant'.")
              }

              for (i in seq_along(distribution_choice)) {
                dist <- distribution_choice[[i]]
                param <- pars[[i]]

                if (dist == "uniform" && (!all(c("min", "max") %in% names(param)))) {
                  stop("For 'uniform', pars must contain 'min' and 'max'.")
                }
                if (dist == "normal" && (!all(c("mean", "sd") %in% names(param)))) {
                  stop("For 'normal', pars must contain 'mean' and 'sd'.")
                }
                if (dist == "lognormal" && (!all(c("meanlog", "sdlog") %in% names(param)))) {
                  stop("For 'lognormal', pars must contain 'meanlog' and 'sdlog'.")
                }
              }

              new_hyperparameter_list <- mapply(function(hp, dist, param) {
                if (dist == "constant") {
                  list(distribution_choice = dist, value = unname(param))
                } else {
                  list(distribution_choice = dist, pars = param)
                }
              }, hyperparameter, distribution_choice, pars, SIMPLIFY = FALSE)

              names(new_hyperparameter_list) <- hyperparameter


            #Extract the current object
            current_hyper_grid_domain <- object@hyper_grid_domain
            updated_hyper_grid_domain <- add_hyperparameter(current_hyper_grid_domain,
                                                            new_hyperparameter_list = new_hyperparameter_list)

            # Update the object
            object@hyper_grid_domain <- updated_hyper_grid_domain

            # Validate the object explicitly
            validObject(object)

            return(object)
          })


#' @describeIn add_hyperparameter Add hyperparameter to `bayesian_opt_strategy` object
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added. Options are:
#' \itemize{
#'  \item \strong{glmnet}: alpha, lambda.min.ratio
#'  \item \strong{rf}: mtry, num.trees, max.depth, min.bucket
#'  \item \strong{xgb}: min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds
#'  \item \strong{nn}: regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs
#' }
#' @param distribution_choice A character vector indicating the distribution to sample from (only used for random_search).
#' @param pars A numeric named vector or list of numeric named vectors specifying parameter values (only used for random_search).
#' @export
setMethod("add_hyperparameter",
          signature(object = "bayesian_opt_strategy"),
          function(object, hyperparameter, bounds, ...) {


            # Logic for bayesian_opt
              if (!is.list(bounds)) {
                bounds <- list(bounds)
              }

              if (!all(sapply(bounds, function(x) is.numeric(x) && length(x) == 2))) {
                stop("Each hyperparameter must have a bounds vector of length 2 (min and max).")
              }

              if (length(hyperparameter) != length(bounds)) {
                stop("All hyperparameters should have corresponding bounds.")
              }

              new_hyperparameter_list <- bounds
              names(new_hyperparameter_list) <- hyperparameter



            #Extract the current object
            current_hyper_grid_domain <- object@hyper_grid_domain
            updated_hyper_grid_domain <- add_hyperparameter(current_hyper_grid_domain, new_hyperparameter_list = new_hyperparameter_list)

            # Update the object
            object@hyper_grid_domain <- updated_hyper_grid_domain

            # Validate the object explicitly
            validObject(object)

            return(object)
          })

#' @describeIn add_hyperparameter Add Hyperparameter to `ml_backtest_config` object
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added.
#' @param grid A numeric vector or list of numeric vectors for grid search values (only used for grid_search).
#' @param distribution_choice A character vector indicating the distribution to sample from (only used for random_search).
#' @param pars A numeric named vector or list of numeric named vectors specifying parameter values (only used for random_search).
#' @param bounds A vector of length 2 indicating minimum and maximum bounds for each hyperparameter (only used for bayesian_opt).
#' @export
setMethod("add_hyperparameter",
          signature(object = "ml_backtest_config"),
          function(object, hyperparameter, grid = NULL, distribution_choice = NULL, pars = NULL, bounds = NULL) {

            #Extract object
            tuning_strategy <- object@tuning_strategy


            #Add hyperparamete
            updated_tuning_strategy <- add_hyperparameter(tuning_strategy, hyperparameter = hyperparameter,
                                                          grid = grid, distribution_choice = distribution_choice, pars = pars, bounds = bounds)

            # Update the object
            object@tuning_strategy <- updated_tuning_strategy

            # Validate the object explicitly
            validObject(object)


            return(object)
          })


#' Add a `hyper_grid_domain` Object
#'
#' This function adds a `hyper_grid_domain` S4 class to a `tuning_strategy` or a `ml_backtest_config`.
#' It allows users to add a `hyper_grid_domain` already built or extracted from other objects.
#'
#' @param object An object of class `tuning_strategy` or a `ml_backtest_config`.
#' @param hyper_grid_domain An object of class `hyper_grid_domain`.
#'
#' @return The appropriate object with the added `hyper_grid_domain`.
#'
#' @export
setGeneric("add_hyper_grid_domain", function(object, hyper_grid_domain) {
  standardGeneric("add_hyper_grid_domain")
})


#' @describeIn add_hyper_grid_domain Add `hyper_grid_domain` to `tuning_strategy` object
#' @param object An object of class `tuning_strategy`.
#' @param hyper_grid_domain An object of class `hyper_grid_domain`.
#' @export
setMethod("add_hyper_grid_domain",
          signature(object = "tuning_strategy", hyper_grid_domain = "hyper_grid_domain"),
          function(object, hyper_grid_domain) {

            #Add hyper_grid_domain
            object@hyper_grid_domain <- hyper_grid_domain

            # Validate the object explicitly
            validObject(object)

            return(object)
          })


#' @describeIn add_hyper_grid_domain Add `hyper_grid_domain` to `ml_backtest_config` object
#' @param object An object of class `ml_backtest_config`.
#' @param hyper_grid_domain An object of class `hyper_grid_domain`.
#' @export
setMethod("add_hyper_grid_domain",
          signature(object = "ml_backtest_config", hyper_grid_domain = "hyper_grid_domain"),
          function(object, hyper_grid_domain) {

            #Add hyper_grid_domain
            object@tuning_strategy@hyper_grid_domain <- hyper_grid_domain

            # Validate the object explicitly
            validObject(object)

            return(object)
          })


#' @title Create Keras Architecture
#' @description Constructor for creating an instance of `keras_architecture_parameters`.
#'
#' @param nn_optimizer A character string specifying the optimizer to use (e.g., "adam").
#' @param units A numeric value for the number of units in the new layer.
#' @param activation A character string specifying the activation function for the new layer (e.g., "relu").
#' @param batch_norm_option A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'
#' @return An object of class `keras_architecture_parameters`.
#'
#' @export
create_keras_architecture <- function(nn_optimizer, units = NULL, activation = NULL, batch_norm_option = NULL) {

  # Check nn_optimizer is valid
  valid_optimizers <- c("Adam", "RMSProp")
  if (!nn_optimizer %in% valid_optimizers) {
    stop("nn_optimizer should be Adam or RMSProp.")
  }

  #Check format
  if(!is.numeric(units)){
    stop("units should be a numeric value.")
  }
  if(!all(activation %in% c("relu", "sigmoid", "tanh", "softmax"))){
    stop("activation should be relu, sigmoid, tanh, or softmax.")
  }
  if(!all(is.logical(batch_norm_option))){
    stop("batch_norm_option should be a logical value.")
  }

 #Create new_keras_architecture
  new_keras_architecture_parameters <-
  new("keras_architecture_parameters",
      units = units,
      n_layers = length(units),
      activation = activation,
      nn_optimizer = nn_optimizer,
      batch_norm_option = batch_norm_option
  )


  return(new_keras_architecture_parameters)

}


#' @title Add Layer to Keras Architecture
#' @description Method to add a new layer to the Keras architecture.
#'
#' @param object An object of class `keras_architecture_parameters` or `ml_backtest_config`
#' @param units A numeric value for the number of units in the new layer.
#' @param activation A character string specifying the activation function for the new layer (e.g., "relu").
#' @param batch_norm_option A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'
#' @return An updated object of class `keras_architecture_parameters`.
#'
#' @export
setGeneric("add_keras_layer", function(object, units, activation, batch_norm_option) {
  standardGeneric("add_keras_layer")
})

#' @describeIn add_keras_layer Add a keras layer to an object of class `keras_architecture_parameters`
#' @param object An object of class `keras_architecture_parameters`
#' @export
setMethod(
  "add_keras_layer", "keras_architecture_parameters", function(object, units, activation, batch_norm_option) {

    # Check that units are numeric
    if (!all(is.numeric(units))) {
      stop("units should be numeric")
    }

    # Check activation functions
    valid_activations <- c("relu", "sigmoid", "softmax", "softplus", "tanh", "leaky_relu")
    if (!all(activation %in% valid_activations)) {
      stop("activation should be one of relu, sigmoid, softmax, softplus, tanh or leaky_relu.")
    }

    # Check batch_norm_option is logical
    if (!all(batch_norm_option %in% c(TRUE, FALSE))) {
      stop("batch_norm_option should be logical (TRUE or FALSE)")
    }

    #Check length
    if(length(units) != length(activation) || length(units) != length(batch_norm_option)){
      stop("units, activation and batch_norm_option should have matching length.")
    }

    # Update the layers
    object@units <- c(object@units, units)
    object@n_layers <- length(object@units)  # Update the number of layers
    object@activation <- c(object@activation, activation)
    object@batch_norm_option <- c(object@batch_norm_option, batch_norm_option)

    if(length(object@units) > 5){
      warning("factoRverse only supports up to 5 layers currently")
    }

    return(object)  # Return the updated object
  }
)


#' @describeIn add_keras_layer Add a keras layer to an object of class `ml_backtest_config`
#' @param object An object of class `ml_backtest_config`
#' @export
setMethod(
  "add_keras_layer", "ml_backtest_config", function(object, units, activation, batch_norm_option) {

    object <- add_keras_layer(object@keras_architecture_parameters, units = units, activation = activation, batch_norm_option)

    return(object)  # Return the updated object
  }
)

#' @title Add Keras Architecture
#' @description Method to add a `keras_architecture_parameters` to a `ml_backtest_config`.
#'
#' This function allows you to either directly add a pre-existing `keras_architecture_parameters` object or create one dynamically by passing additional arguments.
#' When `keras_architecture_parameters` is not provided, a new one will be created using the values for `nn_optimizer`, `units`, `activation`, and `batch_norm_option` passed via the `...` argument.
#'
#' @param object An object of class `ml_backtest_config`.
#' @param keras_architecture_parameters An object of class `keras_architecture_parameters`, or `NULL` if a new architecture is to be created.
#' @param ... Additional arguments used to create a new `keras_architecture_parameters` when `keras_architecture_parameters` is `NULL`. These arguments must include:
#'   \itemize{
#'     \item \strong{nn_optimizer}: A character string specifying the optimizer to use (e.g., "adam").
#'     \item \strong{units}: A numeric value for the number of units in the new layer.
#'     \item \strong{activation}: A character string specifying the activation function for the new layer (e.g., "relu").
#'     \item \strong{batch_norm_option}: A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'   }
#'
#' @return An updated object of class `ml_backtest_config` with the `keras_architecture_parameters` added.
#' @export
setGeneric("add_keras_architecture", function(object, keras_architecture_parameters, ...) {
  standardGeneric("add_keras_architecture")
})

#' @describeIn add_keras_architecture Add existing `keras_architecture_parameters` object
#'
#' This method allows you to add an already existing `keras_architecture_parameters` object to an `ml_backtest_config`.
#'
#' @param object An object of class `ml_backtest_config`.
#' @param keras_architecture_parameters An existing object of class `keras_architecture_parameters`.
#' @return An updated `ml_backtest_config` object with the provided `keras_architecture_parameters`.
#' @export
setMethod(
  "add_keras_architecture",
  signature(object = "ml_backtest_config", keras_architecture_parameters = "keras_architecture_parameters"),
  function(object, keras_architecture_parameters) {

    object@keras_architecture_parameters <- keras_architecture_parameters

    return(object)  # Return the updated object
  }
)



#' @describeIn add_keras_architecture Dynamically create and add `keras_architecture_parameters` object
#'
#' This method creates a new `keras_architecture_parameters` object dynamically when `keras_architecture_parameters` is not provided.
#' The parameters required to create this object must be passed via `...` and include:
#'   \itemize{
#'     \item \strong{nn_optimizer}: A character string specifying the optimizer to use (e.g., "adam").
#'     \item \strong{units}: A numeric value for the number of units in the new layer.
#'     \item \strong{activation}: A character string specifying the activation function for the new layer (e.g., "relu").
#'     \item \strong{batch_norm_option}: A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'   }
#'
#' @param object An object of class `ml_backtest_config`.
#' @param keras_architecture_parameters Should be `NULL` to dynamically create a new architecture.
#' @param ... Additional parameters used to create the `keras_architecture_parameters`, including:
#'   \itemize{
#'     \item \strong{nn_optimizer}: A character string specifying the optimizer to use (e.g., "adam").
#'     \item \strong{units}: A numeric value for the number of units in the new layer.
#'     \item \strong{activation}: A character string specifying the activation function for the new layer (e.g., "relu").
#'     \item \strong{batch_norm_option}: A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'   }
#' @return An updated `ml_backtest_config` object with the newly created `keras_architecture_parameters`.
#' @export
setMethod(
  "add_keras_architecture",
  signature(object = "ml_backtest_config", keras_architecture_parameters = "missing"),
  function(object, keras_architecture_parameters = NULL, ...) {

    #Extract args to build keras
    args <- list(...)

    # Ensure all required parameters are present
    if (!all(c("nn_optimizer", "units", "activation", "batch_norm_option") %in% names(args))) {
      stop("All required parameters (nn_optimizer, units, activation, batch_norm_option) must be provided.")
    }

    # Create the keras architecture parameters using the additional arguments
    keras_architecture_parameters <- create_keras_architecture(nn_optimizer = args$nn_optimizer,
                                                               units = args$units, activation = args$activation, batch_norm_option = args$batch_norm_option)

    # Assign the created keras architecture to the object
    object@keras_architecture_parameters <- keras_architecture_parameters

    return(object)  # Return the updated object
  }
)


#' @title Create ml_backtest_config Object
#' @description Constructs an ml_backtest_config object.
#'
#' @param target_fwd_name Character string indicating the target variable's forward name.
#' @param ml_algorithm Character string specifying the machine learning algorithm to be used ('glmnet', 'rf', 'xgb', 'nn').
#' @param tuning_strategy An object of class tuning_strategy, specifying the strategy for tuning hyperparameters.
#' @param custom_objective Character string specifying the custom objective function ('squared_error', 'pseudo_huber_error', 'absolute_error') or NULL.
#' @param keras_architecture_parameters List or NULL, providing parameters specific to keras-based neural networks.
#' @param quantile_tau Numeric value indicating the tau parameter used for quantile regression, between 0 and 1.
#' @param huber_delta Numeric value greater than 0, specifying the delta parameter for Huber loss function.
#'
#' @return An ml_backtest_config object.
#' @export
create_ml_backtest_config <- function(target_fwd_name, ml_algorithm = "ols", tuning_strategy = NULL,
                                      custom_objective = "squared_error", keras_architecture_parameters = NULL, quantile_tau = 0.5, huber_delta = 1) {
  # Create the ml_backtest_config object
  new("ml_backtest_config",
      target_fwd_name = target_fwd_name,
      ml_algorithm = ml_algorithm,
      tuning_strategy = tuning_strategy,
      custom_objective = custom_objective,
      keras_architecture_parameters = keras_architecture_parameters,
      quantile_tau = quantile_tau,
      huber_delta = huber_delta
  )
}


#' Create ML Meta Backtest Configuration
#'
#' The `create_ml_metabacktest_config` function creates an `ml_metabacktest_config` object by combining `ml_backtest_config` objects and `tuning_strategy` objects.
#' This allows you to generate all possible combinations of configurations and strategies for running multiple machine learning backtests, possibly in parallel.
#'
#' @param ml_backtest_configs A list of `ml_backtest_config` objects with `tuning_strategy` set to `NULL`.
#' @param tuning_strategies A list of `tuning_strategy` objects (optional).
#' @param ... Additional arguments (not used).
#'
#' @return A `ml_metabacktest_config` object containing all viable combinations of configs and strategies.
#'
#' @examples
#' # Example usage:
#' # Assuming you have ml_backtest_config objects config1, config2
#' # and tuning_strategy objects strategy1, strategy2
#'
#' # First method: Combine configs and strategies
#' meta_config <- create_ml_metabacktest_config(
#'     configs = list(config1, config2),
#'     strategies = list(strategy1, strategy2)
#' )
#'
#' # Second method: Configs already have tuning_strategy set
#' meta_config <- create_ml_metabacktest_config(
#'     configs = list(config1, config2)
#' )
#'
#' @seealso \code{\link{ml_backtest_config}}, \code{\link{tuning_strategy}}, \code{\link{ml_metabacktest_config}}
#'
#' @export
setGeneric("create_ml_metabacktest_config", function(ml_backtest_configs, tuning_strategies, ...) standardGeneric("create_ml_metabacktest_config"))




#' @describeIn create_ml_metabacktest_config Combine configs and strategies
#'
#' This method accepts one or multiple `ml_backtest_config` and one or multiple `tuning_strategy` objects.
#' It combines all possible configurations between the configs and strategies by using the `add_tuning_strategy` method.
#'
#' @param ml_backtest_configs A list of `ml_backtest_config` objects.
#' @param tuning_strategies A list of `tuning_strategy` objects.
#' @param ... Additional arguments (not used).
#'
#' @return An `ml_metabacktest_config` object containing all combinations of configs and strategies.
#'
#' @examples
#' # Assuming you have ml_backtest_config objects config1, config2 (with tuning_strategy = NULL)
#' # and tuning_strategy objects strategy1, strategy2
#' meta_config <- create_ml_metabacktest_config(
#'     configs = list(config1, config2),
#'     strategies = list(strategy1, strategy2)
#' )
#'
#' @export
setMethod("create_ml_metabacktest_config",
          signature(ml_backtest_configs = "list", tuning_strategies = "list"),
          function(ml_backtest_configs, tuning_strategies, ...) {

            # Check that all ml_backtest_configs are ml_backtest_config objects
            if (!all(sapply(ml_backtest_configs, function(x) is(x, "ml_backtest_config")))) {
              stop("All elements in 'ml_backtest_configs' must be 'ml_backtest_config' objects.")
            }

            # Check that all tuning_strategies are tuning_strategy objects
            if (!all(sapply(tuning_strategies, function(x) is(x, "tuning_strategy")))) {
              stop("All elements in 'tuning_strategies' must be 'tuning_strategy' objects.")
            }

            # Prepare to capture the original call argument names
            call_args <- match.call()
            config_names <- as.character(call_args$ml_backtest_configs[-1])  # Remove 'list' call
            strategy_names <- as.character(call_args$tuning_strategies[-1])  # Remove 'list' call

            combined_configs <- list()


            # Iterate through each configuration and strategy, applying only valid combinations
            for (i in seq_along(ml_backtest_configs)) {
              config <- ml_backtest_configs[[i]]

              for (j in seq_along(tuning_strategies)) {
                strategy <- tuning_strategies[[j]]

                # Attempt to add the tuning strategy to the config
                new_config <- tryCatch({
                  add_tuning_strategy(config, strategy)
                }, error = function(e) {
                  NULL  # Return NULL to indicate an invalid combination
                })

                # If combination is successful, add it to combined_configs
                if (!is.null(new_config)) {
                  combined_name <- paste0(config_names[i], "_", strategy_names[j])
                  combined_configs[[combined_name]] <- new_config

                }
              }
            }

            # Check if any combinations were successful
            if (length(combined_configs) == 0) {
              stop("No valid combinations were created. Please check your configurations and strategies.")
            }

           # Create the ml_metabacktest_config object
            meta_config <- new("ml_metabacktest_config", ml_backtest_configs = combined_configs)

            # State the number of valid configurations produced
            cat(sprintf("Created %d valid configurations.\n", length(combined_configs)))

            return(meta_config)
          }
)

#' @describeIn create_ml_metabacktest_config Create meta config from configs with tuning_strategy
#'
#' @param ml_backtest_configs A list of `ml_backtest_config` objects with `tuning_strategy` not `NULL`.
#' @param ... Additional arguments (not used).
#'
#' @return An `ml_metabacktest_config` object containing the provided `ml_backtest_config` objects.
#' @export
setMethod("create_ml_metabacktest_config",
          signature(ml_backtest_configs = "list", tuning_strategies = "missing"),
          function(ml_backtest_configs, ...) {

            # Use match.call() to capture the names of each object passed to ml_backtest_configs
            call_args <- match.call()$ml_backtest_configs

            # Extract the actual names from the call arguments
            if (is.call(call_args) && call_args[[1]] == "list") {
              object_names <- as.character(call_args[-1]) # Exclude the "list" element
            } else {
              stop("Please provide a list of ml_backtest_config objects.")
            }

            # Assign these names to the elements in ml_backtest_configs
            names(ml_backtest_configs) <- object_names

            # Check that all configs are ml_backtest_config objects
            if (!all(sapply(ml_backtest_configs, function(x) is(x, "ml_backtest_config")))) {
              stop("All elements in 'ml_backtest_configs' must be 'ml_backtest_config' objects.")
            }

            # Check that tuning_strategy is not NULL for non-'ols' algorithms
            invalid_configs <- sapply(ml_backtest_configs, function(x) {
              is.null(x@tuning_strategy) && x@ml_algorithm != "ols"
            })
            if (any(invalid_configs)) {
              stop("All 'ml_backtest_config' objects must have 'tuning_strategy' not NULL, except for those with 'ml_algorithm' equal to 'ols'.")
            }

            # Create the ml_metabacktest_config object
            meta_config <- new("ml_metabacktest_config", ml_backtest_configs = ml_backtest_configs)
            return(meta_config)
          }
)


#' @describeIn add_ml_backtest_config Add one or more ml_backtest_config objects to an ml_metabacktest_config
#'
#' @param object An `ml_metabacktest_config` object.
#' @param ... One or more `ml_backtest_config` objects to add.
#'
#' @return An updated `ml_metabacktest_config` object with added configurations.
#' @export
setGeneric("add_ml_backtest_config", function(object, ...) standardGeneric("add_ml_backtest_config"))

setMethod("add_ml_backtest_config", "ml_metabacktest_config", function(object, ...) {

  # Capture the names of the new objects from the function call
  new_configs <- list(...)
  call_args <- match.call(expand.dots = TRUE)
  new_names <- as.character(call_args[-(1:2)])  # Skipping the function name and first arg 'object'

  # Check that all new_configs are complete ml_backtest_config objects
  if(!all(sapply(new_configs, function(x) !is.null(x@tuning_strategy)))) {
    stop("All elements in '...' must be complete (with tuning_strategy) 'ml_backtest_config' objects.")
  }

  # Assign names to the new configurations based on the call argument names
  names(new_configs) <- new_names

  # Combine the current ml_backtest_configs with the new configurations
  object@ml_backtest_configs <- c(object@ml_backtest_configs, new_configs)

  # Validate the object explicitly
  validObject(object)

  # Return the updated object
  return(object)
})



#' @describeIn remove_ml_backtest_config Remove an ml_backtest_config by name from an ml_metabacktest_config
#'
#' @param object An `ml_metabacktest_config` object.
#' @param config_name A character string specifying the name of the `ml_backtest_config` to remove.
#'
#' @return An updated `ml_metabacktest_config` object with the specified configuration removed.
#' @export
setGeneric("remove_ml_backtest_config", function(object, config_name) standardGeneric("remove_ml_backtest_config"))

setMethod("remove_ml_backtest_config", "ml_metabacktest_config", function(object, config_name) {
  # Check that config_name is provided and is a character string
  if (missing(config_name) || !is.character(config_name) || length(config_name) != 1) {
    stop("'config_name' must be a single character string specifying the configuration to remove.")
  }

  # Check if the specified config_name exists in the list
  if (!(config_name %in% names(object@ml_backtest_configs))) {
    stop(paste("No configuration found with the name:", config_name))
  }

  # Remove the specified configuration
  object@ml_backtest_configs <- object@ml_backtest_configs[names(object@ml_backtest_configs) != config_name]

  # Validate the object explicitly
  validObject(object)

  # Return the updated object
  return(object)
})




#' @title Create Portfolio Policies
#' @description Constructs a `portfolio_policies` object.
#' portfolio <- create_portfolio_policies()
create_portfolio_policies <- function() {

  # Create the S4 object
  new("portfolio_policies",
      liquidity_constraint_policy = list(),
      signal_selection_policy = list(),
      turnover_constraint_policy = list(),
      concentration_constraint_policy = list(),
      liquidity_floor_cutoffs = list())
}


#' @title Add Liquidity Constraint Policy
#' @description Adds a liquidity constraint policy to a `portfolio_policies` object.
#'
#' @param portfolio_policies_obj A `portfolio_policies` object to which the liquidity constraint policy will be added.
#' @param liquidity_floor_rule A character string representing the liquidity floor rule (optional).
#' @param liquidity_cap_rules A named numeric vector where names represent liquidity classifications and values represent liquidity caps (optional).
#'
#' @return The updated `portfolio_policies` object with the added liquidity constraint policy.
#'
#' @examples
#' portfolio <- create_portfolio_policies()
#' portfolio <- add_liquidity_constraint_policy(portfolio, liquidity_floor_rule = "micro_caps")
#' portfolio <- add_liquidity_constraint_policy(portfolio, liquidity_cap_rules = c(micro_caps = 0.01))
#' portfolio <- add_liquidity_constraint_policy(portfolio, liquidity_floor_rule = "small_caps") # This should update the liquidity_floor_rule
#'
#' @export
setGeneric("add_liquidity_constraint_policy", function(portfolio_policies_obj, liquidity_floor_rule = NULL, liquidity_cap_rules = NULL) {
  standardGeneric("add_liquidity_constraint_policy")
})

#' @export
setMethod("add_liquidity_constraint_policy", "portfolio_policies", function(portfolio_policies_obj, liquidity_floor_rule = NULL, liquidity_cap_rules = NULL) {
  # Allowed classes
  allowed_classes <- c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")

  # Validate liquidity_floor_rule
  if (!is.null(liquidity_floor_rule)) {
    if (!is.character(liquidity_floor_rule) || length(liquidity_floor_rule) != 1) {
      stop("Error: liquidity_floor_rule must be a single character string.")
    }
    if (!liquidity_floor_rule %in% allowed_classes) {
      stop(sprintf("Error: liquidity_floor_rule must be one of: %s.", paste(allowed_classes, collapse = ", ")))
    }
  }

  # Validate liquidity_cap_rules
  if (!is.null(liquidity_cap_rules)) {
    if (!is.numeric(liquidity_cap_rules) || length(liquidity_cap_rules) == 0) {
      stop("Error: liquidity_cap_rules must be a non-empty named numeric vector.")
    }
    if (!all(names(liquidity_cap_rules) %in% allowed_classes)) {
      stop("Error: All names in liquidity_cap_rules must be one of: micro_caps, small_caps, mid_caps, large_caps, mega_caps.")
    }
  }

  # Initialize new liquidity_constraint_policy
  new_liquidity_constraint_policy <- list(liquidity_floor_rule = NULL)

  # Add liquidity_floor_rule
  if (!is.null(liquidity_floor_rule)) {
    new_liquidity_constraint_policy$liquidity_floor_rule <- liquidity_floor_rule
  } else {
    new_liquidity_constraint_policy$liquidity_floor_rule <- portfolio_policies_obj@liquidity_constraint_policy$liquidity_floor_rule  # Keep the existing rule if it exists
  }

  # Add liquidity_cap_rules
  existing_rules <- portfolio_policies_obj@liquidity_constraint_policy
  existing_liquidity_floor_cap_rules <- existing_rules[!(names(existing_rules) %in% "liquidity_floor_rule")]

  if (!is.null(liquidity_cap_rules)) {
    existing_liquidity_floor_cap_rules_vector <- vapply(existing_liquidity_floor_cap_rules, function(x) x$liquidity_cap, numeric(1))
    names(existing_liquidity_floor_cap_rules_vector) <- sapply(existing_liquidity_floor_cap_rules, function(x) x$liquidity_classification)

    # Combine with new rules
    final_liquidity_floor_cap_rules_vector <- c(existing_liquidity_floor_cap_rules_vector[!(names(existing_liquidity_floor_cap_rules_vector) %in% names(liquidity_cap_rules))],
                                                liquidity_cap_rules)

    # Transform to the right format
    new_liquidity_cap_rules <- setNames(
      lapply(seq_along(final_liquidity_floor_cap_rules_vector), function(i) {
        list(
          liquidity_classification = names(final_liquidity_floor_cap_rules_vector)[i],
          liquidity_cap = final_liquidity_floor_cap_rules_vector[[i]]
        )
      }),
      paste0("liquidity_cap_rule_", seq_along(final_liquidity_floor_cap_rules_vector))
    )
  } else {
    #If there are no new rules, use old policy
    new_liquidity_cap_rules <- existing_liquidity_floor_cap_rules
  }


    # Add liquidity_cap_rules to the new policy
    new_liquidity_constraint_policy <- c(new_liquidity_constraint_policy, new_liquidity_cap_rules)




  # Create the S4 object
  new_portfolio_policies_obj <- new("portfolio_policies",
                                    liquidity_constraint_policy = new_liquidity_constraint_policy,
                                    signal_selection_policy = portfolio_policies_obj@signal_selection_policy,
                                    turnover_constraint_policy = portfolio_policies_obj@turnover_constraint_policy,
                                    concentration_constraint_policy = portfolio_policies_obj@concentration_constraint_policy,
                                    liquidity_floor_cutoffs = portfolio_policies_obj@liquidity_floor_cutoffs)

  return(new_portfolio_policies_obj)
})


#' @title Add Turnover Constraint Policy
#' @description Adds a turnover constraint policy to a `portfolio_policies` object.
#'
#' @param portfolio_policies_obj A `portfolio_policies` object to which the liquidity constraint policy will be added.
#' @param turnover_rule A list with the following structure, specifying the turnover rule:
#' #' \itemize{
#'   \item \strong{liquidity_classification:} One of "micro_caps", "small_caps", "mid_caps", "large_caps" or "mega_caps"
#'   \item \strong{turnover_cap:} A number indicating the turnover_cap
#'   \item \strong{top_stock_quantile_buffer:} A number indicating the minimum signal quantile
#'   }
#'
#' @return The updated `portfolio_policies` object with the added liquidity constraint policy.
#'
#' @examples
#' portfolio <- create_portfolio_policies()
#' portfolio <- add_turnover_constraint_policy(portfolio, turnover_rule = list(liquidity_classification = "micro_caps", turnover_cap = 0.01, top_stock_quantile_buffer = 0.66))
#'
#' @export
setGeneric("add_turnover_constraint_policy", function(portfolio_policies_obj, turnover_rules) {
  standardGeneric("add_turnover_constraint_policy")
})

#' @export
setMethod("add_turnover_constraint_policy", "portfolio_policies", function(portfolio_policies_obj, turnover_rules) {

  # Allowed classes
  allowed_classes <- c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")


  # Validate turnover_rules
  if(!is.list(turnover_rules)){
    stop("turnover_rules must be a list or list of lists")
  }

  ##Create function to extract vector dependengin on nesting structure
  unnest_turnover_rules <- function(argument){
    if(all(sapply(turnover_rules, function(x) is.list(x)))){
      return(turnover_rules %>% sapply(function(x) x[[argument]]))
    } else {
      return(turnover_rules[[argument]])
    }
  }

  ##liquidity classification
  liquidity_classification_vector <- unnest_turnover_rules("liquidity_classification")

  if (!all(is.character(liquidity_classification_vector))) {
    stop("Error: liquidity_classification must be a character string.")
  }
  if (!all(liquidity_classification_vector %in% allowed_classes)) {
    stop(sprintf("Error: liquidity_classification must be one of: %s.", paste(allowed_classes, collapse = ", ")))
  }

  ##turnover_cap
  turnover_cap_vector <- unnest_turnover_rules("turnover_cap")
  if(!all(is.numeric(turnover_cap_vector))){
    stop("Error: turnover_cap must be numeric")
  }

  ##top_stock_quantile_buffer
  top_stock_quantile_buffer_vector <- unnest_turnover_rules("top_stock_quantile_buffer")
  if(!all(is.numeric(top_stock_quantile_buffer_vector))){
    stop("Error: top_stock_quantile_buffer must be numeric")
  }

  # Add turnover_cap_rules
  ##Get existing rules
  existing_rules <- portfolio_policies_obj@turnover_constraint_policy
  existing_rules_names <- sapply(existing_rules, function(x) x$liquidity_classification)

  #Combine old and new rules
  if(length(existing_rules) != 0){
    #Extract existing rules not to be overwritten
    unique_existing_rules <- existing_rules[[which(!existing_rules_names %in% sapply(turnover_rules, function(x) x$liquidity_classification))]]

      ###Check if unique_existing_rules is not list of lists
      if(!all(sapply(unique_existing_rules, function(x) is.list(x)))){
        final_rules <- list(unique_existing_rules) #Create list of lists obj
      } else {
        final_rules <- unique_existing_rules #do noth
      }

    #Combine new and old
    if(all(sapply(turnover_rules, function(x) is.list(x)))){

       #If new object is a list of lists, extract individually and add
      for(new_rule in turnover_rules){
          final_rules[[length(final_rules) + 1]] <- new_rule
      }
    }
      else {
        #In case it is not a list of lists
        final_rules[[length(final_rules) + 1]] <- new_rule
      }
    }

  ###In case there are no old rules

  else {

    if(all(sapply(turnover_rules, function(x) is.list(x)))){
      final_rules <- turnover_rules
    } else {
      final_rules <- list(turnover_rules)
    }
  }

    # Transform to the right format
    n <- length(final_rules)

    # Rename
    names(final_rules) <- paste0("buffer_zone_", seq_len(n))


    # Create the S4 object
    new_portfolio_policies_obj <- new("portfolio_policies",
                                      liquidity_constraint_policy = portfolio_policies_obj@liquidity_constraint_policy,
                                      signal_selection_policy = portfolio_policies_obj@signal_selection_policy,
                                      turnover_constraint_policy = final_rules,
                                      concentration_constraint_policy = portfolio_policies_obj@concentration_constraint_policy,
                                      liquidity_floor_cutoffs = portfolio_policies_obj@liquidity_floor_cutoffs)

    return(new_portfolio_policies_obj)
  })


#' @title Add Concentration Constraint Policy
#' @description Adds a concentration constraint policy to a `portfolio_policies` object.
#'
#' @param portfolio_policies_obj A `portfolio_policies` object to which the concentration constraint policy will be added.
#' @param benchmark A character string representing benchmark based on which concentration policy will be applied.
#' @param max_abs_active_individual_weight A number indicating the absolute weight differential from benchmark weights.
#' @param max_abs_active_group_weight A named vector indicating absolute group (sector) weight differentials in relation to the benchmark.
#'
#' @return The updated `portfolio_policies` object with the added liquidity constraint policy.
#'
#' @examples
#' portfolio <- create_portfolio_policies()
#' portfolio <- add_concentration_constraint_policy(portfolio, benchmark = "IBOV", max_abs_active_individual_weight = 0.05, max_abs_active_group_weight = c(Sector = 0.1, Subsector = 0.5))
#'
#' @export
setGeneric("add_concentration_constraint_policy", function(portfolio_policies_obj, benchmark = NULL, max_abs_active_individual_weight = NULL, max_abs_active_group_weight = NULL) {
  standardGeneric("add_concentration_constraint_policy")
})

#' @export
setMethod("add_concentration_constraint_policy",  "portfolio_policies",
          function(portfolio_policies_obj, benchmark = NULL, max_abs_active_individual_weight = NULL, max_abs_active_group_weight = NULL) {

  #Validate arguments
  ##Benchmark
  if(!is.null(benchmark)){
    if(!is.character(benchmark)){
      stop("benchmark should be a character")
    }
  }

  ##Max Absolute Active Individual Weight
  if(!is.null(max_abs_active_individual_weight)){
    if(!is.numeric(max_abs_active_individual_weight)){
      stop("max_abs_active_individual_weight should be numeric")
    }
  }

  ##Max Absolute Active Group Weight
  if(!is.null(max_abs_active_group_weight)){
    if(!all(is.numeric(max_abs_active_group_weight))){
      stop("max_abs_active_group_weight should be a numeric vector")
    }
    if(is.null(names(max_abs_active_group_weight))){
      stop("max_abs_active_group_weight should include names")
    }
  }

  new_concentration_constraint_policy <- list()

  #Set benchmark
  if(!is.null(benchmark)){
    new_concentration_constraint_policy$benchmark <- benchmark
  } else {
    if(!is.null(portfolio_policies_obj@benchmark)){
      new_concentration_constraint_policy$benchmark <- portfolio_policies_obj@benchmark
    } else {
      new_concentration_constraint_policy$benchmark <- NULL
    }
  }

  #Set max_abs_active_individual_weight
  if(!is.null(max_abs_active_individual_weight)){
    new_concentration_constraint_policy$max_abs_active_individual_weight <- max_abs_active_individual_weight
  } else {
    if(!is.null(portfolio_policies_obj@max_abs_active_individual_weight)){
      new_concentration_constraint_policy$max_abs_active_individual_weight <- portfolio_policies_obj@max_abs_active_individual_weight
    } else {
      new_concentration_constraint_policy$max_abs_active_individual_weight <- NULL
    }
  }

  #Set max_abs_active_group_weight
  if(!is.null(max_abs_active_group_weight)){
    new_concentration_constraint_policy$max_abs_active_group_weight <- max_abs_active_group_weight
  } else {
    if(!is.null(portfolio_policies_obj@max_abs_active_group_weight)){
      new_concentration_constraint_policy$max_abs_active_group_weight <- portfolio_policies_obj@max_abs_active_group_weight
    } else {
      new_concentration_constraint_policy$max_abs_active_group_weight <- NULL
    }
  }

  # Create the S4 object
  new_portfolio_policies_obj <- new("portfolio_policies",
                                    liquidity_constraint_policy = portfolio_policies_obj@liquidity_constraint_policy,
                                    signal_selection_policy = portfolio_policies_obj@signal_selection_policy,
                                    turnover_constraint_policy = portfolio_policies_obj@turnover_constraint_policy,
                                    concentration_constraint_policy = new_concentration_constraint_policy,
                                    liquidity_floor_cutoffs = portfolio_policies_obj@liquidity_floor_cutoffs)

  return(new_portfolio_policies_obj)

})




#' @title Add Signal Selection Policy
#' @description Adds a signal selection policy to a `portfolio_policies` object.
#'
#' @param portfolio_policies_obj A `portfolio_policies` object to which the signal selection policy will be added.
#' @param signal_blending_method A method for blending signals.
#' @param sb_benchmark_weighting A weighting strategy for the benchmark.
#' @param max_abs_active_individual_weight A number indicating the absolute weight differential from benchmark weights.
#' @param max_abs_active_group_weight A named vector indicating absolute group (sector) weight differentials in relation to the benchmark.
#' @param p_correction_method Method for p-value correction.
#' @param chosen_signals A vector of chosen signals.
#' @param signal_positions A data structure indicating positions of signals.
#' @param signal_significance_threshold A threshold for signal significance.
#' @param chosen_informative_data A data structure with informative data.
#' @param chosen_sb_metric A metric chosen for signal blending.
#' @param priors_type Type of priors to use.
#' @param data_availability_cutoff A cutoff for data availability.
#'
#' @return The updated `portfolio_policies` object with the added signal selection policy.
#'
#' @examples
#' portfolio <- create_portfolio_policies()
#' portfolio <- add_signal_selection_policy(portfolio, signal_blending_method = "method1", ...)
#'
#' @export
setGeneric("add_signal_selection_policy", function(portfolio_policies_obj,
                                                   #Signal Selection
                                                   chosen_signals = NULL, signal_positions = NULL,
                                                   #How to blend signals?
                                                   signal_blending_method = NULL, chosen_sb_metric = NULL,
                                                   #Restrictions on weights
                                                   sb_benchmark_weighting_method = NULL, max_abs_active_individual_weight = NULL, max_abs_active_group_weight = NULL,
                                                   #How to select signals
                                                   p_correction_method = "none", signal_significance_threshold = 0.05, data_availability_cutoff = 60,
                                                   priors_type = NULL, priors_informative_data = NULL) {

  standardGeneric("add_signal_selection_policy")
})

#' @export
setMethod("add_signal_selection_policy", "portfolio_policies", function(portfolio_policies_obj,
                                                                        #Signal Selection
                                                                        chosen_signals = NULL, signal_positions = NULL,
                                                                        #How to blend signals?
                                                                        signal_blending_method = NULL, chosen_sb_metric = NULL,
                                                                        #Restrictions on weights
                                                                        sb_benchmark_weighting_method = "theme_sb", max_abs_active_individual_weight = NULL, max_abs_active_group_weight = NULL,
                                                                        #How to select signals
                                                                        p_correction_method = "none", signal_significance_threshold = 0.05, data_availability_cutoff = 60,
                                                                        priors_type = NULL, priors_informative_data = NULL
                                                                       ) {

  # Validate arguments
    ## chosen_signals
    if(!is.null(chosen_signals)){
      if (!all(is.character(chosen_signals))){
        stop("chosen_signals should be a character vector.")
      }
      #Check if signal positions is also updated
      if(is.null(signal_positions)){
        stop("please always update chosen_signals with signal_positions to ensure consistency")
      }

      ## signal_position
      if (!all(is.character(signal_positions))){
        stop("signal_positions should be a character vector.")
      }
      if(length(signal_positions) != length(chosen_signals)){
        stop("lengths of signal_positions and chosen_signals should match.")
      }

      ## check for appropriate signal_blending_method
      if(length(chosen_signals) > 1){
        #If more than one signal is chosen, signal_blending_method can't be NULL
        if(any(!is.null(signal_blending_method), !is.null(portfolio_policies_obj@signal_selection_policy))){
          #Either the new signal_blending_method or the old one must be different from NULL
        }
      }
    }

    ## Signal Blending Method
    if (!is.null(signal_blending_method)){
      ###Character
      if(!is.character(signal_blending_method)) {
        stop("signal_blending_method should be a character")
      }
      ###Correct choice
      if(!signal_blending_method %in% c("EW", "SW", "RP", "MTO", "ML")){
        stop("signal_blending_method should be one of EW, SW, RP, MTO or ML")
      }
    }

    ##chosen_sb_metric
    if (!is.null(chosen_sb_metric)){
      ###Character
      if(!is.character(chosen_sb_metric)) {
        stop("chosen_sb_metric should be a character")
      }
      ###Correct choice
      if(!chosen_sb_metric %in% c("mean_active_return", "IR", "alpha", "AP", "treynor")){
        stop("chosen_sb_metric should be one of mean_active_return, IR, alpha, AP or treynor")
      }
    }

    ## Signal Blending Benchmark Weighting Method
    if (!is.null(sb_benchmark_weighting_method)){
      ###Character
      if(!is.character(sb_benchmark_weighting_method)) {
        stop("sb_benchmark_weighting_method should be a character")
      }
      ###Correct choice
      if(!sb_benchmark_weighting_method %in% c("individual_sb", "theme_sb")){
        stop("sb_benchmark_weighting_method should be one of individual_sb or theme_sb")
      }
    }

    ##max_abs_active_individual_weight
    if(!is.null(max_abs_active_individual_weight)){
      if(!is.numeric(max_abs_active_individual_weight)){
        stop("max_abs_active_individual_weight should be numeric")
      }
    }

    ##max_abs_active_group_weight
    if(!is.null(max_abs_active_group_weight)){
      if(!is.numeric(max_abs_active_group_weight)){
        stop("max_abs_active_group_weight should be numeric")
      }
    }


    ## p_correction_method
    if (!is.null(p_correction_method)){
      ###Character
      if(!is.character(p_correction_method)) {
        stop("p_correction_method should be a character")
      }
      ###Correct choice
      if(!p_correction_method %in% c("bayesian", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")){
        stop("p_correction_method should be one of bayesian, holm, hochberg, hommel, bonferroni, BH, BY, fdr or none")
      }
    }

    ##signal_significance_threshold
    if(!is.numeric(signal_significance_threshold)){
      stop("signal_significance_threshold should be numeric")
    }

    ##data_availability_cutoff
    if(!is.numeric(data_availability_cutoff)){
      stop("data_availability_cutoff should be numeric")
    }

    ##priors_type
    if (!is.null(priors_type)){
      ###Character
      if(!is.character(priors_type)) {
        stop("priors_type should be a character")
      }
      ###Correct choice
      if(!priors_type %in% c("uninformative", "all", "mean", "user")){
        stop("priors_type should be one of uninformative, all, mean or user")
      }
    }

    ##priors_informative_data
    if (!is.null(priors_informative_data)){
      ###Character
      if(!is.character(priors_informative_data)) {
        stop("priors_informative_data should be a character")
      }
      ###Correct choice
      if(!priors_informative_data %in% c("jkp_emerging", "cz_global")){
        stop("priors_type should be one of jkp_emerging or cz_global")
      }
    }


  # Construct new_signal_selection_policy list
  new_signal_selection_policy <- list(
    chosen_signals = if (!is.null(chosen_signals)) chosen_signals else portfolio_policies_obj@signal_selection_policy$chosen_signals,
    signal_positions = if (!is.null(signal_positions)) signal_positions else portfolio_policies_obj@signal_selection_policy$signal_positions,
    signal_blending_method = if (!is.null(signal_blending_method)) signal_blending_method else portfolio_policies_obj@signal_selection_policy$signal_blending_method,
    chosen_sb_metric = if (!is.null(chosen_sb_metric)) chosen_sb_metric else portfolio_policies_obj@signal_selection_policy$chosen_sb_metric,
    sb_benchmark_weighting_method = if (!missing(sb_benchmark_weighting_method) && !is.null(sb_benchmark_weighting_method)) {
      sb_benchmark_weighting_method
    } else if (!is.null(portfolio_policies_obj@signal_selection_policy$sb_benchmark_weighting_method)) {
      portfolio_policies_obj@signal_selection_policy$sb_benchmark_weighting_method
    } else {
      "theme_sb"
    },
    max_abs_active_individual_weight = if (!is.null(max_abs_active_individual_weight)) max_abs_active_individual_weight else portfolio_policies_obj@signal_selection_policy$max_abs_active_individual_weight,
    max_abs_active_group_weight = if (!is.null(max_abs_active_group_weight)) max_abs_active_group_weight else portfolio_policies_obj@signal_selection_policy$max_abs_active_group_weight,
    p_correction_method = if (!missing(p_correction_method) && p_correction_method != "none") {
      p_correction_method
    } else if (!is.null(portfolio_policies_obj@signal_selection_policy$p_correction_method)) {
      portfolio_policies_obj@signal_selection_policy$p_correction_method
    } else {
      "none"
    },
    signal_significance_threshold = if (!is.null(signal_significance_threshold)) signal_significance_threshold else portfolio_policies_obj@signal_selection_policy$signal_significance_threshold,
    data_availability_cutoff = if (!is.null(data_availability_cutoff)) data_availability_cutoff else portfolio_policies_obj@signal_selection_policy$data_availability_cutoff,
      priors_type = if (!is.null(p_correction_method) && p_correction_method == "bayesian" && is.null(priors_type)) {
    "uninformative"
  } else if (!is.null(priors_type)) {
    priors_type
  } else {
    portfolio_policies_obj@signal_selection_policy$priors_type
  },
    priors_informative_data = if (!is.null(priors_informative_data)) priors_informative_data else portfolio_policies_obj@signal_selection_policy$priors_informative_data
  )

  # Final consistency checks

  # Check if max_abs_active_individual_weight is set but signal_blending_method is not "MTO"
  if (!is.null(new_signal_selection_policy$signal_blending_method) &&
      new_signal_selection_policy$signal_blending_method != "MTO" &
      !is.null(new_signal_selection_policy$max_abs_active_individual_weight)) {
    stop("signal_blending_method must be equal to MTO if max_abs_active_individual_weight is set")
  }

  # Check if max_abs_active_group_weight is set but signal_blending_method is not "MTO"
  if (!is.null(new_signal_selection_policy$signal_blending_method) &&
      new_signal_selection_policy$signal_blending_method != "MTO" &
      !is.null(new_signal_selection_policy$max_abs_active_group_weight)) {
    stop("signal_blending_method must be equal to MTO if max_abs_active_group_weight is set")
  }

  # Check if chosen_sb_metric is NULL when signal_blending_method is "SW" or "MTO"
  if (!is.null(new_signal_selection_policy$signal_blending_method) &&
      new_signal_selection_policy$signal_blending_method %in% c("SW", "MTO") &
      is.null(new_signal_selection_policy$chosen_sb_metric)) {
    stop("chosen_sb_metric can't be NULL if signal_blending_method is SW or MTO")
  }

  # Check if chosen_sb_metric is NULL when signal_blending_method is "SW" or "MTO"
  if (!is.null(new_signal_selection_policy$signal_blending_method) &&
      !new_signal_selection_policy$signal_blending_method %in% c("SW", "MTO") &
      !is.null(new_signal_selection_policy$chosen_sb_metric)) {
    stop("chosen_sb_metric is only needed if signal_blending_method is SW or MTO")
  }

  # Check for p_correction_method = "bayesian" and valid priors_type and priors_informative_data
  if (!is.null(new_signal_selection_policy$p_correction_method) &&
      new_signal_selection_policy$p_correction_method == "bayesian" &&
      !new_signal_selection_policy$priors_type %in% c("uninformative", "user") &&
      is.null(new_signal_selection_policy$priors_informative_data)) {
    stop("priors_informative_data can't be NULL if p_correction_method is bayesian and priors_type is not uninformative or user")
  }

  # Create the updated portfolio_policies object
  new_portfolio_policies_obj <- new("portfolio_policies",
                                    liquidity_constraint_policy = portfolio_policies_obj@liquidity_constraint_policy,
                                    signal_selection_policy = new_signal_selection_policy,
                                    turnover_constraint_policy = portfolio_policies_obj@turnover_constraint_policy,
                                    concentration_constraint_policy = portfolio_policies_obj@concentration_constraint_policy,
                                    liquidity_floor_cutoffs = portfolio_policies_obj@liquidity_floor_cutoffs)

  return(new_portfolio_policies_obj)
})


#' @title Add Liquidity Floor Cutoffs
#' @description Add liquidity floor cutoffs to the portfolio_policies object.
#' @param portfolio_policies_obj An S4 object of class `portfolio_policies`.
#' @param liquidity_metric A character string representing the liquidity metric.
#' @param cutoffs A numeric vector of length 5 representing the cutoff values for
#' micro_caps, small_caps, mid_caps, large_caps, and mega_caps.
#' @return An updated `portfolio_policies` object.
#' @export
setGeneric("add_liquidity_floor_cutoffs", function(portfolio_policies_obj, liquidity_metric, cutoffs) {
  standardGeneric("add_liquidity_floor_cutoffs")
})

#' @export
setMethod("add_liquidity_floor_cutoffs", "portfolio_policies", function(portfolio_policies_obj,
                                                                        liquidity_metric,
                                                                        cutoffs) {
  # Validate inputs
  if (!is.character(liquidity_metric) || length(liquidity_metric) != 1) {
    stop("liquidity_metric should be a single character string.")
  }

  if (!is.numeric(cutoffs) || length(cutoffs) != 5 || any(cutoffs <= 0)) {
    stop("cutoffs must be a numeric vector of length 5, and all values must be positive.")
  }

  # Check if cutoffs are in ascending order
  if (!all(diff(cutoffs) >= 0)) {
    stop("cutoffs must be provided in ascending order.")
  }

  # Initialize liquidity_floor_cutoffs if it doesn't exist
  if (is.null(portfolio_policies_obj@liquidity_floor_cutoffs)) {
    portfolio_policies_obj@liquidity_floor_cutoffs <- list(
      micro_caps = numeric(),
      small_caps = numeric(),
      mid_caps = numeric(),
      large_caps = numeric(),
      mega_caps = numeric()
    )
  }

  # Update each market capitalization class with the new liquidity metric cutoffs
  portfolio_policies_obj@liquidity_floor_cutoffs$micro_caps[liquidity_metric] <- cutoffs[1]
  portfolio_policies_obj@liquidity_floor_cutoffs$small_caps[liquidity_metric] <- cutoffs[2]
  portfolio_policies_obj@liquidity_floor_cutoffs$mid_caps[liquidity_metric] <- cutoffs[3]
  portfolio_policies_obj@liquidity_floor_cutoffs$large_caps[liquidity_metric] <- cutoffs[4]
  portfolio_policies_obj@liquidity_floor_cutoffs$mega_caps[liquidity_metric] <- cutoffs[5]

  # Return the updated portfolio policies object
  return(portfolio_policies_obj)
})



