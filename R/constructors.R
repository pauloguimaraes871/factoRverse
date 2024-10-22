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
#' @description A constructor function to create a hyperparameter_tuning_strategy object, based on the specified tuning method.
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
create_hyperparameter_tuning_strategy <- function(tuning_method,
                                                  ml_algorithm,
                                                  validation_sample_size,
                                                  split_method = "expanding",
                                                  chosen_eval_metric,
                                                  hyper_grid_domain = NULL,
                                                  early_stop = NULL,
                                                  n_iter = NULL,
                                                  acq = NULL,
                                                  init_points = NULL,
                                                  k_iter = NULL) {

  # Check if hyper_grid_domain is provided; if not, create an empty one
  if(!tuning_method %in% c("grid_search", "random_search", "bayesian_opt")){
    stop("tuning_method must be one of grid_search, random_search or bayesian_opt")
  }
  if(!ml_algorithm %in% c("glmnet", "rf", "xgb", "nn")){
    stop("ml_algorithm must be one of glmnet, rf, xgb or nn")
  }
  if (is.null(hyper_grid_domain)) {
    hyper_grid_domain <- new("hyper_grid_domain", ml_algorithm = ml_algorithm, tuning_method = tuning_method, hyperparameter_list = list())
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
               ml_algorithm = ml_algorithm,
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
               ml_algorithm = ml_algorithm,
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
               ml_algorithm = ml_algorithm,
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


#' Add a Hyperparameter to a `hyper_grid_domain`, whether inside a `hyperparameter_tuning_strategy` or not.
#'
#' This generic function adds a new hyperparameter to an existing `hyper_grid_domain` object, whether it is inside a `hyperparameter_tuning_strategy` or not.
#'
#' @param object A `hyperparameter_tuning_strategy` or a `hyper_grid_domain` object.
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
#' # Create an initial hyperparameter_tuning_strategy object for grid-search
#' grid_search_obj <- create_hyperparameter_tuning_strategy(
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
#' # Create an initial hyperparameter_tuning_strategy object for random_search
#' random_search_obj <- create_hyperparameter_tuning_strategy(
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
#' # Create an initial hyperparameter_tuning_strategy object for bayesian_opt
#' bayesian_opt_obj <- create_hyperparameter_tuning_strategy(
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

#' @title Add Hyperparameter for `hyper_grid_domain`
#' @description Adds a hyperparameter to the object based on the specified tuning method.
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added.
#' @param grid A numeric vector or list of numeric vectors for grid search values (only used for grid_search).
#' @param distribution_choice A character vector indicating the distribution to sample from (only used for random_search).
#' @param pars A numeric named vector or list of numeric named vectors specifying parameter values (only used for random_search).
#' @param bounds A vector of length 2 indicating minimum and maximum bounds for each hyperparameter (only used for bayesian_opt).
#' @export
setMethod("add_hyperparameter",
          signature(object = "hyper_grid_domain"),
          function(object, hyperparameter,
                   grid = NULL, distribution_choice = NULL, pars = NULL, bounds = NULL) {

            #Check if tuning_method is correct
            if(!object@tuning_method %in% c("grid_search", "random_search", "bayesian_opt")){
              stop("Invalid tuning_method. Only 'grid_search', 'random_search', and 'bayesian_opt' are supported.")
            }

            # Logic for grid_search
            if(object@tuning_method == "grid_search"){
            if (is.null(grid)) {
              stop("grid can't be missing when tuning method is grid_search")
            }

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
            }

            # Logic for random_search
            if(object@tuning_method == "random_search"){
              if (is.null(distribution_choice) || is.null(pars)) {
                stop("distribution_choice and pars can't be missing when tuning_method is random_search")
              }

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
            }

            # Logic for bayesian_opt
            if(object@tuning_method == "bayesian_opt"){
              if (is.null(bounds)) {
                stop("bounds can't be missing when tuning_method is bayesian_opt")
              }

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
            }

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


#' @title Add Hyperparameter for `hyperparameter_tuning_strategy`
#' @description Adds hyperparameters to the `hyper_grid_domain` inside the `hyperparameter_tuning_strategy` based on the specified tuning method.
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added.
#' @param grid A numeric vector or list of numeric vectors for grid search values (only used for grid_search).
#' @param distribution_choice A character vector indicating the distribution to sample from (only used for random_search).
#' @param pars A numeric named vector or list of numeric named vectors specifying parameter values (only used for random_search).
#' @param bounds A vector of length 2 indicating minimum and maximum bounds for each hyperparameter (only used for bayesian_opt).
#' @export
setMethod("add_hyperparameter",
          signature(object = "hyperparameter_tuning_strategy"),
          function(object, hyperparameter,
                   grid = NULL, distribution_choice = NULL, pars = NULL, bounds = NULL) {

            #Extract the current object
            current_hyper_grid_domain <- object@hyper_grid_domain
            updated_hyper_grid_domain <- add_hyperparameter(current_hyper_grid_domain, hyperparameter = hyperparameter,
                                                            grid = grid, distribution_choice = distribution_choice, pars = pars, bounds = bounds)

            # Update the object
            object@hyper_grid_domain <- updated_hyper_grid_domain

            # Validate the object explicitly
            validObject(object)

            return(object)
          })


#' Create a `hyper_grid_domain` Object
#'
#' This function creates an instance of the `hyper_grid_domain` S4 class.
#' It allows users to specify relevant hyperparameters.
#'
#' @param hyperparameter A named list representing the hyperparameter to be added. Should be as follows:
#' \itemize{
#'  \item \strong{glmnet}: alpha, lambda.min.ratio
#'  \item \strong{rf}: mtry, num.trees, max.depth, min.bucket
#'  \item \strong{xgb}: min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds
#'  \item \strong{nn}: regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs
#' }
#' @param tuning_method Character string indicating the hyperparameter tuning method. Must be one of 'grid_search', 'random_search', or 'bayesian_opt'.
#' @param grid A numeric vector or list of numeric vectors for grid search values (only used for grid_search).
#' @param distribution_choice A character vector indicating the distribution to sample from (only used for random_search).
#' @param pars A numeric named vector or list of numeric named vectors specifying parameter values (only used for random_search).
#' @param bounds A vector of length 2 indicating minimum and maximum bounds for each hyperparameter (only used for bayesian_opt).
#'
#' @return An instance of the `hyper_grid_domain` S4 class.
#'
#' @examples
#' # Creating a hyper_grid_domain object for a xgb model
#' hyper_grid_xgb_random <- create_hyper_grid_domain(
#'  tuning_method = "random_search",
#'  ml_algorithm = "xgb",
#'  hyperparameter = c("min_child_weight", "max_depth", "subsample", "colsample_bytree",
#'                     "eta", "alpha", "gamma", "nrounds"),
#'  distribution_choice = c("constant", "uniform", "uniform", "uniform",
#'                          "uniform", "uniform", "constant", "uniform"),
#'  pars = list(3, c(min = 1L, max = 2L), c(min = 0.25, max = 0.50),
#'            c(min = 0.25, max = 0.50), c(min = 0.1, max = 0.2),
#'            c(min = 2, max = 5), 0, c(min = 200L, max = 500L))
#')
#'
#'
#'
#'
#' @export
create_hyper_grid_domain <- function(hyperparameter = NULL, tuning_method, ml_algorithm, grid = NULL, distribution_choice = NULL, pars = NULL, bounds = NULL) {

  #Create new obj
  new_hyper_grid_domain <-
    new("hyper_grid_domain",
        tuning_method = tuning_method,
        ml_algorithm = ml_algorithm,
        hyperparameter_list = list())

  if(!is.null(hyperparameter)){
  #Add hyperparameters
  new_hyper_grid_domain <- add_hyperparameter(new_hyper_grid_domain, hyperparameter = hyperparameter,
                                              grid = grid, distribution_choice = distribution_choice, pars = pars, bounds = bounds)
  }


  return(new_hyper_grid_domain)

}






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
#' @param object An object of class `keras_architecture_parameters`.
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

setMethod(
  "add_keras_layer",
  "keras_architecture_parameters",
  function(object, units, activation, batch_norm_option) {

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


#' @title Create ml_experiment Object
#' @description Constructs an ml_experiment object.
#'
#' @param target_fwd_name Character string indicating the target variable's forward name.
#' @param ml_algorithm Character string specifying the machine learning algorithm to be used ('glmnet', 'rf', 'xgb', 'nn').
#' @param hyperparameter_tuning_strategy An object of class hyperparameter_tuning_strategy, specifying the strategy for tuning hyperparameters.
#' @param custom_objective Character string specifying the custom objective function ('squared_error', 'pseudo_huber_error', 'absolute_error') or NULL.
#' @param keras_architecture_parameters List or NULL, providing parameters specific to keras-based neural networks.
#' @param quantile_tau Numeric value indicating the tau parameter used for quantile regression, between 0 and 1.
#' @param huber_delta Numeric value greater than 0, specifying the delta parameter for Huber loss function.
#'
#' @return An ml_experiment object.
#' @export
create_ml_experiment <- function(
    target_fwd_name,
    ml_algorithm = "ols",
    hyperparameter_tuning_strategy = NULL,
    custom_objective = "squared_error",
    keras_architecture_parameters = NULL,
    quantile_tau = 0.5,
    huber_delta = 1
) {
  # Create the ml_experiment object
  new("ml_experiment",
      target_fwd_name = target_fwd_name,
      ml_algorithm = ml_algorithm,
      hyperparameter_tuning_strategy = hyperparameter_tuning_strategy,
      custom_objective = custom_objective,
      keras_architecture_parameters = keras_architecture_parameters,
      quantile_tau = quantile_tau,
      huber_delta = huber_delta
  )
}





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



