#' Define the `meta_dataframe` S4 Class
#'
#' This class represents a metadata-enhanced data frame. It extends the functionality
#' of a standard data frame by including additional metadata slots. The class is designed
#' to ensure that the input data frame adheres to specific structural requirements, including
#' unique identifiers, valid date formats, and unique column names.
#'
#' @slot data A \code{data.frame} containing the actual data.
#' @slot workflow A \code{list} for storing metadata about the data manipulation workflow.
#' @slot signals A \code{character} vector containing the names of columns that represent signals.
#' @slot unique_dates A \code{numeric} value representing the count of unique dates in the data.
#' @slot unique_tickers A \code{numeric} value representing the count of unique tickers in the data.
#' @slot n_obs A \code{numeric} value representing the total number of observations in the data.
#'
#' @details
#' The \code{meta_dataframe} class ensures that the data frame is structured correctly with the required columns,
#' and includes metadata about the data. The \code{signals} slot holds the names of columns representing various signals.
#' The \code{unique_dates}, \code{unique_tickers}, and \code{n_obs} slots store the metadata related to the number of unique dates,
#' tickers, and total observations respectively.
#'
#' @examples
#' # Define a sample data frame
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
#' # Print the meta_dataframe object
#' print(meta_df)
#'
#' @export
setClass("meta_dataframe",
         slots = c(
           data = "data.frame",        # Slot for the data frame
           workflow = "list",          # Slot for storing metadata about the data manipulation workflow
           signals = "character",      # Slot for storing column names
           unique_dates = "numeric",   # Slot for storing count of unique dates
           unique_tickers = "numeric", # Slot for storing count of unique tickers
           n_obs = "numeric"           # Slot for storing total number of observations
         ))


#' Define the `hyper_grid_domain` S4 Class
#'
#' This class represents parameters for defining the hyperparameter domain based on which tuning will be performed.
#' It helps the user in correctly setting this object in the context of the `ml_walk_forward_validation` function.
#'
#' @slot tuning_method A character string specifying the tuning method that will be applied (e.g., "grid_search", "random_search" or "bayesian_optimization").
#' @slot ml_algorithm A character string specifying the machine-learning algorithm that will be used.
#' @slot hyperparameter_list A list with the hyperparameters relevant to the specified machine learning algorithm.
#'
#'
#' @export
setClass(
  "hyper_grid_domain",
  slots = list(
    tuning_method = "character",
    ml_algorithm = "character",
    hyperparameter_list = "list"
  ),
  validity = function(object) {
    valid_ml_algorithms <- c("glmnet", "rf", "xgb", "nn", "ols")

    if (!(object@ml_algorithm %in% valid_ml_algorithms)) {
      return("Invalid ml_algorithm. Choose from 'glmnet', 'rf', 'xgb', 'nn', or 'ols'.")
    }

    if (object@ml_algorithm == "ols" && length(object@hyperparameter_list) > 0) {
      return("ols algorithm does not have hyperparameters.")
    }

    valid_tuning_methods <- c("grid_search", "random_search", "bayesian_opt")
    if (!(object@tuning_method %in% valid_tuning_methods)) {
      return("Invalid tuning method. Choose from 'grid_search', 'random_search', or 'bayesian_opt'.")
    }

    if (nchar(object@ml_algorithm) == 0) {
      return("ml_algorithm cannot be an empty string.")
    }

    return(TRUE)
  }
)


#' Define the `refit_ml_model` S4 Class
#'
#' This class represents a refitted machine learning model. It encapsulates the algorithm used, hyperparameters,
#' feature data, target variable, and the fitted model object.
#'
#' @slot ml_algorithm A character string specifying the machine learning algorithm used (e.g., "ols", "glmnet", "rf", "xgb", "nn").
#' @slot best_hyperparameters The chosen hyperparameters relevant to the specified machine learning algorithm.
#' @slot model The fitted model object, which varies based on the algorithm used.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{refit()}}{Refits the model based on the specified algorithm and hyperparameters.}
#'   \item{\code{predict(new_features)}}{Generates predictions using the fitted model on new feature data.}
#' }
#'
#' @export
setClass(
  "refit_ml_model",
  slots = list(
    model = "ANY",
    model_class = "character",
    ml_algorithm = "character",
    best_hyperparameters = "ANY",
    custom_objective = "ANY",
    huber_delta = "numeric",
    keras_architecture_parameters = "ANY"
  )
)



#' S4 Class for Time Series Walk-Forward Validation Results of Machine-Learning Models
#'
#' This S4 class encapsulates the results and parameters from performing walk-forward
#' validation on time series data using machine learning algorithms. It includes
#' information about the model, data, tuning process, and performance metrics.
#'
#' @slot oos_prediction_list A list containing out-of-sample predictions indexed by testing dates.
#' @slot oos_error_list A list of out-of-sample errors indexed by testing dates.
#' @slot oos_y_list A list containing the actual target values for the out-of-sample period, indexed by testing dates.
#' @slot oos_testing_eval_metrics A list of evaluation metrics for the out-of-sample testing samples.
#' @slot final_model The final refitted machine learning model with best hyperparameters found after tuning. Possibly a object of refit_ml_model S4 class.
#' @slot chosen_eval_metric_validation A list of data.frames with the chosen evaluation metric calculated for the hyperparameter grid.
#' @slot best_hyperparameters A data frame containing the best hyperparameters selected during tuning for each rebalancing period.
#' @slot validation_eval_metrics_hyper_choice All evaluation metrics calculated for the set of best hyperparameters.
#' @slot metadata A list containing metadata about the walk-forward validation process. It includes:
#' \itemize{
#'   \item \strong{ml_algorithm}: A character string specifying the machine learning algorithm used.
#'   \item \strong{custom_objective}: A character string indicating the custom loss function applied (e.g., "squared_error").
#'   \item \strong{dates_covered}: A vector of dates representing the time period covered by the analysis.
#'   \item \strong{n_dates}: An integer indicating the total number of dates in the covered period.
#'   \item \strong{training_sample_size}: An integer representing the size of the training samples used.
#'   \item \strong{validation_sample_size}: An integer indicating the size of the validation samples used.
#'   \item \strong{testing_sample_size}: An integer indicating the size of the testing samples used.
#'   \item \strong{dates_testing_sample}: A vector of dates corresponding to the testing samples.
#'   \item \strong{first_rebalance_date}: A date indicating the first date when the model was rebalanced.
#'   \item \strong{rebalance_dates}: A vector of dates when the model was rebalanced.
#'   \item \strong{split_method}: A character string indicating the method used for splitting the data (e.g., "expanding" or "rolling").
#'   \item \strong{ids}: A vector of identifiers from the features data frame.
#'   \item \strong{nobs}: An integer representing the total number of observations in the features data frame.
#'   \item \strong{tickers}: A vector of unique stock tickers from the features data frame.
#'   \item \strong{n_stocks}: An integer indicating the number of unique stocks in the features data frame.
#'   \item \strong{target_fwd_name}: A character string naming the target variable for forward prediction.
#'   \item \strong{target_fwd}: A vector of forward target values.
#'   \item \strong{target_workflow}: A description of the workflow used for the target variable.
#'   \item \strong{target_object}: A character string capturing the name of the target data frame passed to the function.
#'   \item \strong{features}: A character vector of feature names extracted from the features data frame.
#'   \item \strong{features_workflow}: A description of the workflow used for the features.
#'   \item \strong{features_object}: A character string capturing the name of the features data frame passed to the function.
#'   \item \strong{tuning_method}: A character string indicating the method used for hyperparameter tuning (e.g., "grid_search").
#'   \item \strong{n_iter}: An integer specifying the number of iterations for tuning methods that require it.
#'   \item \strong{k_iter}: An integer indicating the number of times to sample the evaluation function during tuning.
#'   \item \strong{acq}: A character string specifying the acquisition function used in Bayesian optimization.
#'   \item \strong{init_points}: An integer indicating the number of initial random points for Bayesian optimization.
#'   \item \strong{hyper_grid_domain_list}: A list containing hyperparameter definitions for tuning.
#'   \item \strong{chosen_eval_metric}: A character string representing the evaluation metric chosen for optimization.
#'   \item \strong{huber_delta}: A numeric value indicating the boundary for the Huber loss function.
#'   \item \strong{quantile_tau}: A numeric value representing the target quantile for quantile loss.
#'   \item \strong{early_stop}: A criteria indicating if early stopping was used during training.
#'   \item \strong{keras_architecture_parameters}: A list containing parameters for the Keras model architecture.
#'   \item \strong{completion_time}: The system time when the validation process was completed.
#'   \item \strong{elapsed_time}: A numeric value representing the total time taken for the validation process.
#'   \item \strong{parallel}: A logical value indicating whether the process was run in parallel (TRUE or FALSE).
#'   \item \strong{call}: The matched call used to create the S4 object, capturing the function call context.
#' }
#'
#'
#' @return An S4 object of class `ml_wf_val_results` containing all the specified results and metadata.
#'
#'
#'@export
setClass(
  "ml_wf_val_results",
  slots = list(
    oos_prediction_list = "list",
    oos_error_list = "list",
    oos_y_list = "list",
    oos_testing_eval_metrics = "data.frame",
    final_model = "refit_ml_model",
    chosen_eval_metric_validation = "ANY",
    best_hyperparameters = "ANY",
    validation_eval_metrics_hyper_choice = "ANY",
    metadata = "list"
  )
)


#################################################################

##########################
#########Methods#########
##########################




##########################################################


#' Accessor Methods for meta_dataframe
#'
#' These methods are used to access components of a `meta_dataframe` object.
#'
#' @param object An object of class `meta_dataframe`.
#' @return The respective slot of the `meta_dataframe` object.
#' @name meta_dataframe_accessors
#' @export
setGeneric("get_data", function(object) standardGeneric("get_data"))

#' @export
setMethod("get_data", "meta_dataframe", function(object) {
  return(object@data)
})

#' @export
setGeneric("get_workflow", function(object) standardGeneric("get_workflow"))

#' @export
setMethod("get_workflow", "meta_dataframe", function(object) {
  return(object@workflow)
})

#' @export
setMethod(
  "as.data.frame", "meta_dataframe", function(x) {
    as.data.frame(x@data)
  }
)


#' Add a Hyperparameter to a `hyper_grid_domain`
#'
#' This method adds a new hyperparameter to an existing `hyper_grid_domain` object.
#'
#' @param hyper_grid_domain A `hyper_grid_domain` object to which the hyperparameter will be added.
#' @param new_hyperparameters A named list representing the hyperparameter to be added.
#'
#' @return A new `hyper_grid_domain` object with the updated hyperparameters.
#'
#' @examples
#' # Create an initial hyper_grid_domain object for grid-search
#' hyper_grid <- create_hyper_grid_domain(
#'   tuning_method = "grid_search",
#'   ml_algorithm = "glmnet"
#' )
#'
#' #Add hyperparameter for grid_search
#' hyper_grid <- add_hyperparameter(
#' hyper_grid_domain = hyper_grid,
#' new_hyperparameters = list(alpha = c(0.2, 0.5)) #Hyperparameters may be added one by one or all at once (eg.: list(alpha = c(0.2, 0.5), lambda.min.ratio = c(0.5, 0.2, 0.3)))
#' )
#'
#' random_search and xgb algorithm
#' hyper_grid <- create_hyper_grid_domain(
#'   tuning_method = "random_search",
#'   ml_algorithm = "xgb"
#' )
#'
#' hyper_grid <- add_hyperparameter(
#' hyper_grid_domain = hyper_grid,
#' new_hyperparameters =
#' list(min_child_weight = list(distribution_choice = "constant", value = 3),
#'      max_depth = list(distribution_choice = "uniform", pars = c(min = 1L, max = 2L)),
#'      subsample = list(distribution_choice = "uniform", pars = c(min = 0.25, max = 0.50)),
#'      colsample_bytree = list(distribution_choice = "uniform", pars = c(min = 0.25, max = 0.50)),
#'      eta = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.2)),
#'      alpha = list(distribution_choice = "uniform", pars = c(min = 2, max = 5)),
#'      gamma = list(distribution_choice = "constant", value = 0),
#'      nrounds = list(distribution_choice = "uniform", pars = c(min = 200L, max = 500L)))
#' )
#'
#' bayesian_opt and rf algorithm
#' #' hyper_grid <- create_hyper_grid_domain(
#'   tuning_method = "bayesian_opt",
#'   ml_algorithm = "rf"
#' )
#'
#'
#' hyper_grid <- add_hyperparameter(
#' hyper_grid_domain = hyper_grid,
#' new_hyperparameters =
#' list(mtry = c(0,1),
#'      num.trees = c(100L, 1000L),
#'      max.depth = c(2L, 8L),
#'      min.bucket = c(1, 5))
#' )
#'
#'
#' @export
setGeneric("add_hyperparameter", function(hyper_grid_domain, new_hyperparameters) standardGeneric("add_hyperparameter"))

#' @export
setMethod("add_hyperparameter", "hyper_grid_domain", function(hyper_grid_domain, new_hyperparameters) {

  # Ensure new_hyperparameters is a list
  if (!is.list(new_hyperparameters)) {
    stop("new_hyperparameters must be a list.")
  }

  # Get tuning_method and ml_algorithm
  tuning_method <- hyper_grid_domain@tuning_method
  ml_algorithm <- hyper_grid_domain@ml_algorithm

  # Validate new_hyperparameters based on tuning_method
  if (tuning_method == "grid_search") {
    if (!all(sapply(new_hyperparameters, function(x) is.numeric(x) && is.vector(x)))) {
      stop("For 'grid_search', new_hyperparameters must be a list of numeric vectors.")
    }
  } else if (tuning_method == "random_search") {
    for (name in names(new_hyperparameters)) {
      if (!is.list(new_hyperparameters[[name]]) || !all(c("distribution_choice") %in% names(new_hyperparameters[[name]]))) {
        stop("For 'random_search', each new_hyperparameters must be a list with 'distribution_choice'.")
      }

      distribution_choice <- new_hyperparameters[[name]]$distribution_choice

      if (is.null(distribution_choice) || !(distribution_choice %in% c("normal", "uniform", "lognormal", "constant"))) {
        stop("distribution_choice must be one of 'normal', 'uniform', 'lognormal', or 'constant'.")
      }

      if (distribution_choice == "constant") {
        if (is.null(new_hyperparameters[[name]]$value) || !is.numeric(new_hyperparameters[[name]]$value)) {
          stop("For 'constant', the second argument must be a numeric vector named 'value'.")
        }
      } else {
        if (!is.null(new_hyperparameters[[name]]$value)) {
          stop("For distributions other than 'constant', do not specify 'value'.")
        }
        pars <- new_hyperparameters[[name]]$pars
        if (is.null(pars) || !is.numeric(pars) || !is.vector(pars)) {
          stop("For distributions, the second argument must be a numeric vector named 'pars'.")
        }

        # Additional checks based on distribution_choice
        if (distribution_choice == "normal") {
          if (!all(names(pars) %in% c("mean", "sd"))) {
            stop("For 'normal', 'pars' must have names 'mean' and 'sd'.")
          }
        } else if (distribution_choice == "uniform") {
          if (!all(names(pars) %in% c("min", "max"))) {
            stop("For 'uniform', 'pars' must have names 'min' and 'max'.");
          }
        } else if (distribution_choice == "lognormal") {
          if (!all(names(pars) %in% c("meanlog", "sdlog"))) {
            stop("For 'lognormal', 'pars' must have names 'meanlog' and 'sdlog'.");
          }
        }
      }
    }
  } else if (tuning_method == "bayesian_opt") {
    if (any(sapply(new_hyperparameters, function(x) !is.numeric(x) || length(x) != 2))) {
      stop("For 'bayesian_opt', each new_hyperparameters must be a numeric vector of length 2 representing the bounds.")
    }
  } else {
    stop("Invalid tuning_method. Only 'grid_search', 'random_search', and 'bayesian_opt' are supported.")
  }


  # Check new_hyperparameters validity based on ml_algorithm
  new_hyperparameters_names <- names(new_hyperparameters)

  # GLMNET
  if (ml_algorithm == "glmnet" && !all(new_hyperparameters_names %in% c("alpha", "lambda.min.ratio"))) {
    stop("new_hyperparameters do not match ml_algorithm choice for 'glmnet'")
  }

  # RF
  if (ml_algorithm == "rf" && !all(new_hyperparameters_names %in% c("mtry", "num.trees", "max.depth", "min.bucket"))) {
    stop("new_hyperparameters do not match ml_algorithm choice for 'rf'")
  }

  # XGB
  if (ml_algorithm == "xgb" && !all(new_hyperparameters_names %in% c("min_child_weight", "max_depth", "subsample", "colsample_bytree",
                                                                "eta", "alpha", "gamma", "nrounds"))) {
    stop("new_hyperparameters do not match ml_algorithm choice for 'xgb'")
  }

  # NN
  if (ml_algorithm == "nn" && !all(new_hyperparameters_names %in% c("regularizer_l1", "regularizer_l2", "droprate", "lr",
                                                               "size_of_batch", "number_of_epochs"))) {
    stop("new_hyperparameters do not match ml_algorithm choice for 'nn'")
  }

  # Get current hyperparameter_list
  hyperparameter_list <- hyper_grid_domain@hyperparameter_list

  # Overwrite existing hyperparameters if they are duplicates
  for (name in names(new_hyperparameters)) {
    hyperparameter_list[[name]] <- new_hyperparameters[[name]]
  }

  # Create a new hyper_grid_domain object with the updated hyperparameter_list
  hyper_grid_domain_new <- new("hyper_grid_domain",
                               tuning_method = hyper_grid_domain@tuning_method,
                               ml_algorithm = hyper_grid_domain@ml_algorithm,
                               hyperparameter_list = hyperparameter_list
  )

  return(hyper_grid_domain_new) # Return the new object
})



#' Accessor Methods for refit_ml_model
#'
#' These methods are used to access components of a `refit_ml_model` object.
#'
#' @param object An object of class `refit_ml_model`.
#' @return The respective slot of the `refit_ml_model` object.
#' @name refit_ml_model_accessors
#' @export
setGeneric("get_ml_algorithm", function(object) standardGeneric("get_ml_algorithm"))

#' @export
setMethod("get_ml_algorithm", "refit_ml_model", function(object) {
  return(object@ml_algorithm)
})

#' @export
setGeneric("get_best_hyperparameters", function(object) standardGeneric("get_best_hyperparameters"))

#' @export
setMethod("get_best_hyperparameters", "refit_ml_model", function(object) {
  return(object@best_hyperparameters)
})

#' @export
setGeneric("get_model", function(object) standardGeneric("get_model"))

#' @export
setMethod("get_model", "refit_ml_model", function(object) {
  return(object@model)
})

#' Accessor Methods for ml_wf_val_results
#'
#' These methods are used to access various components of an `ml_wf_val_results` object.
#'
#' @param object An object of class `ml_wf_val_results`.
#' @return The respective slot of the `ml_wf_val_results` object.
#' @name ml_wf_val_results_accessors
#' @export
setGeneric("get_oos_prediction_list", function(object) standardGeneric("get_oos_prediction_list"))

#' @export
setMethod("get_oos_prediction_list", "ml_wf_val_results", function(object) {
  return(object@oos_prediction_list)
})

#' @export
setGeneric("get_oos_error_list", function(object) standardGeneric("get_oos_error_list"))

#' @export
setMethod("get_oos_error_list", "ml_wf_val_results", function(object) {
  return(object@oos_error_list)
})

#' @export
setGeneric("get_oos_y_list", function(object) standardGeneric("get_oos_y_list"))

#' @export
setMethod("get_oos_y_list", "ml_wf_val_results", function(object) {
  return(object@oos_y_list)
})

#' @export
setGeneric("get_oos_testing_eval_metrics", function(object) standardGeneric("get_oos_testing_eval_metrics"))

#' @export
setMethod("get_oos_testing_eval_metrics", "ml_wf_val_results", function(object) {
  return(object@oos_testing_eval_metrics)
})

#' @export
setGeneric("get_final_model", function(object) standardGeneric("get_final_model"))

#' @export
setMethod("get_final_model", "ml_wf_val_results", function(object) {
  return(object@final_model)
})

#' @export
setGeneric("get_chosen_eval_metric_validation", function(object) standardGeneric("get_chosen_eval_metric_validation"))

#' @export
setMethod("get_chosen_eval_metric_validation", "ml_wf_val_results", function(object) {
  return(object@chosen_eval_metric_validation)
})

#' @export
setMethod("get_best_hyperparameters", "ml_wf_val_results", function(object) {
  return(object@best_hyperparameters)
})

#' @export
setGeneric("get_validation_eval_metrics_hyper_choice", function(object) standardGeneric("get_validation_eval_metrics_hyper_choice"))

#' @export
setMethod("get_validation_eval_metrics_hyper_choice", "ml_wf_val_results", function(object) {
  return(object@validation_eval_metrics_hyper_choice)
})

#' @export
setGeneric("get_metadata", function(object) standardGeneric("get_metadata"))

#' @export
setMethod("get_metadata", "ml_wf_val_results", function(object) {
  return(object@metadata)
})


#' @export
setMethod("as.list", "ml_wf_val_results", function(x) {
  # Get the names of all slots
  slot_names <- slotNames(x)

  # Create a list to hold the extracted slots, ignoring NULL slots
  slot_list <- lapply(slot_names, function(slot) {
    value <- slot(x, slot)  # Extract the slot using the slot name
    if (!is.null(value)) {
      return(value)  # Return the value only if it's not NULL
    }
    return(NULL)  # Return NULL if the slot is NULL
  })

  # Filter out NULL entries
  non_null_indices <- which(!sapply(slot_list, is.null))
  slot_list <- slot_list[non_null_indices]

  # Set names for the list elements based on non-NULL slots
  names(slot_list) <- slot_names[non_null_indices]

  return(slot_list)
})



